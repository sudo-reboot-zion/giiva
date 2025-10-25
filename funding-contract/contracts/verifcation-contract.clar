;; Validator/Verification Contract
;; Manages validators, community vouches, campaign verification, and stake withdrawals

;; contract-owner
(define-constant contract-owner tx-sender) ;; The account that deploys the contract is the owner/admin

;; Cooldown period before withdrawal (blocks)
(define-constant withdrawal-cooldown u14400) ;; Number of blocks to wait (~100 days) after requesting withdrawal

;; Staking amounts
(define-constant validator-stake-amount u1000000000) ;; Amount a validator must stake (micro-STX units)
(define-constant voucher-stake-amount u100000)      ;; Amount a community member stakes to vouch for a campaign
(define-constant min-vouches-required u3)           ;; Minimum number of vouches to reach community level

;; Error codes (unique errors to return when checks fail)
(define-constant err-unauthorized (err u300))         ;; Caller not allowed to perform this action
(define-constant err-insufficient-stake (err u301))  ;; Not enough STX provided for staking
(define-constant err-already-validator (err u302))   ;; Caller already registered as validator
(define-constant err-not-validator (err u303))       ;; Caller is not a validator
(define-constant err-already-vouched (err u304))     ;; Caller already vouched for this campaign
(define-constant err-campaign-not-found (err u305))  ;; Campaign verification entry not found
(define-constant err-cannot-vouch-own-campaign (err u306)) ;; Caller cannot vouch their own campaign
(define-constant err-validator-not-found (err u307)) ;; Specified validator doesn't exist
(define-constant err-invalid-verification (err u308)) ;; Verification action invalid (e.g., duplicate)
(define-constant err-cooldown-active (err u309))     ;; Withdrawal cooldown still active
(define-constant err-no-stake-to-withdraw (err u310)) ;; No stake recorded to withdraw
(define-constant err-insufficient-funds (err u311))
(define-constant err-too-many-verifications (err u312))
(define-constant err-too-many-vouches (err u313))
(define-constant err-invalid-config (err u314))

;; Data Variables - global on-chain counters
(define-data-var validator-count uint u0) ;; Number of registered validators
(define-data-var total-staked uint u0)    ;; Total STX currently staked in the system

;; Validator registry - tracks trusted validators and their stats
(define-map validators
    principal ;; key: validator principal (address)
    {
        stake-amount: uint,         ;; How much STX they staked
        campaigns-verified: uint,   ;; How many campaigns they've verified
        fraudulent-approvals: uint,  ;; How many times they've been flagged for fraud
        is-active: bool,            ;; Whether they're currently active as validator
        specialization: (string-ascii 20), ;; Their specialty (e.g., "medical")
        joined-at: uint             ;; Block height when they joined
    }
)

;; Vouches - community members staking small amounts to vouch for campaigns
(define-map vouches
    {campaign-id: uint, voucher: principal} ;; key: campaign + voucher address
    {
        stake-amount: uint,   ;; How much the voucher staked
        vouched-at: uint      ;; Block height when they vouched
    }
)

;; Campaign verification status - combined community + validator signals
(define-map campaign-verification
    uint ;; key: campaign ID
    {
        vouch-count: uint,            ;; number of community vouches
        total-vouched-stake: uint,    ;; sum of vouchers' stakes
        validator-approved: bool,     ;; whether a validator approved it
        validator-address: (optional principal), ;; which validator approved (if any)
        verification-level: (string-ascii 10),  ;; "none" | "community" | "validator" | "elite"
        verified-at: uint             ;; block height when verified
    }
)

;; Track which campaigns each validator has verified (prevents duplicate fee claims)
(define-map validator-verifications
    {validator: principal, campaign-id: uint} ;; key: validator + campaign id
    {verified-at: uint, fee-earned: uint}     ;; when verified and fee recorded
)

;; -------------------------
;; PUBLIC: register-validator
;; Stake STX and register as a validator
;; -------------------------
(define-public (register-validator (specialization (string-ascii 20)))
  (let
    (
      (existing-validator (map-get? validators tx-sender)) ;; check if caller is already a validator
    )
    ;; ensure the caller isn't already registered
    (asserts! (is-none existing-validator) err-already-validator)

    ;; validate specialization is one of allowed categories
    (asserts! (or
      (is-eq specialization "medical")
      (is-eq specialization "education")
      (is-eq specialization "donation")
      (is-eq specialization "orphan")
      (is-eq specialization "general")
    ) err-invalid-verification)

    ;; transfer required stake from caller to contract (locks stake on contract)
    (try! (stx-transfer? validator-stake-amount tx-sender (as-contract tx-sender)))

    ;; register validator in the map with initial stats
    (map-set validators tx-sender {
      stake-amount: validator-stake-amount,
      campaigns-verified: u0,
      fraudulent-approvals: u0,
      is-active: true,
      specialization: specialization,
      joined-at: stacks-block-height
    })

    ;; increment global counters
    (var-set validator-count (+ (var-get validator-count) u1))
    (var-set total-staked (+ (var-get total-staked) validator-stake-amount))

    (ok true)
  )
)

;; -------------------------
;; PUBLIC: deactivate-validator
;; Mark validator inactive but keep their stake locked
;; -------------------------
(define-public (deactivate-validator)
  (let
    (
      (validator-data (unwrap! (map-get? validators tx-sender) err-not-validator)) ;; must be registered
    )
    ;; ensure they're currently active
    (asserts! (get is-active validator-data) err-not-validator)

    ;; set is-active to false (keeps stake locked)
    (map-set validators tx-sender
      (merge validator-data { is-active: false })
    )

    (ok true)
  )
)

;; -------------------------
;; PUBLIC: reactivate-validator
;; Reactivate a previously deactivated validator
;; -------------------------
(define-public (reactivate-validator)
  (let
    (
      (validator-data (unwrap! (map-get? validators tx-sender) err-not-validator))
    )
    ;; ensure currently inactive; prevents reactivating an active validator
    (asserts! (not (get is-active validator-data)) err-already-validator)

    ;; set is-active to true
    (map-set validators tx-sender
      (merge validator-data { is-active: true })
    )

    (ok true)
  )
)

;; -------------------------
;; PUBLIC: vouch-for-campaign
;; Community member stakes a small amount to vouch for a campaign
;; -------------------------
(define-public (vouch-for-campaign (campaign-id uint))
  (let
    (
      (existing-vouch (map-get? vouches { campaign-id: campaign-id, voucher: tx-sender }))
      (verification-data-opt (map-get? campaign-verification campaign-id))
      (verification-data (default-to
        {
          vouch-count: u0,
          total-vouched-stake: u0,
          validator-approved: false,
          validator-address: none,
          verification-level: "none",
          verified-at: u0
        }
        verification-data-opt))
    )
    ;; *** VALIDATION: Campaign must exist (or be in valid state) ***
    ;; You might want to verify campaign exists in the factory contract
    (asserts! (< campaign-id u1000000) err-campaign-not-found) ;; Reasonable upper bound
    
    ;; *** VALIDATION: Ensure caller hasn't already vouched ***
    (asserts! (is-none existing-vouch) err-already-vouched)

    ;; *** VALIDATION: Verify voucher stake amount is reasonable ***
    (asserts! (and (> voucher-stake-amount u0) 
                   (<= voucher-stake-amount u100000000000)) err-insufficient-stake)

    ;; *** VALIDATION: Check current vouch count for overflow protection ***
    (let
      (
        (current-vouch-count (get vouch-count verification-data))
        (current-total-stake (get total-vouched-stake verification-data))
        (current-global-stake (var-get total-staked))
      )
      ;; Prevent overflow on vouch count
      (asserts! (< current-vouch-count u1000000) err-too-many-vouches)
      
      ;; Prevent overflow on stake amounts
      (asserts! (<= (+ current-total-stake voucher-stake-amount) u340282366920938463463374607431768211455) err-insufficient-stake)
      (asserts! (<= (+ current-global-stake voucher-stake-amount) u340282366920938463463374607431768211455) err-insufficient-stake)

      ;; Transfer voucher stake to contract (locks it)
      (try! (stx-transfer? voucher-stake-amount tx-sender (as-contract tx-sender)))

      ;; Record this vouch in vouches map
      (map-set vouches
        { campaign-id: campaign-id, voucher: tx-sender }
        {
          stake-amount: voucher-stake-amount,
          vouched-at: stacks-block-height
        }
      )

      ;; Update campaign verification counters
      (let
        (
          (new-vouch-count (+ current-vouch-count u1))
          (new-total-stake (+ current-total-stake voucher-stake-amount))
          (new-level (if (>= new-vouch-count min-vouches-required)
                       "community"
                       (get verification-level verification-data)))
        )
        ;; Validate min-vouches-required is reasonable
        (asserts! (<= min-vouches-required u10000) err-invalid-config)
        
        (map-set campaign-verification campaign-id
          (merge verification-data {
            vouch-count: new-vouch-count,
            total-vouched-stake: new-total-stake,
            verification-level: new-level,
            verified-at: (if (is-eq new-level "community") 
                           stacks-block-height 
                           (get verified-at verification-data))
          })
        )
      )

      ;; Add voucher stake to global total staked
      (var-set total-staked (+ current-global-stake voucher-stake-amount))

      (ok true)
    )
  )
)
;; -------------------------
;; PUBLIC: remove-vouch
;; Voucher withdraws their stake (before campaign ends)
;; -------------------------
(define-public (remove-vouch (campaign-id uint))
  (let
    (
      (vouch-data (unwrap! (map-get? vouches { campaign-id: campaign-id, voucher: tx-sender }) err-unauthorized)) ;; must have vouched
      (verification-data (unwrap! (map-get? campaign-verification campaign-id) err-campaign-not-found))
      (refund-amount (get stake-amount vouch-data))
    )


     ;; Validate refund amount is within reasonable bounds
    (asserts! (and (> refund-amount u0) (<= refund-amount u1000000000000)) err-insufficient-funds)
    
    ;; Ensure sufficient balance exists
    (asserts! (<= refund-amount (get total-vouched-stake verification-data)) err-insufficient-funds)
    
    ;; refund the voucher stake to the voucher
    (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))

    ;; delete the vouch record
    (map-delete vouches { campaign-id: campaign-id, voucher: tx-sender })

    ;; update campaign verification counts
    (map-set campaign-verification campaign-id
      (merge verification-data {
        vouch-count: (- (get vouch-count verification-data) u1),
        total-vouched-stake: (- (get total-vouched-stake verification-data) refund-amount)
      })
    )

    ;; reduce global total staked
    (var-set total-staked (- (var-get total-staked) refund-amount))

    (ok refund-amount)
  )
)

;; -------------------------
;; PUBLIC: validator-approve-campaign
;; A registered validator approves a campaign (adds higher-tier verification)
;; -------------------------
(define-public (validator-approve-campaign 
  (campaign-id uint)
  (verification-fee uint))
  (let
    (
      (validator-data (unwrap! (map-get? validators tx-sender) err-not-validator))
      (verification-data (unwrap! (map-get? campaign-verification campaign-id) err-campaign-not-found))
      (already-verified (map-get? validator-verifications { validator: tx-sender, campaign-id: campaign-id }))
    )
    ;; Validate validator is active
    (asserts! (get is-active validator-data) err-not-validator)

    ;; Validate campaign state
    (asserts! (is-none already-verified) err-already-vouched)
    (asserts! (not (get validator-approved verification-data)) err-invalid-verification)

    ;; *** VALIDATE THE VERIFICATION FEE ***
    ;; Option 1: Validate against expected range
    (asserts! (and (>= verification-fee u1000000) (<= verification-fee u10000000000)) err-insufficient-stake)
    
    ;; Option 2: Use a constant fee instead of accepting parameter
    ;; (define-constant VALIDATOR_FEE u5000000)
    ;; Then use VALIDATOR_FEE instead of verification-fee parameter

    ;; Validate validator stats don't overflow
    (asserts! (< (get campaigns-verified validator-data) u1000000) err-too-many-verifications)

    ;; Record verification event
    (map-set validator-verifications
      { validator: tx-sender, campaign-id: campaign-id }
      {
        verified-at: stacks-block-height,
        fee-earned: verification-fee
      }
    )

    ;; Update validator stats
    (map-set validators tx-sender
      (merge validator-data {
        campaigns-verified: (+ (get campaigns-verified validator-data) u1)
      })
    )

    ;; Validate vouch count before using it
    (let
      (
        (vouch-count (get vouch-count verification-data))
      )
      ;; Validate vouch-count is reasonable
      (asserts! (<= vouch-count u1000000) err-insufficient-funds)
      
      (let
        (
          (new-level (if (>= vouch-count min-vouches-required)
                       "elite"
                       "validator"))
        )

        ;; Update campaign verification
        (map-set campaign-verification campaign-id
          (merge verification-data {
            validator-approved: true,
            validator-address: (some tx-sender),
            verification-level: new-level,
            verified-at: stacks-block-height
          })
        )
      )
    )

    (ok true)
  )
)

;; -------------------------
;; PUBLIC: report-fraud
;; Contract owner penalizes a validator who approved a fraudulent campaign
;; -------------------------
(define-public (report-fraud 
  (campaign-id uint)
  (validator-address principal))
  (let
    (
      (validator-data (unwrap! (map-get? validators validator-address) err-validator-not-found))
      (verification-record (unwrap! 
        (map-get? validator-verifications { validator: validator-address, campaign-id: campaign-id })
        err-invalid-verification))
      (current-stake (get stake-amount validator-data))
      (penalty-amount (/ current-stake u10))
    )
    ;; Authorization check
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    
    ;; Validate validator data before applying penalties
    (asserts! (get is-active validator-data) err-validator-not-found)
    (asserts! (> current-stake u0) err-insufficient-stake)
    (asserts! (>= current-stake penalty-amount) err-insufficient-stake)
    
    ;; Validate penalty amount is reasonable
    (asserts! (and (> penalty-amount u0) (<= penalty-amount u100000000000)) err-insufficient-funds)

    ;; increment fraudulent count, reduce stake by penalty, and deactivate validator
    (map-set validators validator-address
      (merge validator-data {
        fraudulent-approvals: (+ (get fraudulent-approvals validator-data) u1),
        stake-amount: (- current-stake penalty-amount),
        is-active: false
      })
    )

    ;; Transfer penalty from contract to owner (FIX THE TRANSFER!)
    (try! (as-contract (stx-transfer? penalty-amount (as-contract tx-sender) contract-owner)))

    (ok penalty-amount)
  )
)

;; -------------------------
;; READ-ONLY: get-validator
;; Returns validator data
;; -------------------------
(define-read-only (get-validator (validator principal))
  (map-get? validators validator) ;; returns validator record or none
)

;; -------------------------
;; READ-ONLY: is-active-validator
;; Returns true if validator exists and is active
;; -------------------------
(define-read-only (is-active-validator (validator principal))
  (match (map-get? validators validator)
    validator-data (get is-active validator-data)
    false
  )
)

;; -------------------------
;; READ-ONLY: get-verification-status
;; Returns verification summary for a campaign
;; -------------------------
(define-read-only (get-verification-status (campaign-id uint))
  (map-get? campaign-verification campaign-id)
)

;; -------------------------
;; READ-ONLY: get-verification-level
;; Returns string representing verification level or "none"
;; -------------------------
(define-read-only (get-verification-level (campaign-id uint))
  (match (map-get? campaign-verification campaign-id)
    verification-data (ok (get verification-level verification-data))
    (ok "none")
  )
)

;; -------------------------
;; READ-ONLY: is-campaign-verified
;; Returns true if campaign has any non-"none" verification level
;; -------------------------
(define-read-only (is-campaign-verified (campaign-id uint))
  (match (map-get? campaign-verification campaign-id)
    verification-data (not (is-eq (get verification-level verification-data) "none"))
    false
  )
)

;; -------------------------
;; READ-ONLY: get-vouch
;; Returns vouch record for a voucher on a campaign
;; -------------------------
(define-read-only (get-vouch (campaign-id uint) (voucher principal))
  (map-get? vouches { campaign-id: campaign-id, voucher: voucher })
)

;; -------------------------
;; READ-ONLY: get-validator-count
;; Returns number of registered validators
;; -------------------------
(define-read-only (get-validator-count)
  (ok (var-get validator-count))
)

;; -------------------------
;; READ-ONLY: get-total-staked
;; Returns total STX staked by validators + vouchers
;; -------------------------
(define-read-only (get-total-staked)
  (ok (var-get total-staked))
)

;; -------------------------
;; READ-ONLY: get-validator-record
;; Returns a summary of validator performance and success rate
;; -------------------------
(define-read-only (get-validator-record (validator principal))
  (match (map-get? validators validator)
    validator-data (ok {
      verified: (get campaigns-verified validator-data), ;; campaigns validated
      fraudulent: (get fraudulent-approvals validator-data), ;; fraud flags
      success-rate: (if (> (get campaigns-verified validator-data) u0)
                      (/ (* (- (get campaigns-verified validator-data) 
                               (get fraudulent-approvals validator-data)) u100)
                         (get campaigns-verified validator-data))
                      u100) ;; if zero verified, give 100% by default (you can change logic)
    })
    err-validator-not-found
  )
)

;; -------------------------
;; WITHDRAWAL: tracking map
;; -------------------------
(define-map withdrawal-requests
  principal  ;; key: validator address
  {
    requested-at: uint, ;; block height when withdrawal requested
    amount: uint        ;; amount requested (usually stake)
  }
)

;; -------------------------
;; PUBLIC: request-withdrawal
;; Deactivated validator starts cooldown to withdraw stake
;; -------------------------
(define-public (request-withdrawal)
  (let
    (
      (validator-data (unwrap! (map-get? validators tx-sender) err-not-validator))
      (stake-amount (get stake-amount validator-data))
    )
    ;; must have stake to request withdrawal
    (asserts! (> stake-amount u0) err-no-stake-to-withdraw)

    ;; must be deactivated first (cannot withdraw while active)
    (asserts! (not (get is-active validator-data)) err-unauthorized)

    ;; record withdrawal request and timestamp
    (map-set withdrawal-requests tx-sender {
      requested-at: stacks-block-height,
      amount: stake-amount
    })

    (ok stacks-block-height)
  )
)

;; -------------------------
;; PUBLIC: complete-withdrawal
;; After cooldown passes, validator receives their stake back
;; -------------------------
(define-public (complete-withdrawal)
  (let
    (
      (withdrawal-data (unwrap! (map-get? withdrawal-requests tx-sender) err-unauthorized))
      (validator-data (unwrap! (map-get? validators tx-sender) err-not-validator))
      (requested-at (get requested-at withdrawal-data))
      (withdrawal-amount (get amount withdrawal-data))
      (cooldown-end (+ requested-at withdrawal-cooldown))
    )
    ;; ensure cooldown has expired
    (asserts! (>= stacks-block-height cooldown-end) err-cooldown-active)

    ;; transfer stake back to validator
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender tx-sender)))

    ;; remove validator record from registry
    (map-delete validators tx-sender)

    ;; remove withdrawal request
    (map-delete withdrawal-requests tx-sender)

    ;; update counters to reflect withdrawal
    (var-set validator-count (- (var-get validator-count) u1))
    (var-set total-staked (- (var-get total-staked) withdrawal-amount))

    (ok withdrawal-amount)
  )
)

;; -------------------------
;; PUBLIC: cancel-withdrawal
;; Cancels a pending withdrawal request
;; -------------------------
(define-public (cancel-withdrawal)
  (let
    (
      (withdrawal-data (unwrap! (map-get? withdrawal-requests tx-sender) err-unauthorized))
    )
    ;; delete the request
    (map-delete withdrawal-requests tx-sender)

    (ok true)
  )
)

;; -------------------------
;; READ-ONLY: get-withdrawal-status
;; Returns details about a validator's withdrawal request and cooldown
;; -------------------------
(define-read-only (get-withdrawal-status (validator principal))
  (match (map-get? withdrawal-requests validator)
    request-data 
      (let
        (
          (cooldown-end (+ (get requested-at request-data) withdrawal-cooldown))
          (blocks-remaining (if (> cooldown-end stacks-block-height)
                              (- cooldown-end stacks-block-height)
                              u0))
        )
        (ok {
          requested-at: (get requested-at request-data),
          amount: (get amount request-data),
          cooldown-end: cooldown-end,
          blocks-remaining: blocks-remaining,
          can-withdraw: (>= stacks-block-height cooldown-end)
        })
      )
    err-unauthorized
  )
)
