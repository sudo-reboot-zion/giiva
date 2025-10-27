;; ---------------------------------------------------------------
;; Crowdfunding Factory Smart Contract
;; ---------------------------------------------------------------
;; Purpose:
;;   This contract allows anyone to create crowdfunding campaigns,
;;   contribute to them, withdraw funds if successful, or get refunds
;;   if the campaign fails.
;;
;; ---------------------------------------------------------------


;; ===============================================================
;; ================ GLOBAL DATA VARIABLES ========================
;; ===============================================================

;; The percentage of each campaign's raised funds taken by the platform (2.5%)
(define-data-var platform-fee uint u250)

;; Keeps track of the total number of campaigns created
(define-data-var campaign-counter uint u0)

;; Address where platform fees will be sent (set initially to contract deployer)
(define-data-var platform-wallet principal tx-sender)




;; ===============================================================
;; ===================== CONSTANTS ===============================
;; ===============================================================

;; Minimum and maximum allowed campaign durations
(define-constant min-campaign-duration u1440)    ;; 10 days (assuming ~144 blocks/day)
(define-constant max-campaign-duration u14400)   ;; 100 days

;; Minimum and maximum allowed goal amounts
(define-constant min-goal-amount u10000000)      ;; 1 STX minimum
(define-constant max-goal-amount u100000000)     ;; 100,000 STX maximum

;; Small creation fee to prevent spam campaigns
(define-constant campaign-creation-fee u1000000) ;; 0.1 STX



;; ===============================================================
;; ===================== ERROR CODES =============================
;; ===============================================================
;; Each error returns a unique number for clarity when debugging
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-invalid-duration (err u102))
(define-constant err-campaign-not-found (err u103))
(define-constant err-invalid-ipfs-hash (err u104))  
(define-constant err-invalid-category (err u105))  
(define-constant err-campaign-ended (err u106))  
(define-constant err-campaign-not-active (err u107))
(define-constant err-goal-not-reached (err u108))
(define-constant err-campaign-not-ended (err u109))
(define-constant err-already-claimed (err u110))
(define-constant err-no-contribution (err u111))
(define-constant err-campaign-mass-fee (err u112))
(define-constant err-invalid-wallet (err u113))
(define-constant err-wallet-unchanged (err u114))
(define-constant err-insufficient-funds (err u115))


;; ===============================================================
;; ======================= DATA MAPS =============================
;; ===============================================================

;; ---- Campaigns Map ----
;; Each campaign has a unique ID (uint)
;; The value contains all details of that campaign.
(define-map campaigns
    uint
    {
        creator: principal,          ;; Campaign creator's address
        category: (string-ascii 20), ;; Campaign type (education, medical, etc.)
        goal-amount: uint,           ;; Target amount in micro-STX
        raised-amount: uint,         ;; Total funds raised so far
        deadline: uint,              ;; Block height when campaign ends
        ipfs-hash: (string-ascii 64), ;; Off-chain metadata reference (image, desc, etc.)
        status: (string-ascii 10),   ;; "active", "successful", or "failed"
        created-at: uint             ;; Block height when created
    }
)


;; ---- Contributions Map ----
;; Tracks each contributor's total contribution per campaign.
(define-map contributions
    {campaign-id: uint, contributor: principal}
    {amount: uint, timestamp: uint}
)



;; ===============================================================
;; ===================== HELPER FUNCTION =========================
;; ===============================================================

;; Validates whether a category is allowed.
(define-private (is-valid-category (category (string-ascii 20)))
    (or
        (is-eq category "education")
        (is-eq category "medical")
        (is-eq category "farming")
        (is-eq category "orphan")
    )
)



;; ===============================================================
;; ===================== PUBLIC FUNCTIONS ========================
;; ===============================================================

;; ---------------------------------------------------------------
;; (1) CREATE CAMPAIGN
;; ---------------------------------------------------------------
(define-public (create-campaign
    (category (string-ascii 20))
    (goal-amount uint)
    (duration uint)
    (ipfs-hash (string-ascii 64))
)
    (let 
        (
            ;; Create new campaign ID
            (campaign-id (+ (var-get campaign-counter) u1))

            ;; Calculate deadline by adding duration to current block height
            (deadline (+ stacks-block-height duration))
        )

        ;; --- VALIDATIONS ---

        ;; Ensure category is valid
        (asserts! (is-valid-category category) err-invalid-category)

        ;; Ensure goal amount is within allowed range
        (asserts! (and (>= goal-amount min-goal-amount)
                       (<= goal-amount max-goal-amount)) err-invalid-amount)

        ;; Ensure campaign duration is valid
        (asserts! (and (>= duration min-campaign-duration)
                       (<= duration max-campaign-duration)) err-invalid-duration)

        ;; Ensure IPFS hash is not empty
        (asserts! (> (len ipfs-hash) u0) err-invalid-ipfs-hash)

        ;; Charge small fee to prevent spam campaigns
        (try! (stx-transfer? campaign-creation-fee tx-sender (var-get platform-wallet)))

        ;; --- SAVE NEW CAMPAIGN DATA ---
        (map-set campaigns campaign-id {
            creator: tx-sender,
            category: category,
            goal-amount: goal-amount,
            raised-amount: u0,
            deadline: deadline,
            ipfs-hash: ipfs-hash,
            status: "active",
            created-at: stacks-block-height
        })

        ;; Increment campaign counter
        (var-set campaign-counter campaign-id)

        ;; Return new campaign ID
        (ok campaign-id)
    )
)



;; ---------------------------------------------------------------
;; (2) CONTRIBUTE TO A CAMPAIGN
;; ---------------------------------------------------------------
(define-public (contribute (campaign-id uint) (amount uint))
    (let 
        (
            ;; Get campaign details or throw error if not found
            (campaign (unwrap! (map-get? campaigns campaign-id) err-campaign-not-found))

            ;; Current amount raised
            (current-raised (get raised-amount campaign))

            ;; New total raised
            (new-raised (+ current-raised amount))

            ;; Get users  previous contribution, or 0 if none
            (existing-contribution (default-to u0 
                (get amount (map-get? contributions {campaign-id: campaign-id, contributor: tx-sender}))))

            ;; New total contribution by this user
            (total-contribution (+ existing-contribution amount))
        )

        ;; --- VALIDATIONS ---
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (is-eq (get status campaign) "active") err-campaign-not-active)
        (asserts! (< stacks-block-height (get deadline campaign)) err-campaign-ended)

        ;; Transfer STX from user to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

        ;; Update contributors record
        (map-set contributions
            {campaign-id: campaign-id, contributor: tx-sender}
            {amount: total-contribution, timestamp: stacks-block-height}
        )

        ;; Update campaign raised amount
        (map-set campaigns campaign-id
            (merge campaign {raised-amount: new-raised})
        )

        (ok true)
    )
)



;; ---------------------------------------------------------------
;; (3) WITHDRAW FUNDS (For successful campaigns)
;; ---------------------------------------------------------------
(define-public (withdraw-funds (campaign-id uint))
    (let 
        (
            (campaign (unwrap! (map-get? campaigns campaign-id) err-campaign-not-found))
            (creator (get creator campaign))
            (raised (get raised-amount campaign))
            (goal (get goal-amount campaign))
            (deadline (get deadline campaign))
            (status (get status campaign))

            ;; Calculate fee and creator payout
            (fee-amount (/ (* raised (var-get platform-fee)) u10000))
            (creator-amount (- raised fee-amount))
        )

        ;; --- VALIDATIONS ---
        
        ;; *** VALIDATE CREATOR ADDRESS ***
        (asserts! (not (is-eq creator (as-contract tx-sender))) err-invalid-wallet)
        (asserts! (not (is-eq creator 'SP000000000000000000002Q6VF78)) err-invalid-wallet)
        
        (asserts! (is-eq tx-sender creator) err-unauthorized)
        (asserts! (>= stacks-block-height deadline) err-campaign-not-ended)
        (asserts! (>= raised goal) err-goal-not-reached)
        (asserts! (is-eq status "active") err-already-claimed)
        
        ;; *** VALIDATE AMOUNTS ***
        (asserts! (> creator-amount u0) err-insufficient-funds)
        (asserts! (<= creator-amount u100000000000000) err-insufficient-funds)

        ;; Transfer STX to campaign creator
        (try! (as-contract (stx-transfer? creator-amount tx-sender creator)))

        ;; Transfer fee to platform wallet
        (try! (as-contract (stx-transfer? fee-amount tx-sender (var-get platform-wallet))))

        ;; Mark campaign as successful
        (map-set campaigns campaign-id
            (merge campaign {status: "successful"})
        )

        (ok creator-amount)
    )
)



;; ---------------------------------------------------------------
;; (4) CLAIM REFUND (For failed campaigns)
;; ---------------------------------------------------------------
(define-public (claim-refund (campaign-id uint))
    (let 
        (
            (campaign (unwrap! (map-get? campaigns campaign-id) err-campaign-not-found))
            (contributor tx-sender)  ;; Capture contributor BEFORE as-contract
            (contribution-data (unwrap! (map-get? contributions {campaign-id: campaign-id, contributor: contributor}) err-no-contribution))
            (refund-amount (get amount contribution-data))
            (raised (get raised-amount campaign))
            (goal (get goal-amount campaign))
            (deadline (get deadline campaign))
        )

        ;; *** VALIDATION: Campaign ID must be valid ***
        (asserts! (and (> campaign-id u0) (< campaign-id u1000000)) err-campaign-not-found)

        ;; *** VALIDATION: Campaign must have ended ***
        (asserts! (>= stacks-block-height deadline) err-campaign-not-ended)
        
        ;; *** VALIDATION: Goal not reached ***
        (asserts! (< raised goal) err-goal-not-reached)
        
        ;; *** VALIDATION: Refund amount must be reasonable ***
        (asserts! (> refund-amount u0) err-no-contribution)
        (asserts! (<= refund-amount u100000000000000) err-insufficient-funds)

        ;; Refund STX to contributor 
        (try! (as-contract (stx-transfer? refund-amount tx-sender contributor)))

        ;; Remove record to prevent double refunds
        (map-delete contributions {campaign-id: campaign-id, contributor: contributor})

        ;; Mark campaign as failed (only once)
        (if (is-eq (get status campaign) "active")
            (map-set campaigns campaign-id
                (merge campaign {status: "failed"}))
            true
        )

        (ok refund-amount)
    )
)



;; ===============================================================
;; ===================== READ-ONLY FUNCTIONS =====================
;; ===============================================================

;; Get campaign details
(define-read-only (get-campaign (campaign-id uint))
    (map-get? campaigns campaign-id)
)

;; Get contribution details for a specific contributor
(define-read-only (get-contribution (campaign-id uint) (contributor principal))
    (map-get? contributions {campaign-id: campaign-id, contributor: contributor})
)

;; Get total number of campaigns created
(define-read-only (get-campaign-count)
    (ok (var-get campaign-counter))
)

;; Check if campaign is currently active
(define-read-only (is-campaign-active (campaign-id uint))
    (match (map-get? campaigns campaign-id)
        campaign (and
                    (is-eq (get status campaign) "active")
                    (< stacks-block-height (get deadline campaign)))
        false
    )
)

;; Check if campaign has reached its goal
(define-read-only (is-goal-reached (campaign-id uint))
    (match (map-get? campaigns campaign-id)
        campaign (>= (get raised-amount campaign) (get goal-amount campaign))
        false
    )
)

;; Get campaign progress percentage x100
(define-read-only (get-campaign-progress (campaign-id uint))
    (match (map-get? campaigns campaign-id)
        campaign (ok (/ (* (get raised-amount campaign) u10000) (get goal-amount campaign)))
        err-campaign-not-found
    )
)

;; Get platform fee percentage
(define-read-only (get-platform-fee)
    (ok (var-get platform-fee))
)



;; ===============================================================
;; ======================== ADMIN FUNCTIONS ======================
;; ===============================================================

;; Update platform fee (only admin)
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender (var-get platform-wallet)) err-unauthorized)
        (asserts! (<= new-fee u1000) err-campaign-mass-fee)
        (var-set platform-fee new-fee)
        (ok true)
    )
)

;; Update platform wallet (only admin)
(define-public (set-platform-wallet (new-wallet principal))
    (begin
        ;; *** AUTHORIZATION CHECK ***
        (asserts! (is-eq tx-sender (var-get platform-wallet)) err-unauthorized)
        
        ;; *** VALIDATION: Ensure new wallet is not the contract itself ***
        (asserts! (not (is-eq new-wallet (as-contract tx-sender))) err-invalid-wallet)
        
        ;; *** VALIDATION: Ensure new wallet is not the zero/burn address ***
        ;; Note: Clarity doesn't have a "zero address" but you can define one
        (asserts! (not (is-eq new-wallet 'SP000000000000000000002Q6VF78)) err-invalid-wallet)
        
        ;; *** VALIDATION: Ensure new wallet is different from current ***
        (asserts! (not (is-eq new-wallet (var-get platform-wallet))) err-wallet-unchanged)
        
        
        (var-set platform-wallet new-wallet)
        (ok true)
    )
)
