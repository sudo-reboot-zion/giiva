;; ----------------------------------------------------------
;; Reputation Contract
;; ----------------------------------------------------------
;; Purpose:
;; - Tracks both creators and donors on the platform.
;; - Gives them a "reputation score" based on activity.
;; - Allows admin to manually adjust or reset scores.
;; ----------------------------------------------------------


;; ----------------------------------------------------------
;; DATA VARIABLES
;; ----------------------------------------------------------

(define-data-var reputation-enabled bool true)
;; This switch turns the reputation system on/off.
;; true = reputation system is active.
;; false = temporarily disabled (no updates allowed).


;; ----------------------------------------------------------
;; CONSTANTS
;; ----------------------------------------------------------

(define-constant contract-owner tx-sender)
;; The wallet that deployed this contract becomes the admin.
;; Only this address can adjust or reset reputations manually.


;; ----------------------------------------------------------
;; ERROR CODES
;; ----------------------------------------------------------

(define-constant err-unauthorized  (err u200))     ;; Caller not allowed to do this action.
(define-constant err-invalid-score (err u201))     ;; The reputation score must be between 0 => 10000.
(define-constant err-user-not-found (err u202))    ;; No record found for user.
(define-constant err-invalid-campaign (err u203))    ;; No record found for user.
(define-constant err-too-many-campaigns (err u204))
(define-constant err-invalid-donor (err u205))
(define-constant err-invalid-amount (err u206))
(define-constant err-already-backed (err u207))
(define-constant err-donation-overflow (err u208))
(define-constant err-cannot-adjust-self (err u209))
(define-constant err-reputation-disabled (err u210))




;; ----------------------------------------------------------
;; MAP: USER REPUTATION
;; ----------------------------------------------------------

;; Track which campaigns a donor has backed (prevent double-counting)
(define-map donor-campaign-tracking
  { donor: principal, campaign-id: uint }
  { backed: bool }
)


(define-map user-reputation
    principal ;; key: wallet address
    {
        total-campaigns-created: uint, ;; How many campaigns this user created.
        successful-campaigns: uint,    ;; How many succeeded.
        failed-campaigns: uint,        ;; How many failed.
        total-donated: uint,           ;; Total STX donated by the user.
        campaigns-backed: uint,        ;; Number of different campaigns supported.
        reputation-score: uint,        ;; The user's main score (0 => 10000).
        last-updated: uint             ;; Block height of last update.
    }
)


;; ----------------------------------------------------------
;; PRIVATE FUNCTION: get-or-create-reputation
;; ----------------------------------------------------------
;; This helper ensures every user has a reputation record.
;; If user doesn't exist in the map, it creates a default one.
;; ----------------------------------------------------------

(define-private (get-or-create-reputation (user principal))
    (default-to
        {
            total-campaigns-created: u0,
            successful-campaigns: u0,
            failed-campaigns: u0,
            campaigns-backed: u0,
            total-donated: u0,
            reputation-score: u5000, ;; Everyone starts at neutral score 5000 (50%).
            last-updated: stacks-block-height
        }
        (map-get? user-reputation user) ;; Returns existing record or default one.
    )
)


;; ----------------------------------------------------------
;; PRIVATE FUNCTION: min-value
;; ----------------------------------------------------------
;; Simple helper that returns the smaller of two numbers.
;; Used when applying score caps (like max 10000 points).
;; ----------------------------------------------------------

(define-private (min-value (a uint) (b uint))
  (if (<= a b) a b)
)


;; ----------------------------------------------------------
;; PUBLIC FUNCTION: update-creator-reputation
;; ----------------------------------------------------------
;; Called when a campaign ends to reward/punish a creator.
;; - Adds +1 total campaign.
;; - If successful, adds to success count.
;; - If failed, adds to failed count.
;; - Then recalculates reputation score.
;; ----------------------------------------------------------

(define-public (update-creator-reputation 
  (creator principal)
  (campaign-id uint)
  (campaign-successful bool))
  
  (let
    (
      ;; Get reputation record
      (current-rep (get-or-create-reputation creator))
      
      ;; Extract current values for validation
      (current-total (get total-campaigns-created current-rep))
      (current-successful (get successful-campaigns current-rep))
      (current-failed (get failed-campaigns current-rep))
    )
    
    ;; *** AUTHORIZATION: Only campaign factory can call ***
    (asserts! (is-eq contract-caller .campaign-factory) err-unauthorized)
    
    ;; *** VALIDATION: Ensure creator is not contract or burn address ***
    (asserts! (not (is-eq creator (as-contract tx-sender))) err-unauthorized)
    (asserts! (not (is-eq creator 'SP000000000000000000002Q6VF78)) err-unauthorized)
    
    ;; *** VALIDATION: Ensure campaign-id is valid ***
    (asserts! (and (> campaign-id u0) (< campaign-id u1000000)) err-invalid-campaign)
    
    ;; *** VALIDATION: Check for reasonable current values (detect corruption) ***
    (asserts! (<= current-total u100000) err-invalid-score)
    (asserts! (<= current-successful current-total) err-invalid-score)
    (asserts! (<= current-failed current-total) err-invalid-score)
    (asserts! (is-eq current-total (+ current-successful current-failed)) err-invalid-score)
    
    ;; *** OVERFLOW PROTECTION: Check before incrementing ***
    (asserts! (< current-total u100000) err-too-many-campaigns)
    
    (let
      (
        ;; Calculate new values
        (new-total (+ current-total u1))
        (new-successful (if campaign-successful 
                          (+ current-successful u1)
                          current-successful))
        (new-failed (if campaign-successful
                      current-failed
                      (+ current-failed u1)))
      )
      
      ;; *** VALIDATION: Verify arithmetic correctness ***
      (asserts! (is-eq new-total (+ new-successful new-failed)) err-invalid-score)
      
      ;; Calculate success rate with overflow protection
      (let
        (
          ;; Check multiplication won't overflow (new-successful * 10000)
          (success-rate-calc (if (and (> new-total u0) (<= new-successful u42949672))
                               (/ (* new-successful u10000) new-total)
                               u5000))
          
          ;; Activity bonus with bounds
          (activity-bonus (if (>= new-total u10)
                            u1000
                            (* new-total u100)))
          
          (calculated-score (+ success-rate-calc activity-bonus))
          
          ;; Cap to max score
          (new-score (if (<= calculated-score u10000)
                       calculated-score
                       u10000))
        )
        
        ;; *** VALIDATION: Ensure new score is reasonable ***
        (asserts! (<= new-score u10000) err-invalid-score)
        
        ;; Log the update for transparency
        (print {
          event: "reputation-updated",
          creator: creator,
          campaign-id: campaign-id,
          campaign-successful: campaign-successful,
          old-score: (get reputation-score current-rep),
          new-score: new-score,
          total-campaigns: new-total,
          at-height: stacks-block-height
        })
        
        ;; Update reputation record
        (map-set user-reputation creator {
          total-campaigns-created: new-total,
          successful-campaigns: new-successful,
          failed-campaigns: new-failed,
          total-donated: (get total-donated current-rep),
          campaigns-backed: (get campaigns-backed current-rep),
          reputation-score: new-score,
          last-updated: stacks-block-height
        })
        
        (ok new-score)
      )
    )
  )
)


;; ----------------------------------------------------------
;; PUBLIC FUNCTION: update-donor-reputation
;; ----------------------------------------------------------
;; Called whenever someone donates to a campaign.
;; The more they donate or back different campaigns, 
;; the higher their reputation.
;; ----------------------------------------------------------

(define-public (update-donor-reputation
    (donor principal)
    (amount uint)
    (campaign-id uint)) 

    (let 
        (
            (current-rep (get-or-create-reputation donor))
            (current-total-donated (get total-donated current-rep))
            (current-campaigns-backed (get campaigns-backed current-rep))
            (already-backed (map-get? donor-campaign-tracking { donor: donor, campaign-id: campaign-id }))
        )

        ;; *** AUTHORIZATION: Only campaign contract can call this ***
        (asserts! (is-eq contract-caller .campaign-factory) err-unauthorized)
        
        ;; *** VALIDATION: Ensure donor is valid ***
        (asserts! (not (is-eq donor (as-contract tx-sender))) err-invalid-donor)
        (asserts! (not (is-eq donor 'SP000000000000000000002Q6VF78)) err-invalid-donor)
        
        ;; *** VALIDATION: Ensure amount is reasonable ***
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (<= amount u100000000000000) err-invalid-score)
        
        ;; *** VALIDATION: Ensure campaign-id is valid ***
        (asserts! (and (> campaign-id u0) (< campaign-id u1000000)) err-invalid-campaign)
        
        ;; *** IDEMPOTENCY: Prevent double-counting same campaign ***
        (asserts! (is-none already-backed) err-already-backed)
        
        ;; *** VALIDATION: Check current state ***
        (asserts! (<= current-total-donated u1000000000000000) err-invalid-score)
        (asserts! (<= current-campaigns-backed u100000) err-invalid-score)
        
        ;; *** OVERFLOW PROTECTION ***
        (asserts! (<= (+ current-total-donated amount) u1000000000000000) err-donation-overflow)
        (asserts! (< current-campaigns-backed u100000) err-too-many-campaigns)

        (let
            (
                (new-total-donated (+ current-total-donated amount))
                (new-campaigns-backed (+ current-campaigns-backed u1))
                
                ;; Calculate donation score (1 point per 10M micro-STX, max 5000)
                (donation-score-calc (/ new-total-donated u10000000))
                (donation-score (if (<= donation-score-calc u5000)
                                  donation-score-calc
                                  u5000))
                
                ;; Calculate diversity bonus (+100 per campaign, max 3000)
                (diversity-bonus-calc (* new-campaigns-backed u100))
                (diversity-bonus (if (<= diversity-bonus-calc u3000)
                                   diversity-bonus-calc
                                   u3000))
                
                ;; Combined score capped at 10000
                (calculated-score (+ donation-score diversity-bonus))
                (new-score (if (<= calculated-score u10000)
                             calculated-score
                             u10000))
            )
            
            ;; *** VALIDATION: Final score check ***
            (asserts! (<= new-score u10000) err-invalid-score)
            
            ;; Mark campaign as backed
            (map-set donor-campaign-tracking 
              { donor: donor, campaign-id: campaign-id }
              { backed: true })
            
            ;; Log the update
            (print {
              event: "donor-reputation-updated",
              donor: donor,
              campaign-id: campaign-id,
              amount: amount,
              old-score: (get reputation-score current-rep),
              new-score: new-score,
              at-height: stacks-block-height
            })
            
            ;; *** SAVE THE UPDATED REPUTATION ***
            (map-set user-reputation donor {
              total-campaigns-created: (get total-campaigns-created current-rep),
              successful-campaigns: (get successful-campaigns current-rep),
              failed-campaigns: (get failed-campaigns current-rep),
              total-donated: new-total-donated,
              campaigns-backed: new-campaigns-backed,
              reputation-score: new-score,
              last-updated: stacks-block-height
            })
            
            (ok new-score)
        )
    )
)


;; ----------------------------------------------------------
;; READ-ONLY: get-user-reputation
;; ----------------------------------------------------------
;; Returns full reputation record of any user.
;; If user doesn't exist, returns `none`.
;; ----------------------------------------------------------

(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation user)
)


;; ----------------------------------------------------------
;; READ-ONLY: get-reputation-score
;; ----------------------------------------------------------
;; Returns only the numeric reputation score.
;; Creates a default record if user doesnt exist.
;; ----------------------------------------------------------

(define-read-only (get-reputation-score (user principal))
  (ok (get reputation-score (get-or-create-reputation user)))
)


;; ----------------------------------------------------------
;; READ-ONLY: get-creator-stats
;; ----------------------------------------------------------
;; Gives basic summary for creators (used on their profile page):
;; - total campaigns
;; - successful campaigns
;; - failed campaigns
;; - success rate %
;; ----------------------------------------------------------

(define-read-only (get-creator-stats (creator principal))
  (match (map-get? user-reputation creator)
    rep (ok {
      total-campaigns: (get total-campaigns-created rep),
      successful: (get successful-campaigns rep),
      failed: (get failed-campaigns rep),
      success-rate: (if (> (get total-campaigns-created rep) u0)
                        (/ (* (get successful-campaigns rep) u100)
                           (get total-campaigns-created rep))
                        u0)
    })
    ;; if creator not found, return default stats
    (ok {
      total-campaigns: u0,
      successful: u0,
      failed: u0,
      success-rate: u0
    })
  )
)


;; ----------------------------------------------------------
;; READ-ONLY: get-donor-stats
;; ----------------------------------------------------------
;; Gives summary for donors:
;; - Total amount donated
;; - Number of campaigns backed
;; ----------------------------------------------------------

(define-read-only (get-donor-stats (donor principal))
  (match (map-get? user-reputation donor)
    rep (ok {
      total-donated: (get total-donated rep),
      campaigns-backed: (get campaigns-backed rep)
    })
    (ok {
      total-donated: u0,
      campaigns-backed: u0
    })
  )
)


;; ----------------------------------------------------------
;; READ-ONLY: is-trusted-user
;; ----------------------------------------------------------
;; Returns true if user's score >= 7000 (70%)
;; Trusted users have proven reliability.
;; ----------------------------------------------------------

(define-read-only (is-trusted-user (user principal))
  (>= (get reputation-score (get-or-create-reputation user)) u7000)
)


;; ----------------------------------------------------------
;; READ-ONLY: get-reputation-tier
;; ----------------------------------------------------------
;; Categorizes users into tiers/badges:
;; elite    - 90%+
;; trusted  - 75 => 89 %
;; good     - 50 => 74%
;; new      - 25 => 49%
;; unproven - 0 => 24%
;; ----------------------------------------------------------

(define-read-only (get-reputation-tier (user principal))
  (let
    ((score (get reputation-score (get-or-create-reputation user))))
    (if (>= score u9000)
      (ok "elite")
      (if (>= score u7500)
        (ok "trusted")
        (if (>= score u5000)
          (ok "good")
          (if (>= score u2500)
            (ok "new")
            (ok "unproven")
          )
        )
      )
    )
  )
)


;; ----------------------------------------------------------
;; ADMIN FUNCTION: toggle-reputation-system
;; ----------------------------------------------------------
;; Enables or disables the reputation system.
;; Only contract owner can call this.
;; ----------------------------------------------------------

(define-public (toggle-reputation-system (enabled bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set reputation-enabled enabled)
    (ok enabled)
  )
)


;; ----------------------------------------------------------
;; READ-ONLY: is-reputation-enabled
;; ----------------------------------------------------------
;; Checks if reputation system is currently turned on.
;; ----------------------------------------------------------

(define-read-only (is-reputation-enabled)
  (ok (var-get reputation-enabled))
)


;; ----------------------------------------------------------
;; ADMIN FUNCTION: adjust-reputation
;; ----------------------------------------------------------
;; Manually adjust a users score.
;; Only admin can do this (used for disputes or errors).
;; ----------------------------------------------------------

(define-public (adjust-reputation 
  (user principal) 
  (new-score uint))
  (let
    (
      (current-rep (get-or-create-reputation user))
      (old-score (get reputation-score current-rep))
    )
    ;; *** AUTHORIZATION ***
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    
    ;; *** VALIDATION: User must be valid ***
    (asserts! (not (is-eq user (as-contract tx-sender))) err-unauthorized)
    (asserts! (not (is-eq user contract-owner)) err-cannot-adjust-self)
    
    ;; *** VALIDATION: New score in valid range (0-10000) ***
    (asserts! (<= new-score u10000) err-invalid-score)
    
    ;; *** VALIDATION: Score must change ***
    (asserts! (not (is-eq new-score old-score)) err-invalid-score)
    
    ;; *** VALIDATION: System must be enabled ***
    (asserts! (var-get reputation-enabled) err-reputation-disabled)

    ;; Log the change (optional but recommended)
    (print {
      event: "reputation-adjusted",
      user: user,
      old-score: old-score,
      new-score: new-score,
      at-height: stacks-block-height
    })

    ;; Update only the score and last-updated field
    (map-set user-reputation user
      (merge current-rep { 
        reputation-score: new-score,
        last-updated: stacks-block-height
      })
    )
    
    (ok new-score)
  )
)


;; ----------------------------------------------------------
;; ADMIN FUNCTION: reset-user-reputation
;; ----------------------------------------------------------
;; Completely resets a users record to default (5000 score).
;; Useful if user dispute is resolved or theyre starting over.
;; ----------------------------------------------------------

(define-public (reset-user-reputation (user principal))
  (begin
    ;; *** AUTHORIZATION ***
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    
    ;; *** VALIDATION: User must be valid ***
    (asserts! (not (is-eq user (as-contract tx-sender))) err-unauthorized)
    
    ;; *** VALIDATION: System must be enabled ***
    (asserts! (var-get reputation-enabled) err-reputation-disabled)
    
    ;; Log the reset
    (print {
      event: "reputation-reset",
      user: user,
      at-height: stacks-block-height
    })
    
    (map-set user-reputation user {
      total-campaigns-created: u0,
      successful-campaigns: u0,
      failed-campaigns: u0,
      total-donated: u0,
      campaigns-backed: u0,
      reputation-score: u5000,
      last-updated: stacks-block-height
    })
    (ok true)
  )
)
