;; ---------------------------------------------------------------
;; Milestone Contract
;; Manages milestone-based fund releases for campaigns step by step
;; ---------------------------------------------------------------

;; -------------------------
;; CONSTANTS
;; -------------------------
(define-constant contract-owner tx-sender) ;; The address that deploys this contract is stored as the owner/admin
(define-constant max-milestones u10)       ;; Max number of milestones allowed per campaign (10)

;; -------------------------
;; ERROR CODES
;; -------------------------
(define-constant err-unauthorized (err u400))        ;; Caller is not permitted to perform this action
(define-constant err-invalid-milestone (err u401))   ;; Milestone order or data is invalid (e.g., previous milestone not completed)
(define-constant err-milestone-not-found (err u402)) ;; Requested milestone does not exist
(define-constant err-already-completed (err u403))   ;; Milestone already completed; cannot be re-used
(define-constant err-proof-required (err u404))      ;; Proof hash is missing when required
(define-constant err-not-approved (err u405))        ;; Trying to release funds before approval
(define-constant err-invalid-percentage (err u406))  ;; Milestone percentages do not add up to 100%
(define-constant err-campaign-not-found (err u407))  ;; Campaign id not found in storage
(define-constant err-milestones-already-set (err u408)) ;; Milestones already set for this campaign
(define-constant err-too-many-milestones (err u409))    ;; Number of milestones exceeds max-milestones
(define-constant err-insufficient-funds (err u410))     ;; Contract or campaign does not have enough funds to release
(define-constant err-already-approved (err u411))       ;; Approver has already approved this milestone

;; -------------------------
;; DATA VARIABLES
;; -------------------------
(define-data-var milestone-approval-threshold uint u2) ;; Default approvals required to mark a milestone approved (2)

;; -------------------------
;; DATA MAPS
;; -------------------------
(define-map campaign-milestones
  uint ;; key: campaign-id (uint)
  {
    total-milestones: uint,      ;; total milestones defined for this campaign
    completed-milestones: uint,  ;; how many milestones have been completed so far
    creator: principal,          ;; campaign creator/owner
    total-funds: uint,           ;; total STX allocated to be released across milestones
    is-active: bool              ;; whether milestones are still active (true) or closed (false)
  }
)
;; campaign-milestones[campaign-id] => summary info for the campaign's milestones

(define-map milestone-record
  { campaign-id: uint, milestone-id: uint } ;; composite key: campaign + milestone index
  {
    title: (string-ascii 50),            ;; human-readable milestone title
    description-hash: (string-ascii 64), ;; IPFS (or similar) hash pointing to milestone details
    percentage: uint,                    ;; percentage of total-funds represented by this milestone (out of 10000)
    amount: uint,                        ;; computed STX amount for this milestone (derived from percentage and total-funds)
    status: (string-ascii 10),           ;; "pending", "submitted", "approved", "rejected", "completed"
    proof-hash: (optional (string-ascii 64)), ;; optional IPFS hash linking to proof of work
    submitted-at: uint,                  ;; block height when proof was submitted
    approved-at: uint,                   ;; block height when approved
    released-amount: uint                ;; amount of STX released for this milestone (0 if not released)
  }
)
;; milestone-record[{campaign-id, milestone-id}] => detailed info for each milestone

(define-map milestone-approvals
  { campaign-id: uint, milestone-id: uint, approver: principal } ;; key: which approver approved which milestone
  {
    approved: bool,                       ;; true if approver approved
    approved-at: uint,                    ;; block height when approval happened
    comments-hash: (optional (string-ascii 64)) ;; optional IPFS link to comments or evidence
  }
)
;; milestone-approvals[{campaign-id, milestone-id, approver}] => individual approver record

(define-map milestone-approval-count
  { campaign-id: uint, milestone-id: uint } ;; key: campaign+milestone
  uint ;; number of approvals received so far for this milestone
)
;; milestone-approval-count[{campaign-id, milestone-id}] => simple approval tally

;; -------------------------
;; HELPER FUNCTIONS
;; -------------------------

(define-private (calculate-total-percentage 
  (milestones (list 10 { 
    title: (string-ascii 50), 
    description-hash: (string-ascii 64), 
    percentage: uint 
  })))
  ;; Sum the percentage fields of the list of milestones to confirm they add to 10000 (100.00%)
  (fold add-percentage milestones u0)
)
;; calculate-total-percentage(milestones) => returns sum of milestone.percentages

(define-private (add-percentage 
  (milestone { 
    title: (string-ascii 50), 
    description-hash: (string-ascii 64), 
    percentage: uint 
  })
  (total uint))
  ;; Helper used by fold: add this milestone's percentage to running total
  (+ total (get percentage milestone))
)
;; add-percentage(milestone, total) => total + milestone.percentage

(define-private (store-single-milestone
  (milestone { 
    title: (string-ascii 50), 
    description-hash: (string-ascii 64), 
    percentage: uint 
  })
  (state { campaign-id: uint, total-funds: uint, counter: uint }))
  ;; Store one milestone record and initialize its approval count.
  ;; 'state' carries campaign-id, total-funds and the current counter (milestone-id to use).
  (let
    (
      ;; Calculate STX amount for this milestone:
      ;; amount = total-funds * percentage / 10000
      (milestone-amount (/ (* (get total-funds state) (get percentage milestone)) u10000))
    )
    ;; Save milestone record into milestone-record map
    (map-set milestone-record
      { campaign-id: (get campaign-id state), milestone-id: (get counter state) }
      {
        title: (get title milestone),                        ;; store provided title
        description-hash: (get description-hash milestone),  ;; store provided description hash
        percentage: (get percentage milestone),              ;; store percentage
        amount: milestone-amount,                            ;; store calculated amount
        status: "pending",                                   ;; initial status is "pending"
        proof-hash: none,                                    ;; no proof yet
        submitted-at: u0,                                    ;; 0 until submission
        approved-at: u0,                                     ;; 0 until approval
        released-amount: u0                                  ;; 0 until funds released
      }
    )
    ;; Initialize approval count for this milestone to 0
    (map-set milestone-approval-count
      { campaign-id: (get campaign-id state), milestone-id: (get counter state) }
      u0
    )
    ;; Return updated state with counter incremented by 1 for the next milestone
    { 
      campaign-id: (get campaign-id state), 
      total-funds: (get total-funds state), 
      counter: (+ (get counter state) u1) 
    }
  )
)
;; store-single-milestone returns updated state for fold to carry forward

(define-private (store-milestones
  (campaign-id uint)
  (milestone-list (list 10 { 
    title: (string-ascii 50), 
    description-hash: (string-ascii 64), 
    percentage: uint 
  }))
  (total-funds uint)
  (current-id uint))
  ;; Loop over milestone-list and call store-single-milestone for each item.
  ;; Uses fold to carry a state object containing campaign-id, total-funds and the running counter.
  (begin
    (fold store-single-milestone 
      milestone-list 
      { campaign-id: campaign-id, total-funds: total-funds, counter: current-id })
    true ;; return true to indicate completion
  )
)

;; -------------------------
;; PUBLIC FUNCTIONS
;; -------------------------

(define-public (create-campaign-milestones
  (campaign-id uint)
  (milestone-data (list 10 { 
    title: (string-ascii 50), 
    description-hash: (string-ascii 64), 
    percentage: uint 
  }))
  (total-funds uint))
  
  (let
    (
      (milestone-count (len milestone-data))
      (existing-milestones (map-get? campaign-milestones campaign-id))
    )
    ;; *** VALIDATION: Campaign ID must be valid ***
    (asserts! (and (> campaign-id u0) (< campaign-id u1000000)) err-campaign-not-found)
    
    ;; *** VALIDATION: Cannot redefine milestones ***
    (asserts! (is-none existing-milestones) err-milestones-already-set)
    
    ;; *** VALIDATION: Milestone count must be valid ***
    (asserts! (and (> milestone-count u0) (<= milestone-count max-milestones)) err-too-many-milestones)
    
    ;; *** VALIDATION: Total funds must be reasonable ***
    (asserts! (> total-funds u0) err-insufficient-funds)
    (asserts! (<= total-funds u100000000000000) err-insufficient-funds)
    
    ;; *** VALIDATION: Percentages sum to exactly 10000 ***
    (asserts! (is-eq (calculate-total-percentage milestone-data) u10000) err-invalid-percentage)
    
    ;; Save campaign-level milestone summary
    (map-set campaign-milestones campaign-id {
      total-milestones: milestone-count,
      completed-milestones: u0,
      creator: tx-sender,
      total-funds: total-funds,
      is-active: true
    })
    
    ;; Store each individual milestone record
    (begin
      (store-milestones campaign-id milestone-data total-funds u1)
      (ok milestone-count)
    )
  )
)

(define-public (submit-milestone-proof
  (campaign-id uint)
  (milestone-id uint)
  (proof-hash (string-ascii 64)))
  ;; Called by the campaign creator to submit proof (e.g., IPFS hash) that they completed work for a milestone.
  (let
    (
      (campaign-milestones-data (unwrap! (map-get? campaign-milestones campaign-id) err-campaign-not-found)) ;; ensure campaign exists
      (milestone-data (unwrap! (map-get? milestone-record { campaign-id: campaign-id, milestone-id: milestone-id }) err-milestone-not-found)) ;; fetch milestone
    )
    ;; Only the campaign's creator can submit proof
    (asserts! (is-eq tx-sender (get creator campaign-milestones-data)) err-unauthorized)
    ;; Milestone must be in "pending" state to submit proof (not already completed/submitted)
    (asserts! (is-eq (get status milestone-data) "pending") err-already-completed)
    ;; Proof hash must be present (non-empty)
    (asserts! (> (len proof-hash) u0) err-proof-required)
    
    ;; If this is not the first milestone (milestone-id > 1), ensure the previous milestone is completed
    (if (> milestone-id u1)
      (let
        (
          (previous-milestone (unwrap! 
            (map-get? milestone-record { campaign-id: campaign-id, milestone-id: (- milestone-id u1) }) 
            err-milestone-not-found))
        )
        ;; previous milestone must have status "completed" before submitting this one
        (asserts! (is-eq (get status previous-milestone) "completed") err-invalid-milestone)
        true
      )
      ;; else (first milestone), nothing to check
      true
    )
    
    ;; Update milestone record: mark as submitted and store proof hash and timestamp
    (map-set milestone-record
      { campaign-id: campaign-id, milestone-id: milestone-id }
      (merge milestone-data {
        status: "submitted",                         ;; now awaiting approvers
        proof-hash: (some proof-hash),               ;; store proof as optional some(...)
        submitted-at: stacks-block-height           ;; record block height when submitted
      })
    )
    (ok true) ;; indicate submission succeeded
  )
)

(define-public (approve-milestone
  (campaign-id uint)
  (milestone-id uint)
  (comments-hash (optional (string-ascii 64))))
  
  (let
    (
      (milestone-data (unwrap! (map-get? milestone-record { campaign-id: campaign-id, milestone-id: milestone-id }) err-milestone-not-found))
      (existing-approval (map-get? milestone-approvals { campaign-id: campaign-id, milestone-id: milestone-id, approver: tx-sender }))
      (current-approval-count (default-to u0 (map-get? milestone-approval-count { campaign-id: campaign-id, milestone-id: milestone-id })))
    )
    ;; *** VALIDATION: Campaign ID must be valid ***
    (asserts! (and (> campaign-id u0) (< campaign-id u1000000)) err-campaign-not-found)
    
    ;; *** VALIDATION: Milestone ID must be valid ***
    (asserts! (and (> milestone-id u0) (<= milestone-id max-milestones)) err-milestone-not-found)
    
    ;; *** VALIDATION: Comments hash length if provided ***
    (asserts! 
      (match comments-hash
        hash (and (> (len hash) u0) (<= (len hash) u64))
        true)
      err-proof-required)
    
    ;; *** VALIDATION: Only submitted milestones can be approved ***
    (asserts! (is-eq (get status milestone-data) "submitted") err-invalid-milestone)
    
    ;; *** VALIDATION: Approver cannot approve more than once ***
    (asserts! (is-none existing-approval) err-already-approved)
    
    ;; *** VALIDATION: Approval count must be reasonable ***
    (asserts! (< current-approval-count u1000) err-invalid-milestone)
    
    ;; Record this approver's approval
    (map-set milestone-approvals
      { campaign-id: campaign-id, milestone-id: milestone-id, approver: tx-sender }
      {
        approved: true,
        approved-at: stacks-block-height,
        comments-hash: comments-hash
      }
    )
    
    ;; Increase approval count
    (let
      (
        (new-approval-count (+ current-approval-count u1))
      )
      (map-set milestone-approval-count
        { campaign-id: campaign-id, milestone-id: milestone-id }
        new-approval-count
      )
      
      ;; If threshold reached, mark as approved
      (if (>= new-approval-count (var-get milestone-approval-threshold))
        (map-set milestone-record
          { campaign-id: campaign-id, milestone-id: milestone-id }
          (merge milestone-data {
            status: "approved",
            approved-at: stacks-block-height
          })
        )
        true
      )
    )
    (ok true)
  )
)

(define-public (release-milestone-funds
  (campaign-id uint)
  (milestone-id uint))
  
  (let
    (
      (campaign-milestones-data (unwrap! (map-get? campaign-milestones campaign-id) err-campaign-not-found))
      (milestone-data (unwrap! (map-get? milestone-record { campaign-id: campaign-id, milestone-id: milestone-id }) err-milestone-not-found))
      (creator (get creator campaign-milestones-data))
      (release-amount (get amount milestone-data))
      (completed-count (get completed-milestones campaign-milestones-data))
    )
    ;; *** VALIDATION: Campaign ID must be valid ***
    (asserts! (and (> campaign-id u0) (< campaign-id u1000000)) err-campaign-not-found)
    
    ;; *** VALIDATION: Milestone ID must be valid ***
    (asserts! (and (> milestone-id u0) (<= milestone-id max-milestones)) err-milestone-not-found)
    
    ;; *** VALIDATION: Only creator can release funds ***
    (asserts! (is-eq tx-sender creator) err-unauthorized)
    
    ;; *** VALIDATION: Release amount must be reasonable ***
    (asserts! (> release-amount u0) err-insufficient-funds)
    (asserts! (<= release-amount u100000000000000) err-insufficient-funds)
    
    ;; *** VALIDATION: Milestone must be approved ***
    (asserts! (is-eq (get status milestone-data) "approved") err-not-approved)
    
    ;; *** VALIDATION: Funds not already released ***
    (asserts! (is-eq (get released-amount milestone-data) u0) err-already-completed)
    
    ;; *** VALIDATION: Completed count must be reasonable ***
    (asserts! (< completed-count max-milestones) err-invalid-milestone)
    
    ;; Transfer funds from contract to creator
    (try! (as-contract (stx-transfer? release-amount tx-sender creator)))
    
    ;; Update milestone record
    (map-set milestone-record
      { campaign-id: campaign-id, milestone-id: milestone-id }
      (merge milestone-data {
        status: "completed",
        released-amount: release-amount
      })
    )
    
    ;; Update campaign completed counter
    (map-set campaign-milestones campaign-id
      (merge campaign-milestones-data {
        completed-milestones: (+ completed-count u1)
      })
    )
    
    (ok release-amount)
  )
)

(define-public (reject-milestone
  (campaign-id uint)
  (milestone-id uint))
  
  (let
    (
      (milestone-data (unwrap! (map-get? milestone-record { campaign-id: campaign-id, milestone-id: milestone-id }) err-milestone-not-found))
    )
    ;; *** VALIDATION: Campaign ID must be valid ***
    (asserts! (and (> campaign-id u0) (< campaign-id u1000000)) err-campaign-not-found)
    
    ;; *** VALIDATION: Milestone ID must be valid ***
    (asserts! (and (> milestone-id u0) (<= milestone-id max-milestones)) err-milestone-not-found)
    
    ;; *** VALIDATION: Only contract owner can reject ***
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    
    ;; *** VALIDATION: Milestone must be submitted to reject ***
    (asserts! (is-eq (get status milestone-data) "submitted") err-invalid-milestone)
    
    ;; Set milestone status to rejected
    (map-set milestone-record
      { campaign-id: campaign-id, milestone-id: milestone-id }
      (merge milestone-data {
        status: "rejected",
        proof-hash: none
      })
    )
    (ok true)
  )
)

(define-public (resubmit-milestone
  (campaign-id uint)
  (milestone-id uint)
  (new-proof-hash (string-ascii 64)))
  
  (let
    (
      (campaign-milestones-data (unwrap! (map-get? campaign-milestones campaign-id) err-campaign-not-found))
      (milestone-data (unwrap! (map-get? milestone-record { campaign-id: campaign-id, milestone-id: milestone-id }) err-milestone-not-found))
    )
    ;; *** VALIDATION: Campaign ID must be valid ***
    (asserts! (and (> campaign-id u0) (< campaign-id u1000000)) err-campaign-not-found)
    
    ;; *** VALIDATION: Milestone ID must be valid ***
    (asserts! (and (> milestone-id u0) (<= milestone-id max-milestones)) err-milestone-not-found)
    
    ;; *** VALIDATION: New proof hash must be valid ***
    (asserts! (and (> (len new-proof-hash) u0) (<= (len new-proof-hash) u64)) err-proof-required)
    
    ;; *** VALIDATION: Only creator can resubmit ***
    (asserts! (is-eq tx-sender (get creator campaign-milestones-data)) err-unauthorized)
    
    ;; *** VALIDATION: Milestone must be rejected ***
    (asserts! (is-eq (get status milestone-data) "rejected") err-invalid-milestone)
    
    ;; Update milestone to submitted with new proof
    (map-set milestone-record
      { campaign-id: campaign-id, milestone-id: milestone-id }
      (merge milestone-data {
        status: "submitted",
        proof-hash: (some new-proof-hash),
        submitted-at: stacks-block-height
      })
    )
    
    ;; Reset approval count for re-approval
    (map-delete milestone-approval-count { campaign-id: campaign-id, milestone-id: milestone-id })
    (map-set milestone-approval-count { campaign-id: campaign-id, milestone-id: milestone-id } u0)
    (ok true)
  )
)

;; -------------------------
;; READ-ONLY FUNCTIONS
;; -------------------------

(define-read-only (get-campaign-milestones (campaign-id uint))
  (map-get? campaign-milestones campaign-id) ;; Returns summary info for campaign milestones or none
)

(define-read-only (get-milestone (campaign-id uint) (milestone-id uint))
  (map-get? milestone-record { campaign-id: campaign-id, milestone-id: milestone-id }) ;; Returns milestone details or none
)

(define-read-only (get-milestone-approval-count (campaign-id uint) (milestone-id uint))
  (default-to u0 (map-get? milestone-approval-count { campaign-id: campaign-id, milestone-id: milestone-id })) ;; Returns number of approvals (0 if none)
)

(define-read-only (get-approval-threshold)
  (ok (var-get milestone-approval-threshold)) ;; Returns the current approval threshold (how many approvers needed)
)

(define-read-only (has-approved (campaign-id uint) (milestone-id uint) (approver principal))
  (is-some (map-get? milestone-approvals { campaign-id: campaign-id, milestone-id: milestone-id, approver: approver })) ;; true if approver already approved this milestone
)

(define-public (set-approval-threshold (new-threshold uint))
  ;; Allows contract owner to change how many approvals are required per milestone
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized) ;; only owner can change
    (asserts! (> new-threshold u0) err-invalid-milestone) ;; threshold must be >= 1
    (var-set milestone-approval-threshold new-threshold) ;; persist new threshold
    (ok true)
  )
)


