;; Insurance Protocol Smart Contract
;; Implements policy management, claims processing, and premium handling

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-POLICY-EXISTS (err u101))
(define-constant ERR-POLICY-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-PREMIUM (err u103))
(define-constant ERR-POLICY-EXPIRED (err u104))
(define-constant ERR-INVALID-CLAIM (err u105))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u106))

;; Data structures
(define-map policies
    { policy-id: uint, owner: principal }
    {
        coverage-amount: uint,
        premium-amount: uint,
        start-height: uint,
        end-height: uint,
        is-active: bool
    }
)

(define-map claims
    { claim-id: uint, policy-id: uint }
    {
        amount: uint,
        description: (string-ascii 256),
        status: (string-ascii 20),
        processed: bool,
        policy-id: uint
    }
)

;; Storage variables
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var contract-owner principal tx-sender)
(define-data-var total-premiums uint u0)
(define-data-var total-claims-paid uint u0)

;; Administrative functions
(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set contract-owner new-owner))
    )
)

;; Policy management functions
(define-public (create-policy (coverage-amount uint) (premium-amount uint) (duration uint))
    (let
        (
            (policy-id (var-get next-policy-id))
            (start-height block-height)
            (end-height (+ block-height duration))
        )
        (asserts! (> coverage-amount u0) (err u107))
        (asserts! (> premium-amount u0) (err u108))
        (asserts! (> duration u0) (err u109))
        
        (map-insert policies
            { policy-id: policy-id, owner: tx-sender }
            {
                coverage-amount: coverage-amount,
                premium-amount: premium-amount,
                start-height: start-height,
                end-height: end-height,
                is-active: true
            }
        )
        
        (var-set next-policy-id (+ policy-id u1))
        (ok policy-id)
    )
)

(define-public (pay-premium (policy-id uint))
    (let
        (
            (policy (unwrap! (get-policy policy-id) ERR-POLICY-NOT-FOUND))
            (premium-amount (get premium-amount policy))
        )
        (asserts! (unwrap! (is-policy-active policy-id) ERR-POLICY-NOT-FOUND) ERR-POLICY-EXPIRED)
        (try! (stx-transfer? premium-amount tx-sender (var-get contract-owner)))
        (var-set total-premiums (+ (var-get total-premiums) premium-amount))
        (ok true)
    )
)

;; Claims processing functions
(define-public (submit-claim (policy-id uint) (amount uint) (description (string-ascii 256)))
    (let
        (
            (claim-id (var-get next-claim-id))
            (policy (unwrap! (get-policy policy-id) ERR-POLICY-NOT-FOUND))
        )
        (asserts! (unwrap! (is-policy-active policy-id) ERR-POLICY-NOT-FOUND) ERR-POLICY-EXPIRED)
        (asserts! (<= amount (get coverage-amount policy)) ERR-INVALID-CLAIM)
        
        (map-insert claims
            { claim-id: claim-id, policy-id: policy-id }
            {
                amount: amount,
                description: description,
                status: "PENDING",
                processed: false,
                policy-id: policy-id
            }
        )
        
        (var-set next-claim-id (+ claim-id u1))
        (ok claim-id)
    )
)

(define-public (process-claim (claim-id uint) (policy-id uint) (approved bool))
    (let
        (
            (claim (unwrap! (get-claim claim-id) ERR-INVALID-CLAIM))
            (policy-owner (unwrap! (get-policy-owner policy-id) ERR-POLICY-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get processed claim)) ERR-CLAIM-ALREADY-PROCESSED)
        
        (if approved
            (begin
                (try! (stx-transfer? (get amount claim) (var-get contract-owner) policy-owner))
                (var-set total-claims-paid (+ (var-get total-claims-paid) (get amount claim)))
                (map-set claims
                    { claim-id: claim-id, policy-id: policy-id }
                    (merge claim { status: "APPROVED", processed: true })
                )
                (ok true)
            )
            (begin
                (map-set claims
                    { claim-id: claim-id, policy-id: policy-id }
                    (merge claim { status: "REJECTED", processed: true })
                )
                (ok true)
            )
        )
    )
)

;; Read-only functions
(define-read-only (get-policy (policy-id uint))
    (map-get? policies { policy-id: policy-id, owner: tx-sender })
)

(define-read-only (get-claim (claim-id uint))
    (map-get? claims { claim-id: claim-id, policy-id: u0 })
)

(define-read-only (get-policy-owner (policy-id uint))
    (let ((policy-key { policy-id: policy-id, owner: tx-sender }))
        (match (map-get? policies policy-key)
            policy (ok tx-sender)
            ERR-POLICY-NOT-FOUND
        )
    )
)

(define-read-only (is-policy-active (policy-id uint))
    (match (get-policy policy-id)
        policy (ok (and
            (get is-active policy)
            (<= block-height (get end-height policy))
        ))
        ERR-POLICY-NOT-FOUND
    )
)

;; Helper functions
(define-private (merge (claim-data {
        amount: uint,
        description: (string-ascii 256),
        status: (string-ascii 20),
        processed: bool,
        policy-id: uint
    }) 
    (updates {
        status: (string-ascii 20),
        processed: bool
    }))
    {
        amount: (get amount claim-data),
        description: (get description claim-data),
        status: (get status updates),
        processed: (get processed updates),
        policy-id: (get policy-id claim-data)
    }
)