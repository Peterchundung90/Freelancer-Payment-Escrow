(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-NO-ACTIVE-CONTRACT (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-NOT-FOUND (err u104))
(define-constant ERR-WRONG-STATUS (err u105))

(define-constant STATUS-PENDING u1)
(define-constant STATUS-IN-PROGRESS u2)
(define-constant STATUS-COMPLETED u3)
(define-constant STATUS-DISPUTED u4)
(define-constant STATUS-RESOLVED u5)

(define-data-var contract-nonce uint u0)
(define-data-var mediator principal tx-sender)

(define-map Contracts
  uint
  {
    employer: principal,
    freelancer: principal,
    amount: uint,
    status: uint,
    created-at: uint
  }
)

(define-map ContractDisputes
  uint
  {
    reason: (string-ascii 256),
    resolved-in-favor: (optional principal)
  }
)

(define-public (set-mediator (new-mediator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get mediator)) ERR-NOT-AUTHORIZED)
    (ok (var-set mediator new-mediator))
  )
)

(define-public (create-contract (freelancer principal) (amount uint))
  (let
    (
      (contract-id (var-get contract-nonce))
    )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-insert Contracts contract-id {
      employer: tx-sender,
      freelancer: freelancer,
      amount: amount,
      status: STATUS-PENDING,
      created-at: stacks-block-height
    })
    (var-set contract-nonce (+ contract-id u1))
    (ok contract-id)
  )
)

(define-public (accept-contract (contract-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get freelancer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status contract) STATUS-PENDING) ERR-WRONG-STATUS)
    (map-set Contracts contract-id (merge contract {status: STATUS-IN-PROGRESS}))
    (ok true)
  )
)

(define-public (complete-work (contract-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get freelancer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status contract) STATUS-IN-PROGRESS) ERR-WRONG-STATUS)
    (map-set Contracts contract-id (merge contract {status: STATUS-COMPLETED}))
    (ok true)
  )
)

(define-public (release-payment (contract-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get employer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status contract) STATUS-COMPLETED) ERR-WRONG-STATUS)
    (try! (as-contract (stx-transfer? (get amount contract) tx-sender (get freelancer contract))))
    (map-set Contracts contract-id (merge contract {status: STATUS-RESOLVED}))
    (ok true)
  )
)

(define-public (raise-dispute (contract-id uint) (reason (string-ascii 256)))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
    )
    (asserts! (or (is-eq (get employer contract) tx-sender) (is-eq (get freelancer contract) tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq (get status contract) STATUS-IN-PROGRESS) (is-eq (get status contract) STATUS-COMPLETED)) ERR-WRONG-STATUS)
    (map-set Contracts contract-id (merge contract {status: STATUS-DISPUTED}))
    (map-set ContractDisputes contract-id {
      reason: reason,
      resolved-in-favor: none
    })
    (ok true)
  )
)

(define-public (resolve-dispute (contract-id uint) (in-favor-of principal))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (dispute (unwrap! (map-get? ContractDisputes contract-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (var-get mediator)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status contract) STATUS-DISPUTED) ERR-WRONG-STATUS)
    (asserts! (or (is-eq in-favor-of (get employer contract)) (is-eq in-favor-of (get freelancer contract))) ERR-NOT-AUTHORIZED)
    (try! (as-contract (stx-transfer? (get amount contract) tx-sender in-favor-of)))
    (map-set Contracts contract-id (merge contract {status: STATUS-RESOLVED}))
    (map-set ContractDisputes contract-id (merge dispute {resolved-in-favor: (some in-favor-of)}))
    (ok true)
  )
)

(define-read-only (get-contract (contract-id uint))
  (ok (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
)

(define-read-only (get-dispute (contract-id uint))
  (ok (unwrap! (map-get? ContractDisputes contract-id) ERR-NOT-FOUND))
)


(define-map ContractMilestones
  { contract-id: uint, milestone-id: uint }
  {
    amount: uint,
    description: (string-ascii 256),
    status: uint
  }
)

(define-map ContractMilestoneCount
  uint 
  uint
)

(define-public (add-milestone (contract-id uint) (amount uint) (description (string-ascii 256)))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (milestone-count (default-to u0 (map-get? ContractMilestoneCount contract-id)))
    )
    (asserts! (is-eq (get employer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status contract) STATUS-PENDING) ERR-WRONG-STATUS)
    (map-set ContractMilestones 
      { contract-id: contract-id, milestone-id: milestone-count }
      {
        amount: amount,
        description: description,
        status: STATUS-PENDING
      }
    )
    (map-set ContractMilestoneCount contract-id (+ milestone-count u1))
    (ok milestone-count)
  )
)

(define-public (complete-milestone (contract-id uint) (milestone-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (milestone (unwrap! (map-get? ContractMilestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get freelancer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status milestone) STATUS-IN-PROGRESS) ERR-WRONG-STATUS)
    (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get freelancer contract))))
    (map-set ContractMilestones
      { contract-id: contract-id, milestone-id: milestone-id }
      (merge milestone {status: STATUS-COMPLETED})
    )
    (ok true)
  )
)



(define-public (release-milestone-payment (contract-id uint) (milestone-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (milestone (unwrap! (map-get? ContractMilestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get employer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status milestone) STATUS-COMPLETED) ERR-WRONG-STATUS)
    (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get freelancer contract))))
    (map-set ContractMilestones
      { contract-id: contract-id, milestone-id: milestone-id }
      (merge milestone {status: STATUS-RESOLVED})
    )
    (ok true)
  )
)
(define-public (get-milestone (contract-id uint) (milestone-id uint))
  (ok (unwrap! (map-get? ContractMilestones { contract-id: contract-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
)
(define-public (get-milestone-count (contract-id uint))
  (ok (default-to u0 (map-get? ContractMilestoneCount contract-id)))
)

(define-map UserRatings
  principal
  {
    total-ratings: uint,
    total-score: uint,
    as-employer: uint,
    as-freelancer: uint
  }
)

(define-map ContractRatings
  uint
  {
    employer-rating: (optional uint),
    freelancer-rating: (optional uint),
    employer-review: (optional (string-ascii 256)),
    freelancer-review: (optional (string-ascii 256))
  }
)

(define-public (rate-contract-party (contract-id uint) (rating uint) (review (string-ascii 256)))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (existing-ratings (unwrap! (map-get? ContractRatings contract-id) ERR-NOT-FOUND))
      (target-principal (if (is-eq tx-sender (get employer contract)) 
        (get freelancer contract)
        (get employer contract)))
      (user-stats (default-to 
        { total-ratings: u0, total-score: u0, as-employer: u0, as-freelancer: u0 }
        (map-get? UserRatings target-principal)))
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (get status contract) STATUS-RESOLVED) ERR-WRONG-STATUS)
    (asserts! (or (is-eq tx-sender (get employer contract)) (is-eq tx-sender (get freelancer contract))) ERR-NOT-AUTHORIZED)
    (map-set UserRatings target-principal
      (merge user-stats {
        total-ratings: (+ (get total-ratings user-stats) u1),
        total-score: (+ (get total-score user-stats) rating)
      })
    )
    (ok true)
  )
)

(define-constant ERR-DEADLINE-NOT-REACHED (err u106))
(define-constant ERR-DEADLINE-EXPIRED (err u107))

(define-constant DEFAULT-REVIEW-PERIOD u144)

(define-map ContractDeadlines
  uint
  {
    completion-block: uint,
    review-period: uint,
    auto-release-enabled: bool
  }
)

(define-public (create-contract-with-deadline (freelancer principal) (amount uint) (review-period uint))
  (let
    (
      (contract-id (var-get contract-nonce))
    )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> review-period u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-insert Contracts contract-id {
      employer: tx-sender,
      freelancer: freelancer,
      amount: amount,
      status: STATUS-PENDING,
      created-at: stacks-block-height
    })
    (map-insert ContractDeadlines contract-id {
      completion-block: u0,
      review-period: review-period,
      auto-release-enabled: true
    })
    (var-set contract-nonce (+ contract-id u1))
    (ok contract-id)
  )
)

(define-public (complete-work-with-deadline (contract-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (deadline-info (unwrap! (map-get? ContractDeadlines contract-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get freelancer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status contract) STATUS-IN-PROGRESS) ERR-WRONG-STATUS)
    (map-set Contracts contract-id (merge contract {status: STATUS-COMPLETED}))
    (map-set ContractDeadlines contract-id 
      (merge deadline-info {completion-block: stacks-block-height}))
    (ok true)
  )
)

(define-public (auto-release-payment (contract-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (deadline-info (unwrap! (map-get? ContractDeadlines contract-id) ERR-NOT-FOUND))
      (deadline-block (+ (get completion-block deadline-info) (get review-period deadline-info)))
    )
    (asserts! (is-eq (get freelancer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status contract) STATUS-COMPLETED) ERR-WRONG-STATUS)
    (asserts! (get auto-release-enabled deadline-info) ERR-NOT-AUTHORIZED)
    (asserts! (> (get completion-block deadline-info) u0) ERR-WRONG-STATUS)
    (asserts! (>= stacks-block-height deadline-block) ERR-DEADLINE-NOT-REACHED)
    (try! (as-contract (stx-transfer? (get amount contract) tx-sender (get freelancer contract))))
    (map-set Contracts contract-id (merge contract {status: STATUS-RESOLVED}))
    (ok true)
  )
)

(define-public (release-payment-before-deadline (contract-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (deadline-info (unwrap! (map-get? ContractDeadlines contract-id) ERR-NOT-FOUND))
      (deadline-block (+ (get completion-block deadline-info) (get review-period deadline-info)))
    )
    (asserts! (is-eq (get employer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status contract) STATUS-COMPLETED) ERR-WRONG-STATUS)
    (asserts! (< stacks-block-height deadline-block) ERR-DEADLINE-EXPIRED)
    (try! (as-contract (stx-transfer? (get amount contract) tx-sender (get freelancer contract))))
    (map-set Contracts contract-id (merge contract {status: STATUS-RESOLVED}))
    (ok true)
  )
)

(define-public (toggle-auto-release (contract-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (deadline-info (unwrap! (map-get? ContractDeadlines contract-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get employer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq (get status contract) STATUS-RESOLVED)) ERR-WRONG-STATUS)
    (map-set ContractDeadlines contract-id 
      (merge deadline-info {auto-release-enabled: (not (get auto-release-enabled deadline-info))}))
    (ok (not (get auto-release-enabled deadline-info)))
  )
)

(define-read-only (get-contract-deadline (contract-id uint))
  (ok (unwrap! (map-get? ContractDeadlines contract-id) ERR-NOT-FOUND))
)

(define-read-only (get-time-until-auto-release (contract-id uint))
  (let
    (
      (deadline-info (unwrap! (map-get? ContractDeadlines contract-id) ERR-NOT-FOUND))
      (deadline-block (+ (get completion-block deadline-info) (get review-period deadline-info)))
    )
    (if (> (get completion-block deadline-info) u0)
      (if (>= stacks-block-height deadline-block)
        (ok u0)
        (ok (- deadline-block stacks-block-height)))
      (ok (get review-period deadline-info)))
  )
)

(define-read-only (can-auto-release (contract-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (deadline-info (unwrap! (map-get? ContractDeadlines contract-id) ERR-NOT-FOUND))
      (deadline-block (+ (get completion-block deadline-info) (get review-period deadline-info)))
    )
    (ok (and 
      (is-eq (get status contract) STATUS-COMPLETED)
      (get auto-release-enabled deadline-info)
      (> (get completion-block deadline-info) u0)
      (>= stacks-block-height deadline-block)))
  )
)