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
