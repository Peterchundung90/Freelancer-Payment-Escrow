(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-NO-ACTIVE-CONTRACT (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-NOT-FOUND (err u104))
(define-constant ERR-WRONG-STATUS (err u105))
(define-constant ERR-DEADLINE-NOT-REACHED (err u106))
(define-constant ERR-DEADLINE-EXPIRED (err u107))
(define-constant ERR-WITHDRAWAL-PROTECTED (err u108))
(define-constant ERR-PROTECTION-ACTIVE (err u109))
(define-constant ERR-PROTECTION-PERIOD-NOT-ELAPSED (err u110))

(define-constant STATUS-PENDING u1)
(define-constant STATUS-IN-PROGRESS u2)
(define-constant STATUS-COMPLETED u3)
(define-constant STATUS-DISPUTED u4)
(define-constant STATUS-RESOLVED u5)

(define-constant DEFAULT-PROTECTION-PERIOD u144)
(define-constant MIN-PROTECTION-PERIOD u72)
(define-constant MAX-PROTECTION-PERIOD u1008)

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

(define-map WithdrawalProtection
  principal
  {
    protection-enabled: bool,
    protection-period: uint,
    protection-start-block: uint,
    emergency-contact: (optional principal),
    last-activity-block: uint
  }
)

(define-map PendingWithdrawals
  { user: principal, withdrawal-id: uint }
  {
    amount: uint,
    target-contract: uint,
    initiated-at: uint,
    withdrawal-type: uint
  }
)

(define-map UserWithdrawalCount
  principal
  uint
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
    (unwrap-panic (update-user-activity tx-sender))
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
    (unwrap-panic (update-user-activity tx-sender))
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
    (unwrap-panic (update-user-activity tx-sender))
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
    (try! (check-withdrawal-protection tx-sender contract-id u1))
    (try! (as-contract (stx-transfer? (get amount contract) tx-sender (get freelancer contract))))
    (unwrap-panic (update-user-activity tx-sender))
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
    (unwrap-panic (update-user-activity tx-sender))
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
    (try! (check-withdrawal-protection tx-sender contract-id u2))
    (try! (as-contract (stx-transfer? (get amount contract) tx-sender in-favor-of)))
    (map-set Contracts contract-id (merge contract {status: STATUS-RESOLVED}))
    (map-set ContractDisputes contract-id (merge dispute {resolved-in-favor: (some in-favor-of)}))
    (ok true)
  )
)

(define-public (enable-withdrawal-protection (protection-period uint) (emergency-contact (optional principal)))
  (let
    (
      (current-protection (map-get? WithdrawalProtection tx-sender))
    )
    (asserts! (and (>= protection-period MIN-PROTECTION-PERIOD) (<= protection-period MAX-PROTECTION-PERIOD)) ERR-INVALID-AMOUNT)
    (asserts! (is-none current-protection) ERR-ALREADY-EXISTS)
    (map-set WithdrawalProtection tx-sender {
      protection-enabled: true,
      protection-period: protection-period,
      protection-start-block: stacks-block-height,
      emergency-contact: emergency-contact,
      last-activity-block: stacks-block-height
    })
    (ok true)
  )
)

(define-public (disable-withdrawal-protection)
  (let
    (
      (protection-info (unwrap! (map-get? WithdrawalProtection tx-sender) ERR-NOT-FOUND))
      (protection-end-block (+ (get protection-start-block protection-info) (get protection-period protection-info)))
    )
    (asserts! (get protection-enabled protection-info) ERR-PROTECTION-ACTIVE)
    (asserts! (>= stacks-block-height protection-end-block) ERR-PROTECTION-PERIOD-NOT-ELAPSED)
    (map-set WithdrawalProtection tx-sender
      (merge protection-info {protection-enabled: false}))
    (ok true)
  )
)

(define-public (emergency-disable-protection (target-user principal))
  (let
    (
      (protection-info (unwrap! (map-get? WithdrawalProtection target-user) ERR-NOT-FOUND))
      (emergency-contact (get emergency-contact protection-info))
    )
    (asserts! (is-some emergency-contact) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq tx-sender (unwrap-panic emergency-contact)) ERR-NOT-AUTHORIZED)
    (asserts! (get protection-enabled protection-info) ERR-PROTECTION-ACTIVE)
    (map-set WithdrawalProtection target-user
      (merge protection-info {protection-enabled: false}))
    (ok true)
  )
)

(define-public (update-emergency-contact (new-contact (optional principal)))
  (let
    (
      (protection-info (unwrap! (map-get? WithdrawalProtection tx-sender) ERR-NOT-FOUND))
    )
    (map-set WithdrawalProtection tx-sender
      (merge protection-info {emergency-contact: new-contact}))
    (ok true)
  )
)

(define-public (initiate-protected-withdrawal (contract-id uint) (withdrawal-type uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (protection-info (unwrap! (map-get? WithdrawalProtection tx-sender) ERR-NOT-FOUND))
      (withdrawal-count (default-to u0 (map-get? UserWithdrawalCount tx-sender)))
    )
    (asserts! (get protection-enabled protection-info) ERR-PROTECTION-ACTIVE)
    (asserts! (or (is-eq (get employer contract) tx-sender) (is-eq (get freelancer contract) tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set PendingWithdrawals 
      { user: tx-sender, withdrawal-id: withdrawal-count }
      {
        amount: (get amount contract),
        target-contract: contract-id,
        initiated-at: stacks-block-height,
        withdrawal-type: withdrawal-type
      }
    )
    (map-set UserWithdrawalCount tx-sender (+ withdrawal-count u1))
    (ok withdrawal-count)
  )
)

(define-public (execute-protected-withdrawal (withdrawal-id uint))
  (let
    (
      (withdrawal-info (unwrap! (map-get? PendingWithdrawals { user: tx-sender, withdrawal-id: withdrawal-id }) ERR-NOT-FOUND))
      (protection-info (unwrap! (map-get? WithdrawalProtection tx-sender) ERR-NOT-FOUND))
      (contract (unwrap! (map-get? Contracts (get target-contract withdrawal-info)) ERR-NOT-FOUND))
      (cooling-period-end (+ (get initiated-at withdrawal-info) (get protection-period protection-info)))
    )
    (asserts! (get protection-enabled protection-info) ERR-PROTECTION-ACTIVE)
    (asserts! (>= stacks-block-height cooling-period-end) ERR-PROTECTION-PERIOD-NOT-ELAPSED)
    (asserts! (is-eq (get status contract) STATUS-COMPLETED) ERR-WRONG-STATUS)
    (try! (as-contract (stx-transfer? (get amount withdrawal-info) tx-sender (get freelancer contract))))
    (map-delete PendingWithdrawals { user: tx-sender, withdrawal-id: withdrawal-id })
    (map-set Contracts (get target-contract withdrawal-info) 
      (merge contract {status: STATUS-RESOLVED}))
    (ok true)
  )
)

(define-private (check-withdrawal-protection (user principal) (contract-id uint) (withdrawal-type uint))
  (let
    (
      (protection-info (map-get? WithdrawalProtection user))
    )
    (if (is-some protection-info)
      (let
        (
          (protection (unwrap-panic protection-info))
        )
        (if (get protection-enabled protection)
          ERR-WITHDRAWAL-PROTECTED
          (ok true)))
      (ok true))
  )
)

(define-private (update-user-activity (user principal))
  (let
    (
      (protection-info (map-get? WithdrawalProtection user))
    )
    (if (is-some protection-info)
      (let
        (
          (protection (unwrap-panic protection-info))
        )
        (begin
          (map-set WithdrawalProtection user
            (merge protection {last-activity-block: stacks-block-height}))
          (ok true)))
      (ok true))
  )
)

(define-read-only (get-contract (contract-id uint))
  (ok (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
)

(define-read-only (get-dispute (contract-id uint))
  (ok (unwrap! (map-get? ContractDisputes contract-id) ERR-NOT-FOUND))
)

(define-read-only (get-withdrawal-protection (user principal))
  (ok (map-get? WithdrawalProtection user))
)

(define-read-only (get-pending-withdrawal (user principal) (withdrawal-id uint))
  (ok (map-get? PendingWithdrawals { user: user, withdrawal-id: withdrawal-id }))
)

(define-read-only (get-user-withdrawal-count (user principal))
  (ok (default-to u0 (map-get? UserWithdrawalCount user)))
)

(define-read-only (is-withdrawal-ready (user principal) (withdrawal-id uint))
  (let
    (
      (withdrawal-info (unwrap! (map-get? PendingWithdrawals { user: user, withdrawal-id: withdrawal-id }) ERR-NOT-FOUND))
      (protection-info (unwrap! (map-get? WithdrawalProtection user) ERR-NOT-FOUND))
      (cooling-period-end (+ (get initiated-at withdrawal-info) (get protection-period protection-info)))
    )
    (ok (>= stacks-block-height cooling-period-end))
  )
)

(define-read-only (get-protection-status (user principal))
  (let
    (
      (protection-info (map-get? WithdrawalProtection user))
    )
    (if (is-some protection-info)
      (let
        (
          (protection (unwrap-panic protection-info))
          (protection-end-block (+ (get protection-start-block protection) (get protection-period protection)))
        )
        (ok {
          has-protection: true,
          protection-enabled: (get protection-enabled protection),
          can-disable: (>= stacks-block-height protection-end-block),
          blocks-until-disable: (if (>= stacks-block-height protection-end-block)
            u0
            (- protection-end-block stacks-block-height))
        }))
      (ok {
        has-protection: false,
        protection-enabled: false,
        can-disable: false,
        blocks-until-disable: u0
      }))
  )
)
