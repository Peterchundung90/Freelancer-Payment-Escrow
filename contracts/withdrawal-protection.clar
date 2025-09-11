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
(define-constant ERR-INSUFFICIENT-REPUTATION (err u111))
(define-constant ERR-PERFORMANCE-NOT-FOUND (err u112))

(define-constant STATUS-PENDING u1)
(define-constant STATUS-IN-PROGRESS u2)
(define-constant STATUS-COMPLETED u3)
(define-constant STATUS-DISPUTED u4)
(define-constant STATUS-RESOLVED u5)

(define-constant DEFAULT-PROTECTION-PERIOD u144)
(define-constant MIN-PROTECTION-PERIOD u72)
(define-constant MAX-PROTECTION-PERIOD u1008)

;; Performance tier constants
(define-constant TIER-BRONZE u1)
(define-constant TIER-SILVER u2)
(define-constant TIER-GOLD u3)
(define-constant TIER-PLATINUM u4)

;; Performance scoring weights (out of 100)
(define-constant WEIGHT-COMPLETION-RATE u30)
(define-constant WEIGHT-DISPUTE-RATE u25)
(define-constant WEIGHT-RESPONSE-TIME u20)
(define-constant WEIGHT-QUALITY-SCORE u25)

;; Fee discount percentages by tier
(define-constant BRONZE-DISCOUNT u0)
(define-constant SILVER-DISCOUNT u5)
(define-constant GOLD-DISCOUNT u10)
(define-constant PLATINUM-DISCOUNT u15)

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

;; Performance tracking maps
(define-map UserPerformanceMetrics
  principal
  {
    total-contracts: uint,
    completed-contracts: uint,
    disputed-contracts: uint,
    avg-completion-time: uint,
    total-quality-score: uint,
    response-time-score: uint,
    last-updated: uint
  }
)

(define-map UserReputationScore
  principal
  {
    current-score: uint,
    tier: uint,
    fee-discount: uint,
    score-history: (list 10 uint),
    achievements: (list 5 uint)
  }
)

(define-map ContractPerformanceData
  uint
  {
    start-response-time: uint,
    actual-completion-time: uint,
    quality-rating: uint,
    employer-satisfaction: uint,
    freelancer-satisfaction: uint,
    performance-recorded: bool
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

;; Performance tracking functions
(define-public (record-contract-performance (contract-id uint) (quality-rating uint) (employer-satisfaction uint) (freelancer-satisfaction uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (existing-data (map-get? ContractPerformanceData contract-id))
      (completion-time (- stacks-block-height (get created-at contract)))
    )
    (asserts! (or (is-eq tx-sender (get employer contract)) (is-eq tx-sender (get freelancer contract))) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status contract) STATUS-RESOLVED) ERR-WRONG-STATUS)
    (asserts! (and (>= quality-rating u1) (<= quality-rating u5)) ERR-INVALID-AMOUNT)
    (asserts! (and (>= employer-satisfaction u1) (<= employer-satisfaction u5)) ERR-INVALID-AMOUNT)
    (asserts! (and (>= freelancer-satisfaction u1) (<= freelancer-satisfaction u5)) ERR-INVALID-AMOUNT)
    
    (if (is-some existing-data)
      (let
        (
          (data (unwrap-panic existing-data))
        )
        (asserts! (not (get performance-recorded data)) ERR-ALREADY-EXISTS)
        (map-set ContractPerformanceData contract-id
          (merge data {
            actual-completion-time: completion-time,
            quality-rating: quality-rating,
            employer-satisfaction: employer-satisfaction,
            freelancer-satisfaction: freelancer-satisfaction,
            performance-recorded: true
          }))
        (try! (update-user-performance-metrics (get freelancer contract) contract-id quality-rating completion-time))
        (try! (update-user-reputation (get freelancer contract)))
        (ok true))
      (begin
        (map-set ContractPerformanceData contract-id {
          start-response-time: u0,
          actual-completion-time: completion-time,
          quality-rating: quality-rating,
          employer-satisfaction: employer-satisfaction,
          freelancer-satisfaction: freelancer-satisfaction,
          performance-recorded: true
        })
        (try! (update-user-performance-metrics (get freelancer contract) contract-id quality-rating completion-time))
        (try! (update-user-reputation (get freelancer contract)))
        (ok true)))
  )
)

(define-public (record-response-time (contract-id uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (response-time (- stacks-block-height (get created-at contract)))
    )
    (asserts! (is-eq (get freelancer contract) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status contract) STATUS-IN-PROGRESS) ERR-WRONG-STATUS)
    
    (map-set ContractPerformanceData contract-id {
      start-response-time: response-time,
      actual-completion-time: u0,
      quality-rating: u0,
      employer-satisfaction: u0,
      freelancer-satisfaction: u0,
      performance-recorded: false
    })
    (ok true)
  )
)

(define-private (update-user-performance-metrics (user principal) (contract-id uint) (quality-rating uint) (completion-time uint))
  (let
    (
      (contract (unwrap! (map-get? Contracts contract-id) ERR-NOT-FOUND))
      (current-metrics (default-to 
        { total-contracts: u0, completed-contracts: u0, disputed-contracts: u0, 
          avg-completion-time: u0, total-quality-score: u0, response-time-score: u0, last-updated: u0 }
        (map-get? UserPerformanceMetrics user)))
      (new-total-contracts (+ (get total-contracts current-metrics) u1))
      (new-completed-contracts (+ (get completed-contracts current-metrics) u1))
      (new-total-quality (+ (get total-quality-score current-metrics) quality-rating))
      (new-avg-completion (if (> new-completed-contracts u0)
        (/ (+ (* (get avg-completion-time current-metrics) (get completed-contracts current-metrics)) completion-time) new-completed-contracts)
        completion-time))
    )
    (map-set UserPerformanceMetrics user {
      total-contracts: new-total-contracts,
      completed-contracts: new-completed-contracts,
      disputed-contracts: (get disputed-contracts current-metrics),
      avg-completion-time: new-avg-completion,
      total-quality-score: new-total-quality,
      response-time-score: (get response-time-score current-metrics),
      last-updated: stacks-block-height
    })
    (ok true)
  )
)

(define-private (update-user-reputation (user principal))
  (let
    (
      (metrics (unwrap! (map-get? UserPerformanceMetrics user) ERR-PERFORMANCE-NOT-FOUND))
      (completion-rate (if (> (get total-contracts metrics) u0)
        (/ (* (get completed-contracts metrics) u100) (get total-contracts metrics))
        u0))
      (dispute-rate (if (> (get total-contracts metrics) u0)
        (/ (* (get disputed-contracts metrics) u100) (get total-contracts metrics))
        u0))
      (avg-quality (if (> (get completed-contracts metrics) u0)
        (/ (get total-quality-score metrics) (get completed-contracts metrics))
        u0))
      (quality-score (if (> avg-quality u0) (* avg-quality u20) u0))
      (completion-score (* completion-rate WEIGHT-COMPLETION-RATE))
      (dispute-score (* (- u100 dispute-rate) WEIGHT-DISPUTE-RATE))
      (response-score (* (get response-time-score metrics) WEIGHT-RESPONSE-TIME))
      (total-score (/ (+ completion-score dispute-score response-score quality-score) u100))
      (new-tier (calculate-tier total-score))
      (fee-discount (get-tier-discount new-tier))
      (current-reputation (map-get? UserReputationScore user))
    )
    (if (is-some current-reputation)
      (let
        (
          (reputation (unwrap-panic current-reputation))
          (score-history (get score-history reputation))
          (new-history (unwrap-panic (as-max-len? (append score-history total-score) u10)))
        )
        (map-set UserReputationScore user
          (merge reputation {
            current-score: total-score,
            tier: new-tier,
            fee-discount: fee-discount,
            score-history: new-history
          }))
        (ok true))
      (begin
        (map-set UserReputationScore user {
          current-score: total-score,
          tier: new-tier,
          fee-discount: fee-discount,
          score-history: (list total-score),
          achievements: (list)
        })
        (ok true)))
  )
)

(define-private (calculate-tier (score uint))
  (if (>= score u85)
    TIER-PLATINUM
    (if (>= score u70)
      TIER-GOLD
      (if (>= score u50)
        TIER-SILVER
        TIER-BRONZE)))
)

(define-private (get-tier-discount (tier uint))
  (if (is-eq tier TIER-PLATINUM)
    PLATINUM-DISCOUNT
    (if (is-eq tier TIER-GOLD)
      GOLD-DISCOUNT
      (if (is-eq tier TIER-SILVER)
        SILVER-DISCOUNT
        BRONZE-DISCOUNT)))
)

(define-public (get-user-performance (user principal))
  (ok (map-get? UserPerformanceMetrics user))
)

(define-public (get-user-reputation (user principal))
  (ok (map-get? UserReputationScore user))
)

(define-public (get-contract-performance (contract-id uint))
  (ok (map-get? ContractPerformanceData contract-id))
)

(define-read-only (calculate-fee-with-discount (base-fee uint) (user principal))
  (let
    (
      (reputation (map-get? UserReputationScore user))
    )
    (if (is-some reputation)
      (let
        (
          (rep (unwrap-panic reputation))
          (discount (get fee-discount rep))
          (discount-amount (/ (* base-fee discount) u100))
        )
        (ok (- base-fee discount-amount)))
      (ok base-fee))
  )
)

(define-read-only (get-tier-requirements)
  (ok {
    bronze: { min-score: u0, discount: BRONZE-DISCOUNT },
    silver: { min-score: u50, discount: SILVER-DISCOUNT },
    gold: { min-score: u70, discount: GOLD-DISCOUNT },
    platinum: { min-score: u85, discount: PLATINUM-DISCOUNT }
  })
)



