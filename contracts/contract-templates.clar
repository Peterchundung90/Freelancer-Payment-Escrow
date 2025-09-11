;; Contract Templates System
;; Allows users to create reusable templates for common freelance work types

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-NOT-FOUND (err u104))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-INVALID-CATEGORY (err u108))

;; Template categories
(define-constant CATEGORY-WEB-DEV u1)
(define-constant CATEGORY-DESIGN u2)
(define-constant CATEGORY-WRITING u3)
(define-constant CATEGORY-MARKETING u4)
(define-constant CATEGORY-CONSULTING u5)
(define-constant CATEGORY-OTHER u6)

(define-data-var template-nonce uint u0)

;; Template data structure
(define-map Templates
  uint
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    category: uint,
    suggested-amount: uint,
    suggested-timeline: uint,
    terms: (string-ascii 1000),
    is-public: bool,
    usage-count: uint,
    created-at: uint,
    last-updated: uint
  }
)

;; User's personal templates
(define-map UserTemplates
  { user: principal, template-id: uint }
  bool
)

;; Template usage tracking
(define-map TemplateUsage
  { template-id: uint, user: principal }
  {
    usage-count: uint,
    last-used: uint,
    feedback-rating: (optional uint)
  }
)

;; Create a new template
(define-public (create-template 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (category uint)
    (suggested-amount uint)
    (suggested-timeline uint)
    (terms (string-ascii 1000))
    (is-public bool)
  )
  (let
    (
      (template-id (var-get template-nonce))
    )
    (asserts! (and (>= category CATEGORY-WEB-DEV) (<= category CATEGORY-OTHER)) ERR-INVALID-CATEGORY)
    (asserts! (> suggested-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> suggested-timeline u0) ERR-INVALID-AMOUNT)
    
    (map-set Templates template-id {
      creator: tx-sender,
      title: title,
      description: description,
      category: category,
      suggested-amount: suggested-amount,
      suggested-timeline: suggested-timeline,
      terms: terms,
      is-public: is-public,
      usage-count: u0,
      created-at: stacks-block-height,
      last-updated: stacks-block-height
    })
    
    (map-set UserTemplates { user: tx-sender, template-id: template-id } true)
    (var-set template-nonce (+ template-id u1))
    (ok template-id)
  )
)

;; Update existing template
(define-public (update-template 
    (template-id uint)
    (title (string-ascii 100))
    (description (string-ascii 500))
    (suggested-amount uint)
    (suggested-timeline uint)
    (terms (string-ascii 1000))
    (is-public bool)
  )
  (let
    (
      (template (unwrap! (map-get? Templates template-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get creator template) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> suggested-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> suggested-timeline u0) ERR-INVALID-AMOUNT)
    
    (map-set Templates template-id 
      (merge template {
        title: title,
        description: description,
        suggested-amount: suggested-amount,
        suggested-timeline: suggested-timeline,
        terms: terms,
        is-public: is-public,
        last-updated: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Use template to get contract parameters
(define-public (use-template (template-id uint))
  (let
    (
      (template (unwrap! (map-get? Templates template-id) ERR-NOT-FOUND))
      (current-usage (map-get? TemplateUsage { template-id: template-id, user: tx-sender }))
    )
    ;; Update template usage count
    (map-set Templates template-id
      (merge template { usage-count: (+ (get usage-count template) u1) }))
    
    ;; Track user's usage
    (if (is-some current-usage)
      (let
        (
          (usage (unwrap-panic current-usage))
        )
        (map-set TemplateUsage { template-id: template-id, user: tx-sender }
          (merge usage { 
            usage-count: (+ (get usage-count usage) u1),
            last-used: stacks-block-height 
          })))
      (map-set TemplateUsage { template-id: template-id, user: tx-sender } {
        usage-count: u1,
        last-used: stacks-block-height,
        feedback-rating: none
      }))
    
    (ok template)
  )
)

;; Rate template after usage
(define-public (rate-template (template-id uint) (rating uint))
  (let
    (
      (usage (unwrap! (map-get? TemplateUsage { template-id: template-id, user: tx-sender }) ERR-NOT-FOUND))
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-AMOUNT)
    (asserts! (> (get usage-count usage) u0) ERR-NOT-AUTHORIZED)
    
    (map-set TemplateUsage { template-id: template-id, user: tx-sender }
      (merge usage { feedback-rating: (some rating) }))
    (ok true)
  )
)

;; Get template details
(define-read-only (get-template (template-id uint))
  (ok (map-get? Templates template-id))
)

;; Check if user can access template
(define-read-only (can-access-template (template-id uint) (user principal))
  (let
    (
      (template (map-get? Templates template-id))
    )
    (if (is-some template)
      (let
        (
          (t (unwrap-panic template))
        )
        (ok (or 
          (get is-public t)
          (is-eq (get creator t) user)
          (default-to false (map-get? UserTemplates { user: user, template-id: template-id }))
        )))
      (ok false))
  )
)

;; Share template with specific user
(define-public (share-template (template-id uint) (target-user principal))
  (let
    (
      (template (unwrap! (map-get? Templates template-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get creator template) tx-sender) ERR-NOT-AUTHORIZED)
    (map-set UserTemplates { user: target-user, template-id: template-id } true)
    (ok true)
  )
)

;; Get template usage stats
(define-read-only (get-template-usage (template-id uint) (user principal))
  (ok (map-get? TemplateUsage { template-id: template-id, user: user }))
)
