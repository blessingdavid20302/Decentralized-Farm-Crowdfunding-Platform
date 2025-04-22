(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-campaign-inactive (err u105))
(define-constant err-campaign-active (err u106))
(define-constant err-funding-goal-reached (err u107))
(define-constant err-funding-goal-not-reached (err u108))
(define-constant err-deadline-not-reached (err u109))
(define-constant err-already-claimed (err u110))
(define-constant err-zero-amount (err u111))

(define-data-var platform-fee uint u50)
(define-data-var next-campaign-id uint u1)

(define-map campaigns
  { campaign-id: uint }
  {
    owner: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    funding-goal: uint,
    deadline: uint,
    is-active: bool,
    total-raised: uint,
    profit-percentage: uint,
    is-profit-distributed: bool
  }
)

(define-map contributions
  { campaign-id: uint, contributor: principal }
  {
    amount: uint,
    has-claimed-profit: bool
  }
)

(define-map campaign-contributors
  { campaign-id: uint }
  { contributors: (list 50 principal) }
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (get-campaign (campaign-id uint))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign (ok campaign)
    err-not-found
  )
)

(define-read-only (get-contribution (campaign-id uint) (contributor principal))
  (match (map-get? contributions { campaign-id: campaign-id, contributor: contributor })
    contribution (ok contribution)
    err-not-found
  )
)

(define-read-only (get-campaign-contributors (campaign-id uint))
  (match (map-get? campaign-contributors { campaign-id: campaign-id })
    result (ok result)
    err-not-found
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) (err u112))
    (ok (var-set platform-fee new-fee))
  )
)

(define-public (create-campaign (title (string-ascii 100)) (description (string-ascii 500)) (funding-goal uint) (deadline uint) (profit-percentage uint))
  (let
    (
      (campaign-id (var-get next-campaign-id))
    )
    (asserts! (> funding-goal u0) err-zero-amount)
    (asserts! (> deadline stacks-block-height) (err u113))
    (asserts! (<= profit-percentage u1000) (err u114))
    
    (map-set campaigns
      { campaign-id: campaign-id }
      {
        owner: tx-sender,
        title: title,
        description: description,
        funding-goal: funding-goal,
        deadline: deadline,
        is-active: true,
        total-raised: u0,
        profit-percentage: profit-percentage,
        is-profit-distributed: false
      }
    )
    
    (map-set campaign-contributors
      { campaign-id: campaign-id }
      { contributors: (list) }
    )
    
    (var-set next-campaign-id (+ campaign-id u1))
    (ok campaign-id)
  )
)

(define-public (contribute (campaign-id uint) (amount uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (contributor tx-sender)
      (existing-contribution (map-get? contributions { campaign-id: campaign-id, contributor: contributor }))
      (contributors-data (unwrap! (get-campaign-contributors campaign-id) err-not-found))
      (contributors-list (get contributors contributors-data))
    )
    
    (asserts! (get is-active campaign) err-campaign-inactive)
    (asserts! (< stacks-block-height (get deadline campaign)) (err u115))
    (asserts! (> amount u0) err-zero-amount)
    
    (try! (stx-transfer? amount contributor (as-contract tx-sender)))
    
    (if (is-some existing-contribution)
      (map-set contributions
        { campaign-id: campaign-id, contributor: contributor }
        {
          amount: (+ (get amount (unwrap-panic existing-contribution)) amount),
          has-claimed-profit: false
        }
      )
      (begin
        (map-set contributions
          { campaign-id: campaign-id, contributor: contributor }
          {
            amount: amount,
            has-claimed-profit: false
          }
        )
        (let ((new-list (unwrap! (as-max-len? (append contributors-list contributor) u50) (err u117))))
          (map-set campaign-contributors
            { campaign-id: campaign-id }
            { contributors: new-list }
          )
        )
      )
    )
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { total-raised: (+ (get total-raised campaign) amount) })
    )
    
    (ok true)
  )
)

(define-public (end-campaign (campaign-id uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    )
    
    (asserts! (or (is-eq tx-sender (get owner campaign)) (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (get is-active campaign) err-campaign-inactive)
    (asserts! (>= stacks-block-height (get deadline campaign)) err-deadline-not-reached)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { is-active: false })
    )
    
    (ok true)
  )
)

(define-public (withdraw-funds (campaign-id uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (total-raised (get total-raised campaign))
      (fee-amount (/ (* total-raised (var-get platform-fee)) u1000))
      (farm-amount (- total-raised fee-amount))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (not (get is-active campaign)) err-campaign-active)
    (asserts! (>= total-raised (get funding-goal campaign)) err-funding-goal-not-reached)
    
    (try! (as-contract (stx-transfer? farm-amount tx-sender (get owner campaign))))
    (try! (as-contract (stx-transfer? fee-amount tx-sender contract-owner)))
    
    (ok true)
  )
)

(define-public (refund (campaign-id uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (contribution (unwrap! (get-contribution campaign-id tx-sender) err-not-found))
      (amount (get amount contribution))
    )
    
    (asserts! (not (get is-active campaign)) err-campaign-active)
    (asserts! (< (get total-raised campaign) (get funding-goal campaign)) err-funding-goal-reached)
    (asserts! (> amount u0) err-zero-amount)
    
    (map-set contributions
      { campaign-id: campaign-id, contributor: tx-sender }
      { amount: u0, has-claimed-profit: false }
    )
    
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    (ok true)
  )
)

(define-public (distribute-profit (campaign-id uint) (profit-amount uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (not (get is-active campaign)) err-campaign-active)
    (asserts! (>= (get total-raised campaign) (get funding-goal campaign)) err-funding-goal-not-reached)
    (asserts! (not (get is-profit-distributed campaign)) err-already-claimed)
    (asserts! (> profit-amount u0) err-zero-amount)
    
    (try! (stx-transfer? profit-amount tx-sender (as-contract tx-sender)))
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { is-profit-distributed: true })
    )
    
    (ok true)
  )
)

(define-public (claim-profit (campaign-id uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (contribution (unwrap! (get-contribution campaign-id tx-sender) err-not-found))
      (total-raised (get total-raised campaign))
      (profit-percentage (get profit-percentage campaign))
    )
    
    (asserts! (not (get is-active campaign)) err-campaign-active)
    (asserts! (>= total-raised (get funding-goal campaign)) err-funding-goal-not-reached)
    (asserts! (get is-profit-distributed campaign) (err u116))
    (asserts! (not (get has-claimed-profit contribution)) err-already-claimed)
    
    (let
      (
        (contribution-amount (get amount contribution))
        (contribution-ratio (/ (* contribution-amount u1000000) total-raised))
        (profit-share (/ (* contribution-ratio profit-percentage) u1000))
      )
      
      (map-set contributions
        { campaign-id: campaign-id, contributor: tx-sender }
        (merge contribution { has-claimed-profit: true })
      )
      
      (try! (as-contract (stx-transfer? profit-share tx-sender tx-sender)))
      
      (ok profit-share)
    )
  )
)