(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-campaign-inactive (err u105))
(define-constant err-campaign-active (err u106))


(define-constant err-milestone-not-found (err u200))
(define-constant err-milestone-already-completed (err u201))
(define-constant err-insufficient-milestone-funds (err u202))
(define-constant err-milestone-not-approved (err u203))
(define-constant err-already-voted (err u204))
(define-constant err-invalid-milestone-percentage (err u205))
(define-constant err-invalid-rating (err u206))
(define-constant err-already-reviewed (err u207))
(define-constant err-campaign-not-ended (err u208))


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



(define-map campaign-updates
  { campaign-id: uint, update-id: uint }
  {
    title: (string-ascii 100),
    content: (string-ascii 500),
    timestamp: uint
  }
)

(define-map campaign-update-counts
  { campaign-id: uint }
  { count: uint }
)

(define-public (post-campaign-update (campaign-id uint) (title (string-ascii 100)) (content (string-ascii 500)))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (update-count-data (default-to { count: u0 } (map-get? campaign-update-counts { campaign-id: campaign-id })))
      (new-update-id (+ (get count update-count-data) u1))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    
    (map-set campaign-updates
      { campaign-id: campaign-id, update-id: new-update-id }
      {
        title: title,
        content: content,
        timestamp: stacks-block-height
      }
    )
    
    (map-set campaign-update-counts
      { campaign-id: campaign-id }
      { count: new-update-id }
    )
    
    (ok new-update-id)
  )
)

(define-read-only (get-campaign-update (campaign-id uint) (update-id uint))
  (match (map-get? campaign-updates { campaign-id: campaign-id, update-id: update-id })
    update (ok update)
    err-not-found
  )
)


(define-constant valid-categories (list 
  "technology"
  "art"
  "music"
  "film"
  "games"
  "publishing"
))

(define-map campaign-categories
  { campaign-id: uint }
  { category: (string-ascii 20) }
)

(define-map category-campaigns
  { category: (string-ascii 20) }
  { campaigns: (list 100 uint) }
)

(define-public (set-campaign-category (campaign-id uint) (category (string-ascii 10)))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (category-data (default-to { campaigns: (list) } (map-get? category-campaigns { category: category })))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (is-some (index-of valid-categories category)) (err u118))
    
    (map-set campaign-categories
      { campaign-id: campaign-id }
      { category: category }
    )
    
    (map-set category-campaigns
      { category: category }
      { campaigns: (unwrap! (as-max-len? (append (get campaigns category-data) campaign-id) u100) (err u119)) }
    )
    
    (ok true)
  )
)

(define-read-only (get-campaign-category (campaign-id uint))
  (match (map-get? campaign-categories { campaign-id: campaign-id })
    category (ok category)
    err-not-found
  )
)

(define-read-only (get-campaigns-by-category (category (string-ascii 20)))
  (match (map-get? category-campaigns { category: category })
    result (ok result)
    err-not-found
  )
)



(define-map campaign-milestones
  { campaign-id: uint, milestone-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    funding-percentage: uint,
    is-completed: bool,
    is-approved: bool,
    votes-for: uint,
    votes-against: uint,
    total-eligible-voters: uint
  }
)

(define-map milestone-counts
  { campaign-id: uint }
  { count: uint }
)

(define-map milestone-votes
  { campaign-id: uint, milestone-id: uint, voter: principal }
  { vote: bool }
)

(define-map campaign-milestone-funds
  { campaign-id: uint }
  { 
    total-locked: uint,
    total-released: uint
  }
)

(define-public (create-milestone (campaign-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (funding-percentage uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (milestone-count-data (default-to { count: u0 } (map-get? milestone-counts { campaign-id: campaign-id })))
      (new-milestone-id (+ (get count milestone-count-data) u1))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-active campaign) err-campaign-inactive)
    (asserts! (and (> funding-percentage u0) (<= funding-percentage u100)) err-invalid-milestone-percentage)
    
    (map-set campaign-milestones
      { campaign-id: campaign-id, milestone-id: new-milestone-id }
      {
        title: title,
        description: description,
        funding-percentage: funding-percentage,
        is-completed: false,
        is-approved: false,
        votes-for: u0,
        votes-against: u0,
        total-eligible-voters: u0
      }
    )
    
    (map-set milestone-counts
      { campaign-id: campaign-id }
      { count: new-milestone-id }
    )
    
    (ok new-milestone-id)
  )
)

(define-public (complete-milestone (campaign-id uint) (milestone-id uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (milestone (unwrap! (get-milestone campaign-id milestone-id) err-milestone-not-found))
      (contributors-data (unwrap! (get-campaign-contributors campaign-id) err-not-found))
      (contributor-count (len (get contributors contributors-data)))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (not (get is-completed milestone)) err-milestone-already-completed)
    (asserts! (>= (get total-raised campaign) (get funding-goal campaign)) err-funding-goal-not-reached)
    
    (map-set campaign-milestones
      { campaign-id: campaign-id, milestone-id: milestone-id }
      (merge milestone { 
        is-completed: true,
        total-eligible-voters: contributor-count
      })
    )
    
    (ok true)
  )
)

(define-public (vote-milestone (campaign-id uint) (milestone-id uint) (approve bool))
  (let
    (
      (milestone (unwrap! (get-milestone campaign-id milestone-id) err-milestone-not-found))
      (contribution (unwrap! (get-contribution campaign-id tx-sender) err-not-found))
      (existing-vote (map-get? milestone-votes { campaign-id: campaign-id, milestone-id: milestone-id, voter: tx-sender }))
    )
    
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (get is-completed milestone) (err u206))
    (asserts! (not (get is-approved milestone)) err-milestone-already-completed)
    (asserts! (> (get amount contribution) u0) err-unauthorized)
    
    (map-set milestone-votes
      { campaign-id: campaign-id, milestone-id: milestone-id, voter: tx-sender }
      { vote: approve }
    )
    
    (if approve
      (map-set campaign-milestones
        { campaign-id: campaign-id, milestone-id: milestone-id }
        (merge milestone { votes-for: (+ (get votes-for milestone) u1) })
      )
      (map-set campaign-milestones
        { campaign-id: campaign-id, milestone-id: milestone-id }
        (merge milestone { votes-against: (+ (get votes-against milestone) u1) })
      )
    )
    
    (ok true)
  )
)

(define-public (release-milestone-funds (campaign-id uint) (milestone-id uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (milestone (unwrap! (get-milestone campaign-id milestone-id) err-milestone-not-found))
      (fund-data (default-to { total-locked: u0, total-released: u0 } (map-get? campaign-milestone-funds { campaign-id: campaign-id })))
      (total-raised (get total-raised campaign))
      (milestone-amount (/ (* total-raised (get funding-percentage milestone)) u100))
      (approval-threshold (/ (get total-eligible-voters milestone) u2))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-completed milestone) (err u207))
    (asserts! (> (get votes-for milestone) approval-threshold) err-milestone-not-approved)
    (asserts! (not (get is-approved milestone)) err-milestone-already-completed)
    
    (map-set campaign-milestones
      { campaign-id: campaign-id, milestone-id: milestone-id }
      (merge milestone { is-approved: true })
    )
    
    (map-set campaign-milestone-funds
      { campaign-id: campaign-id }
      { 
        total-locked: (get total-locked fund-data),
        total-released: (+ (get total-released fund-data) milestone-amount)
      }
    )
    
    (try! (as-contract (stx-transfer? milestone-amount tx-sender (get owner campaign))))
    
    (ok milestone-amount)
  )
)

(define-read-only (get-milestone (campaign-id uint) (milestone-id uint))
  (match (map-get? campaign-milestones { campaign-id: campaign-id, milestone-id: milestone-id })
    milestone (ok milestone)
    err-milestone-not-found
  )
)

(define-read-only (get-milestone-count (campaign-id uint))
  (match (map-get? milestone-counts { campaign-id: campaign-id })
    count (ok count)
    (ok { count: u0 })
  )
)

(define-read-only (get-milestone-funds (campaign-id uint))
  (match (map-get? campaign-milestone-funds { campaign-id: campaign-id })
    funds (ok funds)
    (ok { total-locked: u0, total-released: u0 })
  )
)

(define-read-only (get-user-milestone-vote (campaign-id uint) (milestone-id uint) (voter principal))
  (match (map-get? milestone-votes { campaign-id: campaign-id, milestone-id: milestone-id, voter: voter })
    vote (ok vote)
    err-not-found
  )
)

(define-map campaign-reviews
  { campaign-id: uint, reviewer: principal }
  {
    rating: uint,
    review-text: (string-ascii 300),
    timestamp: uint
  }
)

(define-map campaign-ratings
  { campaign-id: uint }
  {
    total-rating: uint,
    review-count: uint,
    average-rating: uint
  }
)

(define-public (submit-review (campaign-id uint) (rating uint) (review-text (string-ascii 300)))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (contribution (unwrap! (get-contribution campaign-id tx-sender) err-not-found))
      (existing-review (map-get? campaign-reviews { campaign-id: campaign-id, reviewer: tx-sender }))
      (current-ratings (default-to { total-rating: u0, review-count: u0, average-rating: u0 } 
                                   (map-get? campaign-ratings { campaign-id: campaign-id })))
    )
    
    (asserts! (is-none existing-review) err-already-reviewed)
    (asserts! (not (get is-active campaign)) err-campaign-not-ended)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    (asserts! (> (get amount contribution) u0) err-unauthorized)
    
    (map-set campaign-reviews
      { campaign-id: campaign-id, reviewer: tx-sender }
      {
        rating: rating,
        review-text: review-text,
        timestamp: stacks-block-height
      }
    )
    
    (let
      (
        (new-total-rating (+ (get total-rating current-ratings) rating))
        (new-review-count (+ (get review-count current-ratings) u1))
        (new-average-rating (/ new-total-rating new-review-count))
      )
      
      (map-set campaign-ratings
        { campaign-id: campaign-id }
        {
          total-rating: new-total-rating,
          review-count: new-review-count,
          average-rating: new-average-rating
        }
      )
    )
    
    (ok true)
  )
)

(define-read-only (get-campaign-review (campaign-id uint) (reviewer principal))
  (match (map-get? campaign-reviews { campaign-id: campaign-id, reviewer: reviewer })
    review (ok review)
    err-not-found
  )
)

(define-read-only (get-campaign-rating (campaign-id uint))
  (match (map-get? campaign-ratings { campaign-id: campaign-id })
    rating (ok rating)
    (ok { total-rating: u0, review-count: u0, average-rating: u0 })
  )
)

(define-constant err-refund-already-processed (err u300))
(define-constant err-refund-not-eligible (err u301))
(define-constant err-batch-limit-exceeded (err u302))

(define-map campaign-refund-status
  { campaign-id: uint }
  {
    is-refund-enabled: bool,
    total-refunded: uint,
    contributors-processed: uint
  }
)

(define-map contributor-refund-status
  { campaign-id: uint, contributor: principal }
  { is-refunded: bool }
)

(define-public (enable-campaign-refunds (campaign-id uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    )
    
    (asserts! (or (is-eq tx-sender (get owner campaign)) (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (not (get is-active campaign)) err-campaign-active)
    (asserts! (< (get total-raised campaign) (get funding-goal campaign)) err-funding-goal-reached)
    
    (map-set campaign-refund-status
      { campaign-id: campaign-id }
      {
        is-refund-enabled: true,
        total-refunded: u0,
        contributors-processed: u0
      }
    )
    
    (ok true)
  )
)

(define-public (batch-process-refunds (campaign-id uint) (contributors (list 20 principal)))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (refund-status (unwrap! (get-campaign-refund-status campaign-id) err-refund-not-eligible))
    )
    
    (asserts! (or (is-eq tx-sender (get owner campaign)) (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (get is-refund-enabled refund-status) err-refund-not-eligible)
    (asserts! (<= (len contributors) u20) err-batch-limit-exceeded)
    
    (var-set current-batch-campaign-id campaign-id)
    
    (let
      (
        (refund-results (fold process-refund-for-contributor
                               contributors
                               { processed: u0, failed: u0, total-amount: u0 }))
      )
      
      (map-set campaign-refund-status
        { campaign-id: campaign-id }
        {
          is-refund-enabled: (get is-refund-enabled refund-status),
          total-refunded: (+ (get total-refunded refund-status) (get total-amount refund-results)),
          contributors-processed: (+ (get contributors-processed refund-status) (get processed refund-results))
        }
      )
      
      (ok (get processed refund-results))
    )
  )
)

(define-data-var current-batch-campaign-id uint u0)

(define-private (process-refund-for-contributor 
  (contributor principal) 
  (acc { processed: uint, failed: uint, total-amount: uint }))
  (let
    (
      (campaign-id (var-get current-batch-campaign-id))
      (contribution-result (get-contribution campaign-id contributor))
      (existing-refund-status (map-get? contributor-refund-status { campaign-id: campaign-id, contributor: contributor }))
    )
    
    (if (and (is-ok contribution-result) (is-none existing-refund-status))
      (let
        (
          (contribution (unwrap-panic contribution-result))
          (amount (get amount contribution))
        )
        
        (if (> amount u0)
          (begin
            (map-set contributor-refund-status
              { campaign-id: campaign-id, contributor: contributor }
              { is-refunded: true }
            )
            
            (map-set contributions
              { campaign-id: campaign-id, contributor: contributor }
              { amount: u0, has-claimed-profit: false }
            )
            
            (match (as-contract (stx-transfer? amount tx-sender contributor))
              success {
                processed: (+ (get processed acc) u1),
                failed: (get failed acc),
                total-amount: (+ (get total-amount acc) amount)
              }
              error {
                processed: (get processed acc),
                failed: (+ (get failed acc) u1),
                total-amount: (get total-amount acc)
              }
            )
          )
          {
            processed: (get processed acc),
            failed: (+ (get failed acc) u1),
            total-amount: (get total-amount acc)
          }
        )
      )
      {
        processed: (get processed acc),
        failed: (+ (get failed acc) u1),
        total-amount: (get total-amount acc)
      }
    )
  )
)

(define-public (check-refund-eligibility (campaign-id uint) (contributor principal))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (contribution-result (get-contribution campaign-id contributor))
      (refund-status (map-get? contributor-refund-status { campaign-id: campaign-id, contributor: contributor }))
    )
    
    (if (and 
          (is-ok contribution-result)
          (not (get is-active campaign))
          (< (get total-raised campaign) (get funding-goal campaign))
          (is-none refund-status)
          (> (get amount (unwrap-panic contribution-result)) u0))
      (ok true)
      (ok false)
    )
  )
)

(define-read-only (get-campaign-refund-status (campaign-id uint))
  (match (map-get? campaign-refund-status { campaign-id: campaign-id })
    status (ok status)
    err-not-found
  )
)

(define-read-only (get-contributor-refund-status (campaign-id uint) (contributor principal))
  (match (map-get? contributor-refund-status { campaign-id: campaign-id, contributor: contributor })
    status (ok status)
    (ok { is-refunded: false })
  )
)

;; Dynamic Market Pricing System Constants
(define-constant err-invalid-market-signal (err u400))
(define-constant err-price-adjustment-not-allowed (err u401))
(define-constant err-tier-not-found (err u402))
(define-constant err-invalid-price-multiplier (err u403))
(define-constant err-market-data-stale (err u404))

;; Market signal tracking for price adjustments
(define-map market-signals
  { signal-type: (string-ascii 20) }
  {
    current-value: uint,
    last-updated: uint,
    trend-direction: bool, ;; true for up, false for down
    volatility-index: uint
  }
)

;; Campaign-specific market configurations
(define-map campaign-market-config
  { campaign-id: uint }
  {
    base-funding-goal: uint,
    current-multiplier: uint, ;; 1000 = 100% (no change)
    max-adjustment: uint, ;; maximum percentage adjustment allowed
    linked-signal: (string-ascii 20),
    auto-adjust-enabled: bool,
    last-adjustment-block: uint
  }
)

;; Contribution tiers based on market conditions
(define-map contribution-tiers
  { campaign-id: uint, tier-level: uint }
  {
    min-amount: uint,
    reward-multiplier: uint, ;; multiplier for profit sharing
    market-bonus: uint, ;; additional bonus based on market conditions
    tier-name: (string-ascii 50)
  }
)

;; Track contributor tier memberships
(define-map contributor-tiers
  { campaign-id: uint, contributor: principal }
  {
    tier-level: uint,
    locked-rate: uint, ;; locked profit rate at time of contribution
    market-bonus-earned: uint
  }
)

;; Price history for trend analysis
(define-map price-history
  { campaign-id: uint, block-height: uint }
  {
    funding-goal: uint,
    multiplier: uint,
    signal-value: uint
  }
)

;; Initialize default market signals for common farm commodities
(define-public (initialize-market-signals)
  (begin
    ;; Grain market signal
    (map-set market-signals
      { signal-type: "grain" }
      {
        current-value: u1000, ;; baseline 1000
        last-updated: stacks-block-height,
        trend-direction: true,
        volatility-index: u50
      }
    )
    
    ;; Vegetable market signal  
    (map-set market-signals
      { signal-type: "vegetables" }
      {
        current-value: u1000,
        last-updated: stacks-block-height,
        trend-direction: true,
        volatility-index: u75
      }
    )
    
    ;; Livestock market signal
    (map-set market-signals
      { signal-type: "livestock" }
      {
        current-value: u1000,
        last-updated: stacks-block-height,
        trend-direction: false,
        volatility-index: u30
      }
    )
    
    (ok true)
  )
)

;; Update market signal values (would be called by oracle in production)
(define-public (update-market-signal (signal-type (string-ascii 20)) (new-value uint) (trend-up bool) (volatility uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-value u0) err-invalid-market-signal)
    (asserts! (<= volatility u100) err-invalid-market-signal)
    
    (map-set market-signals
      { signal-type: signal-type }
      {
        current-value: new-value,
        last-updated: stacks-block-height,
        trend-direction: trend-up,
        volatility-index: volatility
      }
    )
    
    (ok true)
  )
)

;; Configure campaign for dynamic pricing
(define-public (configure-campaign-market-pricing 
  (campaign-id uint) 
  (linked-signal (string-ascii 20)) 
  (max-adjustment uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (signal (unwrap! (get-market-signal linked-signal) err-invalid-market-signal))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-active campaign) err-campaign-inactive)
    (asserts! (<= max-adjustment u500) err-invalid-price-multiplier) ;; max 50% adjustment
    
    (map-set campaign-market-config
      { campaign-id: campaign-id }
      {
        base-funding-goal: (get funding-goal campaign),
        current-multiplier: u1000, ;; start at 100%
        max-adjustment: max-adjustment,
        linked-signal: linked-signal,
        auto-adjust-enabled: true,
        last-adjustment-block: stacks-block-height
      }
    )
    
    (ok true)
  )
)

;; Create contribution tiers for enhanced rewards
(define-public (create-contribution-tier 
  (campaign-id uint) 
  (tier-level uint) 
  (min-amount uint) 
  (reward-multiplier uint)
  (tier-name (string-ascii 50)))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-active campaign) err-campaign-inactive)
    (asserts! (> min-amount u0) err-zero-amount)
    (asserts! (>= reward-multiplier u1000) err-invalid-price-multiplier) ;; min 100%
    
    (map-set contribution-tiers
      { campaign-id: campaign-id, tier-level: tier-level }
      {
        min-amount: min-amount,
        reward-multiplier: reward-multiplier,
        market-bonus: u0, ;; calculated dynamically
        tier-name: tier-name
      }
    )
    
    (ok tier-level)
  )
)

;; Calculate dynamic funding goal based on market signals
(define-private (calculate-dynamic-goal (campaign-id uint))
  (match (get-campaign-market-config campaign-id)
    config
      (let
        (
          (base-goal (get base-funding-goal config))
          (current-multiplier (get current-multiplier config))
          (signal-data (unwrap! (get-market-signal (get linked-signal config)) err-invalid-market-signal))
          (signal-value (get current-value signal-data))
        )
        
        ;; Calculate new multiplier based on signal deviation from baseline
        (let
          (
            (deviation (if (> signal-value u1000)
                         (- signal-value u1000)
                         (- u1000 signal-value)))
            (adjustment-factor (/ (* deviation u100) u1000)) ;; percentage deviation
            (capped-adjustment (if (> adjustment-factor (get max-adjustment config))
                                 (get max-adjustment config)
                                 adjustment-factor))
            (new-multiplier (if (> signal-value u1000)
                              (+ u1000 capped-adjustment)
                              (- u1000 capped-adjustment)))
          )
          
          (ok (/ (* base-goal new-multiplier) u1000))
        )
      )
    err-not-found
  )
)

;; Enhanced contribution function with tier assignment
(define-public (contribute-with-market-pricing (campaign-id uint) (amount uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (market-config (get-campaign-market-config campaign-id))
      (assigned-tier (determine-contribution-tier campaign-id amount))
    )
    
    ;; First make the regular contribution
    (try! (contribute campaign-id amount))
    
    ;; Then handle tier assignment and market bonuses if configured
    (if (is-some market-config)
      (let
        (
          (config (unwrap-panic market-config))
          (signal-data (unwrap! (get-market-signal (get linked-signal config)) err-invalid-market-signal))
          (market-bonus (calculate-market-bonus signal-data amount))
        )
        
        (map-set contributor-tiers
          { campaign-id: campaign-id, contributor: tx-sender }
          {
            tier-level: assigned-tier,
            locked-rate: (get profit-percentage campaign),
            market-bonus-earned: market-bonus
          }
        )
        
        (ok assigned-tier)
      )
      (ok u0) ;; no market configuration
    )
  )
)

;; Determine appropriate tier for contribution amount
(define-private (determine-contribution-tier (campaign-id uint) (amount uint))
  (let
    (
      ;; Check tiers from highest to lowest
      (tier-3 (get-contribution-tier campaign-id u3))
      (tier-2 (get-contribution-tier campaign-id u2))
      (tier-1 (get-contribution-tier campaign-id u1))
    )
    
    (if (and (is-some tier-3) (>= amount (get min-amount (unwrap-panic tier-3))))
      u3
      (if (and (is-some tier-2) (>= amount (get min-amount (unwrap-panic tier-2))))
        u2
        (if (and (is-some tier-1) (>= amount (get min-amount (unwrap-panic tier-1))))
          u1
          u0 ;; default tier
        )
      )
    )
  )
)

;; Calculate market-based bonus for contributions
(define-private (calculate-market-bonus (signal-data {current-value: uint, last-updated: uint, trend-direction: bool, volatility-index: uint}) (amount uint))
  (let
    (
      (volatility (get volatility-index signal-data))
      (trend-up (get trend-direction signal-data))
      (base-bonus-rate (/ volatility u20)) ;; higher volatility = higher bonus potential
    )
    
    (if trend-up
      (/ (* amount base-bonus-rate) u1000) ;; bonus for positive trends
      u0 ;; no bonus for negative trends
    )
  )
)

;; Trigger price adjustment for campaign
(define-public (adjust-campaign-pricing (campaign-id uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (config (unwrap! (get-campaign-market-config campaign-id) err-not-found))
      (new-goal (unwrap! (calculate-dynamic-goal campaign-id) err-invalid-market-signal))
      (signal-data (unwrap! (get-market-signal (get linked-signal config)) err-invalid-market-signal))
    )
    
    (asserts! (get auto-adjust-enabled config) err-price-adjustment-not-allowed)
    (asserts! (> (- stacks-block-height (get last-adjustment-block config)) u10) (err u405)) ;; cooldown period
    (asserts! (< (- stacks-block-height (get last-updated signal-data)) u100) err-market-data-stale)
    
    ;; Update campaign with new goal
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { funding-goal: new-goal })
    )
    
    ;; Update config with new multiplier
    (let
      (
        (new-multiplier (/ (* new-goal u1000) (get base-funding-goal config)))
      )
      
      (map-set campaign-market-config
        { campaign-id: campaign-id }
        (merge config { 
          current-multiplier: new-multiplier,
          last-adjustment-block: stacks-block-height
        })
      )
    )
    
    ;; Record price history
    (map-set price-history
      { campaign-id: campaign-id, block-height: stacks-block-height }
      {
        funding-goal: new-goal,
        multiplier: (get current-multiplier config),
        signal-value: (get current-value signal-data)
      }
    )
    
    (ok new-goal)
  )
)

;; Read-only functions for market data
(define-read-only (get-market-signal (signal-type (string-ascii 20)))
  (match (map-get? market-signals { signal-type: signal-type })
    signal (ok signal)
    err-not-found
  )
)

(define-read-only (get-campaign-market-config (campaign-id uint))
  (map-get? campaign-market-config { campaign-id: campaign-id })
)

(define-read-only (get-contribution-tier (campaign-id uint) (tier-level uint))
  (map-get? contribution-tiers { campaign-id: campaign-id, tier-level: tier-level })
)

(define-read-only (get-contributor-tier (campaign-id uint) (contributor principal))
  (match (map-get? contributor-tiers { campaign-id: campaign-id, contributor: contributor })
    tier (ok tier)
    err-not-found
  )
)

(define-read-only (get-price-history (campaign-id uint) (block-height uint))
  (match (map-get? price-history { campaign-id: campaign-id, block-height: block-height })
    history (ok history)
    err-not-found
  )
)

;; Calculate current effective funding goal considering market adjustments
(define-read-only (get-effective-funding-goal (campaign-id uint))
  (match (get-campaign-market-config campaign-id)
    config
      (match (calculate-dynamic-goal campaign-id)
        goal (ok goal)
        error error
      )
    (match (get-campaign campaign-id)
      campaign (ok (get funding-goal campaign))
      error error
    )
  )
)

;; Dynamic Market Pricing System Constants
(define-constant err-invalid-market-signal (err u400))
(define-constant err-price-adjustment-not-allowed (err u401))
(define-constant err-tier-not-found (err u402))
(define-constant err-invalid-price-multiplier (err u403))
(define-constant err-market-data-stale (err u404))

;; Market signal tracking for price adjustments
(define-map market-signals
  { signal-type: (string-ascii 20) }
  {
    current-value: uint,
    last-updated: uint,
    trend-direction: bool, ;; true for up, false for down
    volatility-index: uint
  }
)

;; Campaign-specific market configurations
(define-map campaign-market-config
  { campaign-id: uint }
  {
    base-funding-goal: uint,
    current-multiplier: uint, ;; 1000 = 100% (no change)
    max-adjustment: uint, ;; maximum percentage adjustment allowed
    linked-signal: (string-ascii 20),
    auto-adjust-enabled: bool,
    last-adjustment-block: uint
  }
)

;; Contribution tiers based on market conditions
(define-map contribution-tiers
  { campaign-id: uint, tier-level: uint }
  {
    min-amount: uint,
    reward-multiplier: uint, ;; multiplier for profit sharing
    market-bonus: uint, ;; additional bonus based on market conditions
    tier-name: (string-ascii 50)
  }
)

;; Track contributor tier memberships
(define-map contributor-tiers
  { campaign-id: uint, contributor: principal }
  {
    tier-level: uint,
    locked-rate: uint, ;; locked profit rate at time of contribution
    market-bonus-earned: uint
  }
)

;; Price history for trend analysis
(define-map price-history
  { campaign-id: uint, block-height: uint }
  {
    funding-goal: uint,
    multiplier: uint,
    signal-value: uint
  }
)

;; Initialize default market signals for common farm commodities
(define-public (initialize-market-signals)
  (begin
    ;; Grain market signal
    (map-set market-signals
      { signal-type: "grain" }
      {
        current-value: u1000, ;; baseline 1000
        last-updated: stacks-block-height,
        trend-direction: true,
        volatility-index: u50
      }
    )
    
    ;; Vegetable market signal  
    (map-set market-signals
      { signal-type: "vegetables" }
      {
        current-value: u1000,
        last-updated: stacks-block-height,
        trend-direction: true,
        volatility-index: u75
      }
    )
    
    ;; Livestock market signal
    (map-set market-signals
      { signal-type: "livestock" }
      {
        current-value: u1000,
        last-updated: stacks-block-height,
        trend-direction: false,
        volatility-index: u30
      }
    )
    
    (ok true)
  )
)

;; Update market signal values (would be called by oracle in production)
(define-public (update-market-signal (signal-type (string-ascii 20)) (new-value uint) (trend-up bool) (volatility uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-value u0) err-invalid-market-signal)
    (asserts! (<= volatility u100) err-invalid-market-signal)
    
    (map-set market-signals
      { signal-type: signal-type }
      {
        current-value: new-value,
        last-updated: stacks-block-height,
        trend-direction: trend-up,
        volatility-index: volatility
      }
    )
    
    (ok true)
  )
)

;; Configure campaign for dynamic pricing
(define-public (configure-campaign-market-pricing 
  (campaign-id uint) 
  (linked-signal (string-ascii 20)) 
  (max-adjustment uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (signal (unwrap! (get-market-signal linked-signal) err-invalid-market-signal))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-active campaign) err-campaign-inactive)
    (asserts! (<= max-adjustment u500) err-invalid-price-multiplier) ;; max 50% adjustment
    
    (map-set campaign-market-config
      { campaign-id: campaign-id }
      {
        base-funding-goal: (get funding-goal campaign),
        current-multiplier: u1000, ;; start at 100%
        max-adjustment: max-adjustment,
        linked-signal: linked-signal,
        auto-adjust-enabled: true,
        last-adjustment-block: stacks-block-height
      }
    )
    
    (ok true)
  )
)

;; Create contribution tiers for enhanced rewards
(define-public (create-contribution-tier 
  (campaign-id uint) 
  (tier-level uint) 
  (min-amount uint) 
  (reward-multiplier uint)
  (tier-name (string-ascii 50)))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    )
    
    (asserts! (is-eq tx-sender (get owner campaign)) err-unauthorized)
    (asserts! (get is-active campaign) err-campaign-inactive)
    (asserts! (> min-amount u0) err-zero-amount)
    (asserts! (>= reward-multiplier u1000) err-invalid-price-multiplier) ;; min 100%
    
    (map-set contribution-tiers
      { campaign-id: campaign-id, tier-level: tier-level }
      {
        min-amount: min-amount,
        reward-multiplier: reward-multiplier,
        market-bonus: u0, ;; calculated dynamically
        tier-name: tier-name
      }
    )
    
    (ok tier-level)
  )
)

;; Calculate dynamic funding goal based on market signals
(define-private (calculate-dynamic-goal (campaign-id uint))
  (match (get-campaign-market-config campaign-id)
    config
      (let
        (
          (base-goal (get base-funding-goal config))
          (current-multiplier (get current-multiplier config))
          (signal-data (unwrap! (get-market-signal (get linked-signal config)) err-invalid-market-signal))
          (signal-value (get current-value signal-data))
        )
        
        ;; Calculate new multiplier based on signal deviation from baseline
        (let
          (
            (deviation (if (> signal-value u1000)
                         (- signal-value u1000)
                         (- u1000 signal-value)))
            (adjustment-factor (/ (* deviation u100) u1000)) ;; percentage deviation
            (capped-adjustment (if (> adjustment-factor (get max-adjustment config))
                                 (get max-adjustment config)
                                 adjustment-factor))
            (new-multiplier (if (> signal-value u1000)
                              (+ u1000 capped-adjustment)
                              (- u1000 capped-adjustment)))
          )
          
          (ok (/ (* base-goal new-multiplier) u1000))
        )
      )
    err-not-found
  )
)

;; Enhanced contribution function with tier assignment
(define-public (contribute-with-market-pricing (campaign-id uint) (amount uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (market-config (get-campaign-market-config campaign-id))
      (assigned-tier (determine-contribution-tier campaign-id amount))
    )
    
    ;; First make the regular contribution
    (try! (contribute campaign-id amount))
    
    ;; Then handle tier assignment and market bonuses if configured
    (if (is-some market-config)
      (let
        (
          (config (unwrap-panic market-config))
          (signal-data (unwrap! (get-market-signal (get linked-signal config)) err-invalid-market-signal))
          (market-bonus (calculate-market-bonus signal-data amount))
        )
        
        (map-set contributor-tiers
          { campaign-id: campaign-id, contributor: tx-sender }
          {
            tier-level: assigned-tier,
            locked-rate: (get profit-percentage campaign),
            market-bonus-earned: market-bonus
          }
        )
        
        (ok assigned-tier)
      )
      (ok u0) ;; no market configuration
    )
  )
)

;; Determine appropriate tier for contribution amount
(define-private (determine-contribution-tier (campaign-id uint) (amount uint))
  (let
    (
      ;; Check tiers from highest to lowest
      (tier-3 (get-contribution-tier campaign-id u3))
      (tier-2 (get-contribution-tier campaign-id u2))
      (tier-1 (get-contribution-tier campaign-id u1))
    )
    
    (if (and (is-some tier-3) (>= amount (get min-amount (unwrap-panic tier-3))))
      u3
      (if (and (is-some tier-2) (>= amount (get min-amount (unwrap-panic tier-2))))
        u2
        (if (and (is-some tier-1) (>= amount (get min-amount (unwrap-panic tier-1))))
          u1
          u0 ;; default tier
        )
      )
    )
  )
)

;; Calculate market-based bonus for contributions
(define-private (calculate-market-bonus (signal-data {current-value: uint, last-updated: uint, trend-direction: bool, volatility-index: uint}) (amount uint))
  (let
    (
      (volatility (get volatility-index signal-data))
      (trend-up (get trend-direction signal-data))
      (base-bonus-rate (/ volatility u20)) ;; higher volatility = higher bonus potential
    )
    
    (if trend-up
      (/ (* amount base-bonus-rate) u1000) ;; bonus for positive trends
      u0 ;; no bonus for negative trends
    )
  )
)

;; Trigger price adjustment for campaign
(define-public (adjust-campaign-pricing (campaign-id uint))
  (let
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (config (unwrap! (get-campaign-market-config campaign-id) err-not-found))
      (new-goal (unwrap! (calculate-dynamic-goal campaign-id) err-invalid-market-signal))
      (signal-data (unwrap! (get-market-signal (get linked-signal config)) err-invalid-market-signal))
    )
    
    (asserts! (get auto-adjust-enabled config) err-price-adjustment-not-allowed)
    (asserts! (> (- stacks-block-height (get last-adjustment-block config)) u10) (err u405)) ;; cooldown period
    (asserts! (< (- stacks-block-height (get last-updated signal-data)) u100) err-market-data-stale)
    
    ;; Update campaign with new goal
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { funding-goal: new-goal })
    )
    
    ;; Update config with new multiplier
    (let
      (
        (new-multiplier (/ (* new-goal u1000) (get base-funding-goal config)))
      )
      
      (map-set campaign-market-config
        { campaign-id: campaign-id }
        (merge config { 
          current-multiplier: new-multiplier,
          last-adjustment-block: stacks-block-height
        })
      )
    )
    
    ;; Record price history
    (map-set price-history
      { campaign-id: campaign-id, block-height: stacks-block-height }
      {
        funding-goal: new-goal,
        multiplier: (get current-multiplier config),
        signal-value: (get current-value signal-data)
      }
    )
    
    (ok new-goal)
  )
)

;; Read-only functions for market data
(define-read-only (get-market-signal (signal-type (string-ascii 20)))
  (match (map-get? market-signals { signal-type: signal-type })
    signal (ok signal)
    err-not-found
  )
)

(define-read-only (get-campaign-market-config (campaign-id uint))
  (map-get? campaign-market-config { campaign-id: campaign-id })
)

(define-read-only (get-contribution-tier (campaign-id uint) (tier-level uint))
  (map-get? contribution-tiers { campaign-id: campaign-id, tier-level: tier-level })
)

(define-read-only (get-contributor-tier (campaign-id uint) (contributor principal))
  (match (map-get? contributor-tiers { campaign-id: campaign-id, contributor: contributor })
    tier (ok tier)
    err-not-found
  )
)

(define-read-only (get-price-history (campaign-id uint) (block-height uint))
  (match (map-get? price-history { campaign-id: campaign-id, block-height: block-height })
    history (ok history)
    err-not-found
  )
)

;; Calculate current effective funding goal considering market adjustments
(define-read-only (get-effective-funding-goal (campaign-id uint))
  (match (get-campaign-market-config campaign-id)
    config
      (match (calculate-dynamic-goal campaign-id)
        goal (ok goal)
        error error
      )
    (match (get-campaign campaign-id)
      campaign (ok (get funding-goal campaign))
      error error
    )
  )
)