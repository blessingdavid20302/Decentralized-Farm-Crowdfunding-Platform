;; Farm Insurance Pool Contract
;; Provides insurance coverage for farm campaigns against crop failures and weather risks

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u400))
(define-constant ERR_INVALID_AMOUNT (err u401))
(define-constant ERR_INSUFFICIENT_FUNDS (err u402))
(define-constant ERR_POLICY_NOT_FOUND (err u403))
(define-constant ERR_CLAIM_NOT_FOUND (err u404))
(define-constant ERR_POLICY_EXPIRED (err u405))
(define-constant ERR_CLAIM_ALREADY_FILED (err u406))
(define-constant ERR_INSUFFICIENT_POOL_BALANCE (err u407))
(define-constant ERR_INVALID_COVERAGE_PERCENTAGE (err u408))

;; Insurance configuration
(define-constant MIN_PREMIUM_RATE u25) ;; 2.5% of coverage amount
(define-constant MAX_PREMIUM_RATE u150) ;; 15% of coverage amount  
(define-constant DEFAULT_COVERAGE_PERIOD u17280) ;; ~120 days in blocks
(define-constant MAX_CLAIMS_PER_POLICY u3)
(define-constant POOL_RESERVE_RATIO u200) ;; 20% reserve requirement

;; Data variables
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var total-pool-balance uint u0)
(define-data-var total-premiums-collected uint u0)
(define-data-var total-claims-paid uint u0)
(define-data-var insurance-admin principal tx-sender)

;; Insurance policies for farm campaigns
(define-map farm-insurance-policies uint
    {
        campaign-id: uint,
        farmer: principal,
        coverage-amount: uint,
        premium-paid: uint,
        coverage-percentage: uint, ;; percentage of campaign goal covered
        policy-start: uint,
        policy-end: uint,
        risk-category: (string-ascii 20),
        is-active: bool,
        claims-filed: uint
    }
)

;; Insurance claims tracking
(define-map insurance-claims uint
    {
        policy-id: uint,
        campaign-id: uint,
        claimant: principal,
        claim-amount: uint,
        loss-percentage: uint,
        claim-reason: (string-ascii 100),
        evidence-hash: (string-ascii 64),
        claim-status: (string-ascii 20),
        filed-at: uint,
        processed-at: (optional uint),
        payout-amount: uint
    }
)

;; Pool contributor stakes and rewards
(define-map pool-contributors principal
    {
        total-contributed: uint,
        current-stake: uint,
        rewards-earned: uint,
        risk-score: uint,
        contribution-date: uint,
        is-active: bool
    }
)

;; Risk assessment data for different farm types
(define-map risk-factors (string-ascii 20)
    {
        base-premium-multiplier: uint, ;; 100 = 1.0x multiplier
        max-coverage-limit: uint,
        seasonal-adjustment: uint,
        historical-loss-rate: uint
    }
)

;; Campaign insurance eligibility and coverage
(define-map campaign-insurance-status uint
    {
        is-eligible: bool,
        risk-assessment: (string-ascii 20),
        recommended-coverage: uint,
        premium-quote: uint,
        assessment-date: uint
    }
)

;; Initialize default risk categories
(define-public (initialize-risk-categories)
    (begin
        ;; Grain crops - moderate risk
        (map-set risk-factors "grain"
            {
                base-premium-multiplier: u120,
                max-coverage-limit: u50000000, ;; 50 STX max
                seasonal-adjustment: u110,
                historical-loss-rate: u15
            }
        )
        
        ;; Vegetables - higher risk
        (map-set risk-factors "vegetables"
            {
                base-premium-multiplier: u180,
                max-coverage-limit: u30000000,
                seasonal-adjustment: u140,
                historical-loss-rate: u25
            }
        )
        
        ;; Livestock - lower risk
        (map-set risk-factors "livestock"
            {
                base-premium-multiplier: u90,
                max-coverage-limit: u100000000,
                seasonal-adjustment: u105,
                historical-loss-rate: u8
            }
        )
        
        (ok true)
    )
)

;; Contributors can add funds to the insurance pool
(define-public (contribute-to-pool (amount uint))
    (let (
        (contributor tx-sender)
        (existing-contribution (default-to 
            {
                total-contributed: u0,
                current-stake: u0,
                rewards-earned: u0,
                risk-score: u100,
                contribution-date: stacks-block-height,
                is-active: true
            }
            (map-get? pool-contributors contributor)
        ))
    )
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (>= (stx-get-balance contributor) amount) ERR_INSUFFICIENT_FUNDS)
        
        ;; Transfer funds to insurance pool
        (try! (stx-transfer? amount contributor (as-contract tx-sender)))
        
        ;; Update contributor record
        (map-set pool-contributors contributor
            {
                total-contributed: (+ (get total-contributed existing-contribution) amount),
                current-stake: (+ (get current-stake existing-contribution) amount),
                rewards-earned: (get rewards-earned existing-contribution),
                risk-score: (calculate-contributor-risk-score contributor amount),
                contribution-date: stacks-block-height,
                is-active: true
            }
        )
        
        ;; Update pool balance
        (var-set total-pool-balance (+ (var-get total-pool-balance) amount))
        
        (ok amount)
    )
)

;; Assess campaign eligibility and calculate insurance quote
(define-public (assess-campaign-for-insurance 
    (campaign-id uint)
    (risk-category (string-ascii 20))
    (requested-coverage-percentage uint))
    (let (
        (risk-data (unwrap! (map-get? risk-factors risk-category) (err u409)))
        (campaign-result (contract-call? .dcp get-campaign campaign-id))
    )
        (asserts! (is-eq tx-sender (var-get insurance-admin)) ERR_NOT_AUTHORIZED)
        (asserts! (and (>= requested-coverage-percentage u25) (<= requested-coverage-percentage u90)) ERR_INVALID_COVERAGE_PERCENTAGE)
        
        (match campaign-result
            ok-result
                (let (
                    (funding-goal (get funding-goal ok-result))
                    (coverage-amount (/ (* funding-goal requested-coverage-percentage) u100))
                    (base-premium (/ (* coverage-amount (get base-premium-multiplier risk-data)) u10000))
                    (adjusted-premium (/ (* base-premium (get seasonal-adjustment risk-data)) u100))
                )
                    ;; Store assessment results
                    (map-set campaign-insurance-status campaign-id
                        {
                            is-eligible: (<= coverage-amount (get max-coverage-limit risk-data)),
                            risk-assessment: risk-category,
                            recommended-coverage: coverage-amount,
                            premium-quote: adjusted-premium,
                            assessment-date: stacks-block-height
                        }
                    )
                    
                    (ok {
                        eligible: (<= coverage-amount (get max-coverage-limit risk-data)),
                        coverage-amount: coverage-amount,
                        premium-quote: adjusted-premium,
                        risk-category: risk-category
                    })
                )
            err-code (err err-code)
        )
    )
)

;; Purchase insurance policy for a farm campaign
(define-public (purchase-insurance-policy 
    (campaign-id uint)
    (coverage-percentage uint))
    (let (
        (farmer tx-sender)
        (assessment (unwrap! (map-get? campaign-insurance-status campaign-id) (err u410)))
        (policy-id (var-get next-policy-id))
    )
        (asserts! (get is-eligible assessment) (err u411))
        (asserts! (is-eq coverage-percentage (/ (* (get recommended-coverage assessment) u100) (get recommended-coverage assessment))) ERR_INVALID_COVERAGE_PERCENTAGE)
        
        (let (
            (premium-amount (get premium-quote assessment))
            (coverage-amount (get recommended-coverage assessment))
        )
            ;; Validate farmer has sufficient funds
            (asserts! (>= (stx-get-balance farmer) premium-amount) ERR_INSUFFICIENT_FUNDS)
            
            ;; Transfer premium to pool
            (try! (stx-transfer? premium-amount farmer (as-contract tx-sender)))
            
            ;; Create insurance policy
            (map-set farm-insurance-policies policy-id
                {
                    campaign-id: campaign-id,
                    farmer: farmer,
                    coverage-amount: coverage-amount,
                    premium-paid: premium-amount,
                    coverage-percentage: coverage-percentage,
                    policy-start: stacks-block-height,
                    policy-end: (+ stacks-block-height DEFAULT_COVERAGE_PERIOD),
                    risk-category: (get risk-assessment assessment),
                    is-active: true,
                    claims-filed: u0
                }
            )
            
            ;; Update tracking variables
            (var-set next-policy-id (+ policy-id u1))
            (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium-amount))
            (var-set total-pool-balance (+ (var-get total-pool-balance) premium-amount))
            
            (ok policy-id)
        )
    )
)

;; File insurance claim for crop losses
(define-public (file-insurance-claim 
    (policy-id uint)
    (loss-percentage uint)
    (claim-reason (string-ascii 100))
    (evidence-hash (string-ascii 64)))
    (let (
        (policy (unwrap! (map-get? farm-insurance-policies policy-id) ERR_POLICY_NOT_FOUND))
        (farmer (get farmer policy))
        (claim-id (var-get next-claim-id))
    )
        (asserts! (is-eq tx-sender farmer) ERR_NOT_AUTHORIZED)
        (asserts! (get is-active policy) ERR_POLICY_EXPIRED)
        (asserts! (>= (get policy-end policy) stacks-block-height) ERR_POLICY_EXPIRED)
        (asserts! (< (get claims-filed policy) MAX_CLAIMS_PER_POLICY) ERR_CLAIM_ALREADY_FILED)
        (asserts! (and (>= loss-percentage u25) (<= loss-percentage u100)) ERR_INVALID_AMOUNT)
        
        (let (
            (claim-amount (/ (* (get coverage-amount policy) loss-percentage) u100))
        )
            ;; Create claim record
            (map-set insurance-claims claim-id
                {
                    policy-id: policy-id,
                    campaign-id: (get campaign-id policy),
                    claimant: farmer,
                    claim-amount: claim-amount,
                    loss-percentage: loss-percentage,
                    claim-reason: claim-reason,
                    evidence-hash: evidence-hash,
                    claim-status: "PENDING",
                    filed-at: stacks-block-height,
                    processed-at: none,
                    payout-amount: u0
                }
            )
            
            ;; Update policy claims count
            (map-set farm-insurance-policies policy-id
                (merge policy { claims-filed: (+ (get claims-filed policy) u1) })
            )
            
            (var-set next-claim-id (+ claim-id u1))
            (ok claim-id)
        )
    )
)

;; Process insurance claim (admin only)
(define-public (process-claim (claim-id uint) (approve bool) (payout-percentage uint))
    (let (
        (claim (unwrap! (map-get? insurance-claims claim-id) ERR_CLAIM_NOT_FOUND))
        (policy (unwrap! (map-get? farm-insurance-policies (get policy-id claim)) ERR_POLICY_NOT_FOUND))
    )
        (asserts! (is-eq tx-sender (var-get insurance-admin)) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get claim-status claim) "PENDING") (err u412))
        (asserts! (<= payout-percentage u100) ERR_INVALID_AMOUNT)
        
        (if approve
            (let (
                (payout-amount (/ (* (get claim-amount claim) payout-percentage) u100))
                (current-pool-balance (var-get total-pool-balance))
            )
                (asserts! (>= current-pool-balance payout-amount) ERR_INSUFFICIENT_POOL_BALANCE)
                
                ;; Pay out claim
                (try! (as-contract (stx-transfer? payout-amount tx-sender (get claimant claim))))
                
                ;; Update claim status
                (map-set insurance-claims claim-id
                    (merge claim {
                        claim-status: "APPROVED",
                        processed-at: (some stacks-block-height),
                        payout-amount: payout-amount
                    })
                )
                
                ;; Update pool balance and stats
                (var-set total-pool-balance (- current-pool-balance payout-amount))
                (var-set total-claims-paid (+ (var-get total-claims-paid) payout-amount))
                
                (ok payout-amount)
            )
            (begin
                ;; Reject claim
                (map-set insurance-claims claim-id
                    (merge claim {
                        claim-status: "REJECTED",
                        processed-at: (some stacks-block-height),
                        payout-amount: u0
                    })
                )
                (ok u0)
            )
        )
    )
)

;; Distribute rewards to pool contributors based on their stake
(define-public (distribute-pool-rewards)
    (let (
        (current-pool-balance (var-get total-pool-balance))
        (total-premiums (var-get total-premiums-collected))
        (total-claims (var-get total-claims-paid))
        (profit (if (> total-premiums total-claims) (- total-premiums total-claims) u0))
    )
        (asserts! (is-eq tx-sender (var-get insurance-admin)) ERR_NOT_AUTHORIZED)
        (asserts! (> profit u0) ERR_INSUFFICIENT_FUNDS)
        
        ;; Calculate reward distribution rate (simplified - in practice would iterate through contributors)
        (let (
            (reward-rate (/ profit u100)) ;; 1% of profit as rewards
        )
            ;; Update global reward tracking
            (var-set total-pool-balance (- current-pool-balance reward-rate))
            (ok reward-rate)
        )
    )
)

;; Utility functions
(define-private (min (a uint) (b uint))
    (if (<= a b) a b)
)

;; Helper function to calculate contributor risk score
(define-private (calculate-contributor-risk-score (contributor principal) (amount uint))
    (let (
        (existing-contrib (get-contributor-safe contributor))
        (total-stake (+ (get current-stake existing-contrib) amount))
        (contribution-age (- stacks-block-height (get contribution-date existing-contrib)))
    )
        ;; Higher stake and longer contribution = better risk score
        (min u200 (+ u100 (/ total-stake u1000000) (/ contribution-age u1000)))
    )
)

;; Helper function to safely get contributor data
(define-private (get-contributor-safe (contributor principal))
    (default-to 
        {
            total-contributed: u0,
            current-stake: u0,
            rewards-earned: u0,
            risk-score: u100,
            contribution-date: stacks-block-height,
            is-active: false
        }
        (map-get? pool-contributors contributor)
    )
)

;; Read-only functions
(define-read-only (get-insurance-policy (policy-id uint))
    (map-get? farm-insurance-policies policy-id)
)

(define-read-only (get-insurance-claim (claim-id uint))
    (map-get? insurance-claims claim-id)
)

(define-read-only (get-pool-contributor (contributor principal))
    (map-get? pool-contributors contributor)
)

(define-read-only (get-campaign-assessment (campaign-id uint))
    (map-get? campaign-insurance-status campaign-id)
)

(define-read-only (get-risk-factors (risk-category (string-ascii 20)))
    (map-get? risk-factors risk-category)
)

(define-read-only (get-pool-stats)
    {
        total-balance: (var-get total-pool-balance),
        premiums-collected: (var-get total-premiums-collected),
        claims-paid: (var-get total-claims-paid),
        next-policy-id: (var-get next-policy-id),
        next-claim-id: (var-get next-claim-id)
    }
)

(define-read-only (calculate-insurance-quote (campaign-goal uint) (risk-category (string-ascii 20)) (coverage-percentage uint))
    (match (map-get? risk-factors risk-category)
        risk-data
            (let (
                (coverage-amount (/ (* campaign-goal coverage-percentage) u100))
                (base-premium (/ (* coverage-amount (get base-premium-multiplier risk-data)) u10000))
                (adjusted-premium (/ (* base-premium (get seasonal-adjustment risk-data)) u100))
            )
                (ok {
                    coverage-amount: coverage-amount,
                    premium-quote: adjusted-premium,
                    eligible: (<= coverage-amount (get max-coverage-limit risk-data))
                })
            )
        (err u413)
    )
)

;; Admin functions
(define-public (update-risk-factors (category (string-ascii 20)) (multiplier uint) (max-coverage uint) (seasonal-adj uint) (loss-rate uint))
    (begin
        (asserts! (is-eq tx-sender (var-get insurance-admin)) ERR_NOT_AUTHORIZED)
        (map-set risk-factors category
            {
                base-premium-multiplier: multiplier,
                max-coverage-limit: max-coverage,
                seasonal-adjustment: seasonal-adj,
                historical-loss-rate: loss-rate
            }
        )
        (ok true)
    )
)

(define-public (transfer-insurance-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get insurance-admin)) ERR_NOT_AUTHORIZED)
        (var-set insurance-admin new-admin)
        (ok true)
    )
)
