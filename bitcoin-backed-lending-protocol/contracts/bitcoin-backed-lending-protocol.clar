;; title: bitcoin-backed-lending-protocol

;; Platform fee taken on interest (in basis points, e.g., 1000 = 10%)
(define-constant CONTRACT_FEE_BPS u1000)

;; Minimum collateralization ratio (in basis points, e.g., 15000 = 150%)
(define-constant MIN_COLLATERAL_RATIO u15000)

;; Liquidation threshold (in basis points, e.g., 12500 = 125%)
(define-constant LIQUIDATION_THRESHOLD u12500)

;; Liquidation penalty (in basis points, e.g., 1000 = 10%)
(define-constant LIQUIDATION_PENALTY u1000)

;; Interest rate model parameters (in basis points)
(define-constant BASE_RATE u200)             ;; 2% base interest rate
(define-constant RATE_SLOPE_1 u1000)         ;; Rate increase up to optimal utilization
(define-constant RATE_SLOPE_2 u10000)        ;; Rate increase above optimal utilization
(define-constant OPTIMAL_UTILIZATION u8000)  ;; 80% is optimal utilization

;; Protocol addresses
(define-constant CONTRACT_OWNER tx-sender)
(define-constant TREASURY_ADDRESS 'SP000000000000000000002Q6VF78)

;; Precision for fixed-point math (10^8)
(define-constant PRECISION u100000000)


;; Reserve factor - portion of interest that goes to reserves (in basis points)
(define-constant RESERVE_FACTOR u2000)  ;; 20% of interest goes to reserves

;; Track user deposits and borrows
(define-map user-positions 
  { user: principal } 
  {
    btc-collateral: uint,  ;; Amount of BTC collateral in satoshis
    borrowed-amount: uint, ;; Amount borrowed in micro-USD
    last-update: uint,     ;; Last block when interest was calculated
    risk-score: uint       ;; User risk score (higher is riskier)
  }
)

;; Map of Bitcoin addresses to Stacks addresses for verification
(define-map btc-address-verifications
  { btc-address: (buff 33) }
  { stacks-owner: principal, verified: bool }
)

;; Pool state
(define-data-var total-collateral uint u0)     ;; Total BTC collateral (in satoshis)
(define-data-var total-borrowed uint u0)       ;; Total borrowed (in micro-USD)
(define-data-var total-reserves uint u0)       ;; Protocol reserves (in micro-USD)
(define-data-var last-accrual-time uint u0)    ;; Last time interest was accrued
(define-data-var current-interest-rate uint u0) ;; Current interest rate (in basis points)

;; Protocol status
(define-data-var protocol-paused bool false)  ;; Emergency pause switch
(define-data-var min-borrow-amount uint u50000000)  ;; Minimum borrow amount (50 USD)
(define-data-var max-utilization uint u9000)  ;; Max utilization rate (90%)

;; Risk parameters
(define-map risk-parameters
  { risk-level: uint }
  {
    collateral-factor: uint,  ;; How much can be borrowed against collateral
    interest-multiplier: uint ;; Interest rate multiplier
  }
)

;; Protocol health metrics
(define-data-var bad-debt uint u0)  ;; Uncovered debt from liquidations
(define-data-var total-liquidations uint u0)  ;; Count of total liquidations

(define-constant ERR_UNAUTHORIZED u1)
(define-constant ERR_PAUSED u2)
(define-constant ERR_INVALID_AMOUNT u3)
(define-constant ERR_INSUFFICIENT_COLLATERAL u4)
(define-constant ERR_UTILIZATION_TOO_HIGH u5)
(define-constant ERR_BTC_NOT_VERIFIED u6)
(define-constant ERR_LOAN_NOT_FOUND u7)
(define-constant ERR_ALREADY_VERIFIED u8)
(define-constant ERR_NOT_LIQUIDATABLE u9)
(define-constant ERR_MIN_BORROW u10)
(define-constant ERR_INSUFFICIENT_FUNDS u11)

;; Helper for fixed-point multiplication
(define-private (mul-div (a uint) (b uint) (c uint))
  (/ (* a b) c)
)

;; Calculate the collateralization ratio for a position
(define-private (get-collateral-ratio (collateral uint) (borrowed uint) (btc-price uint))
  (if (is-eq borrowed u0)
    ;; If nothing is borrowed, return max uint
    (- (pow u2 u128) u1)
    ;; Otherwise, calculate collateralization ratio
    ;; (collateral * btc-price * 10000) / borrowed
    (mul-div 
      (mul-div collateral btc-price PRECISION)
      u10000
      borrowed
    )
  )
)

;; Check if a position can be liquidated
(define-private (is-liquidatable (user principal) (btc-price uint))
  (let
    (
      (position (unwrap! (map-get? user-positions { user: user }) false))
      (collateral (get btc-collateral position))
      (borrowed (get borrowed-amount position))
      (collateral-value (mul-div collateral btc-price PRECISION))
      (collateral-ratio (get-collateral-ratio collateral borrowed btc-price))
    )
    (< collateral-ratio LIQUIDATION_THRESHOLD)
  )
)

;; Initialize the protocol
(define-public (initialize)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_UNAUTHORIZED))
    (var-set last-accrual-time stacks-block-height)
    (var-set current-interest-rate (+ BASE_RATE (/ RATE_SLOPE_1 u2)))
    
    ;; Set up initial risk parameters
    (map-set risk-parameters { risk-level: u10 }
      { collateral-factor: u9000, interest-multiplier: u8000 }) ;; Low risk: 90% LTV, 80% interest rate
    (map-set risk-parameters { risk-level: u50 }
      { collateral-factor: u8000, interest-multiplier: u10000 }) ;; Medium risk: 80% LTV, normal interest
    (map-set risk-parameters { risk-level: u90 }
      { collateral-factor: u7000, interest-multiplier: u12000 }) ;; High risk: 70% LTV, 120% interest rate
    
    (ok true)
  )
)

;; Update risk parameters
(define-public (update-risk-parameters (risk-level uint) (collateral-factor uint) (interest-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_UNAUTHORIZED))
    (asserts! (<= collateral-factor u9000) (err ERR_INVALID_AMOUNT)) ;; Max 90% LTV
    
    (map-set risk-parameters { risk-level: risk-level }
      { 
        collateral-factor: collateral-factor,
        interest-multiplier: interest-multiplier
      }
    )
    
    (ok true)
  )
)

;; Toggle protocol pause state
(define-public (toggle-protocol-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_UNAUTHORIZED))
    (var-set protocol-paused (not (var-get protocol-paused)))
    (ok (var-get protocol-paused))
  )
)

;; Update protocol parameters
(define-public (update-protocol-parameters 
                (new-min-borrow uint) 
                (new-max-utilization uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_UNAUTHORIZED))
    (asserts! (>= new-max-utilization u5000) (err ERR_INVALID_AMOUNT)) ;; Min 50% utilization
    (asserts! (<= new-max-utilization u9500) (err ERR_INVALID_AMOUNT)) ;; Max 95% utilization
    
    (var-set min-borrow-amount new-min-borrow)
    (var-set max-utilization new-max-utilization)
    
    (ok true)
  )
)

;; Multi-Collateral Support Module

;; Asset configuration map
(define-map supported-assets
  { asset-id: uint }
  {
    asset-type: (string-ascii 10),        ;; "STX", "BTC", "NFT", etc.
    asset-contract: principal,            ;; Contract address for the asset
    oracle-contract: principal,           ;; Price oracle contract
    ltv-ratio: uint,                      ;; Loan-to-value ratio (in basis points)
    liquidation-threshold: uint,          ;; When position becomes liquidatable (in basis points)
    liquidation-penalty: uint,            ;; Penalty for liquidation (in basis points)
    borrowing-enabled: bool,              ;; Can this asset be borrowed against
    borrow-cap: uint,                     ;; Maximum amount that can be borrowed against this asset
    reserve-factor: uint                  ;; Portion of interest that goes to reserves (in basis points)
  }
)

;; Asset state (for each supported asset)
(define-map asset-state
  { asset-id: uint }
  {
    total-supplied: uint,                 ;; Total amount supplied of this asset
    total-borrowed: uint,                 ;; Total amount borrowed against this asset
    utilization: uint,                    ;; Current utilization ratio (in basis points)
    interest-rate: uint,                  ;; Current interest rate (in basis points)
    exchange-rate: uint                   ;; Exchange rate between asset and internal token
  }
)

;; Extended user positions to support multiple assets
(define-map multi-asset-positions
  { user: principal, asset-id: uint }
  {
    supplied-amount: uint,                ;; Amount supplied of this asset
    borrowed-amount: uint,                ;; Amount borrowed against this asset
    last-update: uint,                    ;; Last block when interest was calculated
    ltv-override: uint                    ;; Optional override for LTV (0 means use default)
  }
)

;; Asset Registry Functions

;; Add a new supported asset
(define-public (register-asset 
               (asset-id uint) 
               (asset-type (string-ascii 10))
               (asset-contract principal)
               (oracle-contract principal)
               (ltv-ratio uint)
               (liquidation-threshold uint)
               (liquidation-penalty uint)
               (reserve-factor uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_UNAUTHORIZED))
    (asserts! (> liquidation-threshold ltv-ratio) (err ERR_INVALID_AMOUNT))
    (asserts! (<= ltv-ratio u9000) (err ERR_INVALID_AMOUNT)) ;; Max 90% LTV
    
    (map-set supported-assets 
      { asset-id: asset-id }
      {
        asset-type: asset-type,
        asset-contract: asset-contract,
        oracle-contract: oracle-contract, 
        ltv-ratio: ltv-ratio,
        liquidation-threshold: liquidation-threshold,
        liquidation-penalty: liquidation-penalty,
        borrowing-enabled: true,
        borrow-cap: u0,  ;; Unlimited by default
        reserve-factor: reserve-factor
      }
    )
    
    (map-set asset-state
      { asset-id: asset-id }
      {
        total-supplied: u0,
        total-borrowed: u0,
        utilization: u0,
        interest-rate: (+ BASE_RATE (/ RATE_SLOPE_1 u2)),
        exchange-rate: PRECISION  ;; Initial 1:1 exchange rate
      }
    )
    
    (ok true)
  )
)

;; Update asset parameters
(define-public (update-asset-parameters
               (asset-id uint)
               (ltv-ratio uint)
               (liquidation-threshold uint)
               (liquidation-penalty uint)
               (borrowing-enabled bool)
               (borrow-cap uint)
               (reserve-factor uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) (err ERR_UNAUTHORIZED))
    (asserts! (> liquidation-threshold ltv-ratio) (err ERR_INVALID_AMOUNT))
    (asserts! (<= ltv-ratio u9000) (err ERR_INVALID_AMOUNT)) ;; Max 90% LTV
    
    (let
      ((asset (unwrap! (map-get? supported-assets { asset-id: asset-id }) (err ERR_INVALID_AMOUNT))))
      
      (map-set supported-assets 
        { asset-id: asset-id }
        (merge asset {
          ltv-ratio: ltv-ratio,
          liquidation-threshold: liquidation-threshold,
          liquidation-penalty: liquidation-penalty,
          borrowing-enabled: borrowing-enabled,
          borrow-cap: borrow-cap,
          reserve-factor: reserve-factor
        })
      )
    )
    
    (ok true)
  )
)

;; Supply Asset Function
(define-public (supply-asset (asset-id uint) (amount uint))
  (begin
    (asserts! (not (var-get protocol-paused)) (err ERR_PAUSED))
    (asserts! (> amount u0) (err ERR_INVALID_AMOUNT))
    
    (let
      (
        (asset (unwrap! (map-get? supported-assets { asset-id: asset-id }) (err ERR_INVALID_AMOUNT)))
        (asset-contract (get asset-contract asset))
        (position-key { user: tx-sender, asset-id: asset-id })
        (current-position (default-to 
          { supplied-amount: u0, borrowed-amount: u0, last-update: stacks-block-height, ltv-override: u0 }
          (map-get? multi-asset-positions position-key)))
        (new-supplied-amount (+ (get supplied-amount current-position) amount))
      )
      
      ;; Transfer asset from user to contract (implementation depends on asset type)
      ;; For fungible tokens, we would call a transfer function
      ;; This is simplified and would need to be adjusted based on asset type
      ;; (contract-call? asset-contract transfer amount tx-sender (as-contract tx-sender) none)
      
      ;; Update user position
      (map-set multi-asset-positions
        position-key
        (merge current-position { 
          supplied-amount: new-supplied-amount,
          last-update: stacks-block-height
        })
      )
      
      ;; Update asset state
      (let
        (
          (current-state (unwrap! (map-get? asset-state { asset-id: asset-id }) (err ERR_INVALID_AMOUNT)))
          (new-total-supplied (+ (get total-supplied current-state) amount))
          (new-utilization (if (is-eq new-total-supplied u0)
                              u0
                              (mul-div (get total-borrowed current-state) PRECISION new-total-supplied)))
        )
        
        (map-set asset-state
          { asset-id: asset-id }
          (merge current-state {
            total-supplied: new-total-supplied,
            utilization: new-utilization
          })
        )
      )
      
      (ok true)
    )
  )
)

;; Helper to calculate collateral value for a single asset
(define-private (calculate-asset-collateral-value (asset-id uint) (current-value uint))
  (let
    (
      (position-key { user: tx-sender, asset-id: asset-id })
      (position (default-to 
        { supplied-amount: u0, borrowed-amount: u0, last-update: u0, ltv-override: u0 }
        (map-get? multi-asset-positions position-key)))
      (supplied-amount (get supplied-amount position))
      ;; Would need to get the price from oracle here
      (asset-price u0)  ;; Placeholder
    )
    (+ current-value (mul-div supplied-amount asset-price PRECISION))
  )
)

;; Flash Loan Service Module

;; Flash loan fee in basis points (0.09%)
(define-constant FLASH_LOAN_FEE_BPS u9)

;; Flash loan state to prevent re-entrancy
(define-data-var flash-loan-in-progress bool false)
(define-data-var flash-loan-user principal 'SP000000000000000000002Q6VF78)
(define-data-var flash-loan-amount uint u0)
(define-data-var flash-loan-asset uint u0)

;; Governance System Module

;; Governance token SIP-010 interface (would be defined in a separate contract)
(define-trait governance-token-trait
  (
    (get-balance (principal) (response uint uint))
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-total-supply () (response uint uint))
  )
)

;; Proposal states
(define-constant PROPOSAL_STATE_PENDING u0)
(define-constant PROPOSAL_STATE_ACTIVE u1)
(define-constant PROPOSAL_STATE_CANCELED u2)
(define-constant PROPOSAL_STATE_DEFEATED u3)
(define-constant PROPOSAL_STATE_SUCCEEDED u4)
(define-constant PROPOSAL_STATE_QUEUED u5)
(define-constant PROPOSAL_STATE_EXPIRED u6)
(define-constant PROPOSAL_STATE_EXECUTED u7)


