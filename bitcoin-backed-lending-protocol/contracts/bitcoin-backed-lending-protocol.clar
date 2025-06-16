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
