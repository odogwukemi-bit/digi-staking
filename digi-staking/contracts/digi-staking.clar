;; DigiStaking Protocol
;; A liquid staking protocol that allows users to stake STX and receive

;; Constants

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-ZERO-AMOUNT (err u102))
(define-constant ERR-PAUSED (err u103))
(define-constant ERR-NOT-FOUND (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-TIMELOCK-NOT-EXPIRED (err u106))
(define-constant ERR-INVALID-AMOUNT (err u107))

;; Basis points denominator (100.00%)
(define-constant BASIS-POINTS u10000)

;; Minimum stake amount (1 STX in microSTX)
(define-constant MIN-STAKE u1000000)

;; Insurance pool fee: 0.5% of yield
(define-constant INSURANCE-FEE-BPS u50)

;; Governance timelock: ~1 day in blocks (~144 blocks)
(define-constant GOVERNANCE-TIMELOCK u144)

;; ============================================================
;; Data Variables
;; ============================================================

;; Protocol pause flag
(define-data-var is-paused bool false)

;; Total STX staked in protocol
(define-data-var total-staked uint u0)

;; Total dTokens minted
(define-data-var total-dtokens uint u0)

;; Accumulated yield (in microSTX)
(define-data-var total-yield-accrued uint u0)

;; Insurance pool balance (in microSTX)
(define-data-var insurance-pool-balance uint u0)

;; Annual yield rate in basis points (e.g., 800 = 8.00%)
(define-data-var yield-rate-bps uint u800)

;; Proposed yield rate (for timelock governance)
(define-data-var proposed-yield-rate-bps uint u0)

;; Block height at which governance change was proposed
(define-data-var governance-proposal-block uint u0)

;; Next validator id
(define-data-var next-validator-id uint u0)

;; ============================================================
;; Data Maps
;; ============================================================

;; dToken balances per user
(define-map dtokens-balance
  { holder: principal }
  { balance: uint })

;; DIGI governance token balances per user
(define-map digi-balance
  { holder: principal }
  { balance: uint })

;; Staking positions per user
(define-map staking-positions
  { staker: principal }
  {
    staked-amount: uint,
    dtokens-minted: uint,
    stake-block: uint,
    last-yield-block: uint
  })

;; Validators registered in the protocol
(define-map validators
  { validator-id: uint }
  {
    validator-address: principal,
    total-delegated: uint,
    performance-score: uint,
    is-active: bool
  })

;; Validator index by address
(define-map validator-index
  { validator-address: principal }
  { validator-id: uint })

;; ============================================================
;; Private Helper Functions
;; ============================================================

;; Check if caller is contract owner
(define-private (is-owner)
  (is-eq tx-sender CONTRACT-OWNER))

;; Check protocol is not paused; returns (ok true) or ERR-PAUSED
(define-private (check-not-paused)
  (if (var-get is-paused) ERR-PAUSED (ok true)))

;; Calculate dTokens to mint for a given STX amount.
;; Exchange rate grows as yield accrues into total-staked.
;; Genesis rate is 1:1.
(define-private (calculate-dtokens-for-stx (stx-amount uint))
  (let (
    (total-st (var-get total-staked))
    (total-dt (var-get total-dtokens))
  )
    (if (or (is-eq total-st u0) (is-eq total-dt u0))
      stx-amount
      (/ (* stx-amount total-dt) total-st)
    )
  ))

;; Calculate pending yield for a staker based on elapsed blocks.
;; Simplified linear yield: yield = staked * rate * blocks / (BASIS-POINTS * annual-blocks)
(define-private (calculate-pending-yield (staked-amount uint) (from-block uint))
  (let (
    (blocks-elapsed (- block-height from-block))
    (annual-blocks u52560)
    (rate (var-get yield-rate-bps))
  )
    (/ (* (* staked-amount rate) blocks-elapsed) (* BASIS-POINTS annual-blocks))
  ))

;; Mint dTokens to a principal. Uses asserts! so the err type is (uint).
(define-private (mint-dtokens (recipient principal) (amount uint))
  (let (
    (current-bal (default-to { balance: u0 }
      (map-get? dtokens-balance { holder: recipient })))
  )
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    (map-set dtokens-balance
      { holder: recipient }
      { balance: (+ (get balance current-bal) amount) })
    (var-set total-dtokens (+ (var-get total-dtokens) amount))
    (ok amount)
  ))

;; Burn dTokens from a principal.
(define-private (burn-dtokens (from principal) (amount uint))
  (let (
    (current-bal (default-to { balance: u0 }
      (map-get? dtokens-balance { holder: from })))
  )
    (asserts! (>= (get balance current-bal) amount) ERR-INSUFFICIENT-BALANCE)
    (map-set dtokens-balance
      { holder: from }
      { balance: (- (get balance current-bal) amount) })
    (var-set total-dtokens (- (var-get total-dtokens) amount))
    (ok amount)
  ))

;; Mint DIGI governance tokens proportional to staking duration.
(define-private (mint-digi (recipient principal) (staked-amount uint) (blocks-staked uint))
  (let (
    (digi-amount (/ (* staked-amount blocks-staked) u52560))
    (current-bal (default-to { balance: u0 }
      (map-get? digi-balance { holder: recipient })))
  )
    (asserts! (>= staked-amount u0) ERR-ZERO-AMOUNT)
    (map-set digi-balance
      { holder: recipient }
      { balance: (+ (get balance current-bal) digi-amount) })
    (ok digi-amount)
  ))

;; Deduct insurance fee from yield, credit pool, return net yield.
(define-private (deduct-insurance-fee (yield-amount uint))
  (let (
    (fee (/ (* yield-amount INSURANCE-FEE-BPS) BASIS-POINTS))
    (net-yield (- yield-amount fee))
  )
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) fee))
    net-yield
  ))

;; ============================================================
;; Public Functions
;; ============================================================

;; Stake STX to receive dTokens
(define-public (stake (stx-amount uint))
  (begin
    (try! (check-not-paused))
    (asserts! (>= stx-amount MIN-STAKE) ERR-INVALID-AMOUNT)
    (asserts! (is-none (map-get? staking-positions { staker: tx-sender })) ERR-ALREADY-EXISTS)

    ;; Transfer STX from staker to contract
    (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))

    (let (
      (dtokens-to-mint (calculate-dtokens-for-stx stx-amount))
    )
      (var-set total-staked (+ (var-get total-staked) stx-amount))

      (map-set staking-positions
        { staker: tx-sender }
        {
          staked-amount: stx-amount,
          dtokens-minted: dtokens-to-mint,
          stake-block: block-height,
          last-yield-block: block-height
        })

      (try! (mint-dtokens tx-sender dtokens-to-mint))

      (print { event: "staked", staker: tx-sender, stx-amount: stx-amount, dtokens-minted: dtokens-to-mint })
      (ok dtokens-to-mint)
    )
  ))

;; Unstake: burn dTokens to reclaim STX plus accrued yield.
;; Staker principal is captured before as-contract changes tx-sender context.
(define-public (unstake)
  (let (
    (staker tx-sender)
  )
    (try! (check-not-paused))

    (let (
      (position (unwrap! (map-get? staking-positions { staker: staker }) ERR-NOT-FOUND))
      (staked-amount (get staked-amount position))
      (dtokens-minted (get dtokens-minted position))
      (last-yield-block (get last-yield-block position))
      (stake-block (get stake-block position))
    )
      (let (
        (raw-yield (calculate-pending-yield staked-amount last-yield-block))
        (net-yield (deduct-insurance-fee raw-yield))
        (total-return (+ staked-amount net-yield))
        (blocks-staked (- block-height stake-block))
      )
        (try! (burn-dtokens staker dtokens-minted))

        (var-set total-staked (- (var-get total-staked) staked-amount))
        (var-set total-yield-accrued (+ (var-get total-yield-accrued) net-yield))

        (map-delete staking-positions { staker: staker })

        ;; DIGI tokens are minted proportional to staking duration
        (try! (mint-digi staker staked-amount blocks-staked))

        ;; Contract sends STX + yield back to the original caller
        (try! (as-contract (stx-transfer? total-return tx-sender staker)))

        (print { event: "unstaked", staker: staker, stx-returned: staked-amount, yield: net-yield, digi-minted: blocks-staked })
        (ok total-return)
      )
    )
  ))

;; Claim accrued yield without unstaking
(define-public (claim-yield)
  (let (
    (staker tx-sender)
  )
    (try! (check-not-paused))

    (let (
      (position (unwrap! (map-get? staking-positions { staker: staker }) ERR-NOT-FOUND))
      (staked-amount (get staked-amount position))
      (last-yield-block (get last-yield-block position))
    )
      (let (
        (raw-yield (calculate-pending-yield staked-amount last-yield-block))
        (net-yield (deduct-insurance-fee raw-yield))
      )
        (asserts! (> net-yield u0) ERR-ZERO-AMOUNT)

        (map-set staking-positions
          { staker: staker }
          (merge position { last-yield-block: block-height }))

        (var-set total-yield-accrued (+ (var-get total-yield-accrued) net-yield))

        (try! (as-contract (stx-transfer? net-yield tx-sender staker)))

        (print { event: "yield-claimed", staker: staker, yield: net-yield })
        (ok net-yield)
      )
    )
  ))

;; Transfer dTokens to another principal
(define-public (transfer-dtokens (amount uint) (recipient principal))
  (begin
    (try! (check-not-paused))
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)

    (let (
      (sender-bal (default-to { balance: u0 }
        (map-get? dtokens-balance { holder: tx-sender })))
      (recipient-bal (default-to { balance: u0 }
        (map-get? dtokens-balance { holder: recipient })))
    )
      (asserts! (>= (get balance sender-bal) amount) ERR-INSUFFICIENT-BALANCE)

      (map-set dtokens-balance
        { holder: tx-sender }
        { balance: (- (get balance sender-bal) amount) })
      (map-set dtokens-balance
        { holder: recipient }
        { balance: (+ (get balance recipient-bal) amount) })

      (print { event: "dtokens-transferred", from: tx-sender, to: recipient, amount: amount })
      (ok true)
    )
  ))

;; Register a new validator (owner only)
(define-public (register-validator (validator-address principal))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? validator-index { validator-address: validator-address })) ERR-ALREADY-EXISTS)

    (let (
      (vid (var-get next-validator-id))
    )
      (map-set validators
        { validator-id: vid }
        {
          validator-address: validator-address,
          total-delegated: u0,
          performance-score: u10000,
          is-active: true
        })
      (map-set validator-index
        { validator-address: validator-address }
        { validator-id: vid })
      (var-set next-validator-id (+ vid u1))

      (print { event: "validator-registered", validator-id: vid, address: validator-address })
      (ok vid)
    )
  ))

;; Update validator performance score (owner only)
(define-public (update-validator-score (validator-id uint) (new-score uint))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-score BASIS-POINTS) ERR-INVALID-AMOUNT)

    (let (
      (validator (unwrap! (map-get? validators { validator-id: validator-id }) ERR-NOT-FOUND))
    )
      (map-set validators
        { validator-id: validator-id }
        (merge validator { performance-score: new-score }))

      (print { event: "validator-score-updated", validator-id: validator-id, score: new-score })
      (ok true)
    )
  ))

;; Deactivate a validator (owner only)
(define-public (deactivate-validator (validator-id uint))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)

    (let (
      (validator (unwrap! (map-get? validators { validator-id: validator-id }) ERR-NOT-FOUND))
    )
      (map-set validators
        { validator-id: validator-id }
        (merge validator { is-active: false }))

      (print { event: "validator-deactivated", validator-id: validator-id })
      (ok true)
    )
  ))

;; Compensate user from insurance pool after a slashing event (owner only)
(define-public (insurance-compensate (user principal) (amount uint))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (asserts! (<= amount (var-get insurance-pool-balance)) ERR-INSUFFICIENT-BALANCE)

    (var-set insurance-pool-balance (- (var-get insurance-pool-balance) amount))
    (try! (as-contract (stx-transfer? amount tx-sender user)))

    (print { event: "insurance-compensation", user: user, amount: amount })
    (ok true)
  ))

;; Propose a new yield rate; subject to timelock before taking effect
(define-public (propose-yield-rate (new-rate-bps uint))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-rate-bps u5000) ERR-INVALID-AMOUNT)

    (var-set proposed-yield-rate-bps new-rate-bps)
    (var-set governance-proposal-block block-height)

    (print { event: "yield-rate-proposed", new-rate: new-rate-bps, executable-after: (+ block-height GOVERNANCE-TIMELOCK) })
    (ok true)
  ))

;; Execute proposed yield rate change after the timelock expires
(define-public (execute-yield-rate-change)
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (asserts! (>= block-height (+ (var-get governance-proposal-block) GOVERNANCE-TIMELOCK)) ERR-TIMELOCK-NOT-EXPIRED)
    (asserts! (> (var-get proposed-yield-rate-bps) u0) ERR-NOT-FOUND)

    (let (
      (old-rate (var-get yield-rate-bps))
      (new-rate (var-get proposed-yield-rate-bps))
    )
      (var-set yield-rate-bps new-rate)
      (var-set proposed-yield-rate-bps u0)

      (print { event: "yield-rate-updated", old-rate: old-rate, new-rate: new-rate })
      (ok new-rate)
    )
  ))

;; Emergency pause toggle (owner only)
(define-public (set-paused (paused bool))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (var-set is-paused paused)
    (print { event: "pause-state-changed", paused: paused })
    (ok true)
  ))

;; ============================================================
;; Read-Only Functions
;; ============================================================

;; Get dToken balance for a holder
(define-read-only (get-dtoken-balance (holder principal))
  (get balance (default-to { balance: u0 } (map-get? dtokens-balance { holder: holder }))))

;; Get DIGI governance token balance
(define-read-only (get-digi-balance (holder principal))
  (get balance (default-to { balance: u0 } (map-get? digi-balance { holder: holder }))))

;; Get staking position details
(define-read-only (get-staking-position (staker principal))
  (map-get? staking-positions { staker: staker }))

;; Get pending net yield for a staker (after insurance fee deduction)
(define-read-only (get-pending-yield (staker principal))
  (match (map-get? staking-positions { staker: staker })
    position
      (let (
        (raw (calculate-pending-yield
          (get staked-amount position)
          (get last-yield-block position)))
        (fee (/ (* raw INSURANCE-FEE-BPS) BASIS-POINTS))
      )
        (- raw fee)
      )
    u0
  ))

;; Get current dToken exchange rate expressed in basis points (STX per dToken)
(define-read-only (get-exchange-rate)
  (let (
    (total-st (var-get total-staked))
    (total-dt (var-get total-dtokens))
  )
    (if (or (is-eq total-dt u0) (is-eq total-st u0))
      BASIS-POINTS
      (/ (* total-st BASIS-POINTS) total-dt)
    )
  ))

;; Get high-level protocol stats
(define-read-only (get-protocol-stats)
  {
    total-staked: (var-get total-staked),
    total-dtokens: (var-get total-dtokens),
    total-yield-accrued: (var-get total-yield-accrued),
    insurance-pool: (var-get insurance-pool-balance),
    yield-rate-bps: (var-get yield-rate-bps),
    is-paused: (var-get is-paused),
    exchange-rate-bps: (get-exchange-rate)
  })

;; Get validator details by id
(define-read-only (get-validator (validator-id uint))
  (map-get? validators { validator-id: validator-id }))

;; Get validator id by address
(define-read-only (get-validator-id (validator-address principal))
  (map-get? validator-index { validator-address: validator-address }))

;; Get current governance proposal details
(define-read-only (get-governance-proposal)
  {
    proposed-rate: (var-get proposed-yield-rate-bps),
    proposal-block: (var-get governance-proposal-block),
    executable-after: (+ (var-get governance-proposal-block) GOVERNANCE-TIMELOCK),
    current-block: block-height
  })
