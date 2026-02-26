# FIFO/LIFO Automatic Lot Matching

## Overview

Ledger now supports automatic FIFO (First-In-First-Out) and LIFO (Last-In-First-Out) commodity lot matching for cryptocurrency and other commodity tracking. This feature automatically matches sales against purchase lots according to your selected accounting method.

## Usage

### Command Line Options

Use the `--lot-matching` option to enable automatic lot matching:

```bash
# Use FIFO matching (oldest lots first)
ledger --lot-matching=fifo -f journal.dat balance

# Use LIFO matching (newest lots first)  
ledger --lot-matching=lifo -f journal.dat balance

# Disable automatic matching (default)
ledger --lot-matching=none -f journal.dat balance
```

### Journal File Directives

You can also set the policy in your journal file:

```
@lot matching fifo
```

## How It Works

### Purchase Transactions

When you purchase a commodity, specify the lot details using annotations:

```ledger
2020/01/15 Purchase Bitcoin - Lot A
    Assets:Crypto         2 BTC [2020/01/15] @ $8000
    Assets:USD           -$16000
```

The `[date]` annotation records the acquisition date, and `@ $price` records the per-unit cost.

### Sale Transactions

With automatic lot matching enabled, you can write simple sale transactions:

```ledger
2020/06/01 Sell Bitcoin
    Assets:Crypto        -1 BTC
    Assets:USD            9000 USD
```

The system automatically:
1. Identifies this as a sale (negative unannotated amount)
2. Matches against existing lots using your selected policy (FIFO/LIFO)
3. Calculates gain/loss based on the matched lot's cost basis
4. Tracks the remaining lot balance

### Viewing Lot Details

Use the `--lots` family of options to see lot breakdowns:

```bash
# Show all lots with purchase prices and dates
ledger --lots balance Assets:Crypto

# Show lots in FIFO order (oldest first)
ledger --lots-fifo balance Assets:Crypto

# Show lots in LIFO order (newest first)
ledger --lots-lifo balance Assets:Crypto
```

## Examples

### Example 1: FIFO Matching

**Journal:**
```ledger
@lot matching fifo

2020/01/15 Buy BTC Lot A
    Assets:Crypto    2 BTC [2020/01/15] @$8000
    Assets:USD      -$16000

2020/03/20 Buy BTC Lot B
    Assets:Crypto    1 BTC [2020/03/20] @$9000
    Assets:USD       -$9000

2020/06/01 Sell BTC
    Assets:Crypto   -1 BTC
    Assets:USD       9500
```

**Result:** The sale matches against Lot A (oldest), realizing a gain of $1500 ($9500 - $8000).

### Example 2: LIFO Matching

Same transactions with `@lot matching lifo`:

**Result:** The sale matches against Lot B (newest), realizing a gain of $500 ($9500 - $9000).

## Tax Implications

Different jurisdictions have different requirements for cost basis calculation:

- **FIFO**: Often required for traditional securities in some countries
- **LIFO**: May provide tax advantages in inflationary environments
- **Specific identification**: Still supported via explicit lot annotations

Consult a tax professional for your specific situation.

## Technical Details

### Lot Annotations

Lots are tracked using annotations:
- `{price}` - Purchase price per unit
- `[date]` - Acquisition date
- `(tag)` - Optional lot identifier

### Balance Assertions

You can assert lot balances:

```ledger
= account Assets:Crypto
  assert balance == 2 BTC { $8000 } [2020/01/15]
```

### Integration

The lot matching integrates with:
- Balance assertions
- Gain/loss calculations
- Commodity exchange tracking
- Report generation

## Implementation Notes

- Lot matching is session-wide (applies to all commodities)
- Only affects unannotated sales (explicit annotations take precedence)
- Partial lot sales are supported
- Remaining lot balances are automatically tracked

## See Also

- `--lots` - Display lot details
- `--lots-fifo` - Sort by FIFO order
- `--lots-lifo` - Sort by LIFO order
- `--average-lot-prices` - Show weighted average cost

## History

Issue #164 - Request for automatic FIFO/LIFO commodity lot matching.