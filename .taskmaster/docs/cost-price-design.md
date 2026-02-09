# Design Document: Splitting annotation_t Price into Acquisition Cost and Price

**Status:** Draft
**Author:** (generated)
**Date:** 2026-02-09
**Affects:** `src/annotate.h`, `src/annotate.cc`, `src/pool.cc`, `src/xact.cc`, `src/textual.cc`, `src/amount.cc`, `src/commodity.cc`, `src/report.h`, `src/post.h`, `src/py_commodity.cc`

---

## 1. Current State

### 1.1 The annotation_t Structure

The `annotation_t` struct (defined in `src/annotate.h:52-98`) associates metadata with a commodity to create an annotated commodity. Its current fields are:

```cpp
struct annotation_t : public flags::supports_flags<>,
                      public equality_comparable<annotation_t> {
    optional<amount_t> price;       // line 63
    optional<date_t>   date;        // line 64
    optional<string>   tag;         // line 65
    optional<expr_t>   value_expr;  // line 66
};
```

### 1.2 Annotation Flags

Six flags are defined on `annotation_t`:

| Flag | Value | Meaning |
|------|-------|---------|
| `ANNOTATION_PRICE_CALCULATED` | 0x01 | The price was computed during finalization, not specified by the user. |
| `ANNOTATION_PRICE_FIXATED` | 0x02 | The price is a fixed acquisition cost (`{=amount}`), not a floating market indicator. Short-circuits all price lookups. |
| `ANNOTATION_PRICE_NOT_PER_UNIT` | 0x04 | The price was specified as a total (`{{amount}}`), not per-unit. |
| `ANNOTATION_DATE_CALCULATED` | 0x08 | The lot date was computed, not user-specified. |
| `ANNOTATION_TAG_CALCULATED` | 0x10 | The lot tag was computed, not user-specified. |
| `ANNOTATION_VALUE_EXPR_CALCULATED` | 0x20 | The value expression was computed. |

The `ANNOTATION_SEMANTIC_FLAGS` mask is defined as `ANNOTATION_PRICE_FIXATED` only. This mask controls which flags participate in commodity identity comparisons (via `operator==` and `operator<` on `annotation_t`, and in `commodity.cc:484`). Two annotated commodities with the same price but differing fixation status are considered different commodities.

### 1.3 The post_t Cost Fields

`post_t` (defined in `src/post.h:51-223`) maintains its own cost-related fields that are distinct from annotation prices:

```cpp
amount_t           amount;          // line 69 -- the posting amount
optional<amount_t> cost;            // line 71 -- computed total cost
optional<amount_t> given_cost;      // line 72 -- original cost as written by user
```

Relevant `post_t` flags:

| Flag | Value | Meaning |
|------|-------|---------|
| `POST_COST_CALCULATED` | 0x0080 | Cost was auto-computed from two-commodity balancing. |
| `POST_COST_IN_FULL` | 0x0100 | Cost specified with `@@` (total cost, not per-unit). |
| `POST_COST_FIXATED` | 0x0200 | Cost was specified with `@= amount` (fixed cost). |
| `POST_COST_VIRTUAL` | 0x0400 | Cost was specified with `(@)` (virtual cost). |

### 1.4 Current Parsing Rules

**Annotation price parsing** (`annotate.cc:101-206`):

| Syntax | Result |
|--------|--------|
| `{price}` | Sets `annotation_t::price`, no special flags. |
| `{=price}` | Sets `annotation_t::price` + `ANNOTATION_PRICE_FIXATED`. |
| `{{price}}` | Sets `annotation_t::price` + `ANNOTATION_PRICE_NOT_PER_UNIT`. |
| `{{=price}}` | Sets `annotation_t::price` + both `ANNOTATION_PRICE_FIXATED` and `ANNOTATION_PRICE_NOT_PER_UNIT`. |

**Posting cost parsing** (`textual.cc:1557-1634`):

| Syntax | Result |
|--------|--------|
| `@ cost` | Sets `post_t::cost` (per-unit, multiplied by amount). |
| `@@ cost` | Sets `post_t::cost` + `POST_COST_IN_FULL`. |
| `@ =cost` | Sets `post_t::cost` + `POST_COST_FIXATED`. |
| `(@cost)` | Sets `post_t::cost` + `POST_COST_VIRTUAL`. |

After parsing cost, `post_t::given_cost` is set to `post_t::cost` (`textual.cc:1620`).

**Fixed rate application** (`textual.cc:1536-1549`): When a `fixed` rate directive is active and a posting's amount has no annotation, an annotation is created with the fixed rate price and `ANNOTATION_PRICE_FIXATED` is set.

### 1.5 The Semantic Overload Problem

The single `annotation_t::price` field is overloaded to mean two fundamentally different things:

1. **Acquisition cost** (lot cost): The price at which a commodity was acquired, used for cost-basis tracking. Written as `{$100}` or `{=$100}`. When fixated, this is a permanent property of the lot that should not change with market conditions.

2. **Market/exchange price**: A per-unit conversion rate computed during transaction finalization. When a two-commodity transaction balances, the implied exchange rate is stored as the annotation price with `ANNOTATION_PRICE_CALCULATED`. This price may be used as a hint for price lookups but does not represent a fixed cost basis.

The fixated flag is the only mechanism that distinguishes these two meanings, which leads to complex conditional logic spread across seven source files.

---

## 2. Proposed Structure

### 2.1 New annotation_t Definition

```cpp
struct annotation_t : public flags::supports_flags<>,
                      public equality_comparable<annotation_t> {
#define ANNOTATION_COST_CALCULATED     0x01
#define ANNOTATION_COST_FIXATED        0x02
#define ANNOTATION_COST_NOT_PER_UNIT   0x04
#define ANNOTATION_DATE_CALCULATED     0x08
#define ANNOTATION_TAG_CALCULATED      0x10
#define ANNOTATION_VALUE_EXPR_CALCULATED 0x20
#define ANNOTATION_PRICE_CALCULATED    0x40

#define ANNOTATION_SEMANTIC_FLAGS (ANNOTATION_COST_FIXATED)

    optional<amount_t> acquisition_cost;  // from {cost} / {=fixated}
    optional<amount_t> price;             // from @ price (exchange rate)
    optional<date_t>   date;
    optional<string>   tag;
    optional<expr_t>   value_expr;
};
```

### 2.2 Field Semantics

**`acquisition_cost`** (formerly the sole `price` field for `{...}` syntax):
- Represents the cost basis of a commodity lot.
- Set by `{amount}` and `{=amount}` syntax during annotation parsing.
- When `ANNOTATION_COST_FIXATED` is set, the cost basis is permanent and overrides all market price lookups for valuation.
- When `ANNOTATION_COST_NOT_PER_UNIT` is set (from `{{amount}}`), the cost is for the total lot, not per unit.
- When `ANNOTATION_COST_CALCULATED` is set, the cost was computed during finalization, not explicitly written.

**`price`** (new field for `@` syntax exchange rates):
- Represents the exchange rate used in a posting, stored on the annotation for reference.
- Currently, the exchange rate from `@` syntax is only stored on `post_t::cost`; in the new model, it is additionally stored on the annotation when the amount is re-annotated during `commodity_pool_t::exchange()`.
- When `ANNOTATION_PRICE_CALCULATED` is set, the price was auto-computed during finalization rather than written by the user.
- This field does NOT participate in commodity identity comparisons. Two lots at the same acquisition cost but different exchange prices are the same annotated commodity.

---

## 3. Relationship Between Fields

### 3.1 Source-of-Truth Matrix

| User writes | `annotation_t::acquisition_cost` | `annotation_t::price` | `post_t::cost` | `post_t::given_cost` |
|-------------|----------------------------------|----------------------|----------------|---------------------|
| `10 AAPL {$100}` | `$100` (per-unit) | (none) | (none, unless `@` also present) | (none) |
| `10 AAPL {=$100}` | `$100` (per-unit, fixated) | (none) | (none, unless `@` also present) | (none) |
| `10 AAPL {{$1000}}` | `$1000` (total, `COST_NOT_PER_UNIT`) | (none) | (none) | (none) |
| `10 AAPL @ $105` | (none initially; set to `$105` by `exchange()`) | `$105` (per-unit, set by `exchange()`) | `$1050` (total) | `$1050` |
| `10 AAPL {$100} @ $105` | `$100` (per-unit, user-specified) | `$105` (per-unit, set by `exchange()`) | `$1050` (total) | `$1050` |
| `10 AAPL {=$100} @ $105` | `$100` (per-unit, fixated) | `$105` (per-unit, set by `exchange()`) | `$1050` (total) | `$1050` |
| Two-commodity balance (no `@`) | (set to computed per-unit cost by `exchange()`, with `COST_CALCULATED`) | (set to same value, with `PRICE_CALCULATED`) | computed total | (none) |

### 3.2 Priority for Valuation

When computing the value of a commodity holding (in `amount_t::value()` and `annotated_commodity_t::find_price()`):

1. If `acquisition_cost` is present and `ANNOTATION_COST_FIXATED` is set: return the fixated cost. No market lookup occurs.
2. If `acquisition_cost` is present (not fixated): use the acquisition cost's commodity as a hint for the target commodity in price lookups, but look up the current market price.
3. If `price` is present (and no fixated cost): use the price's commodity as a hint for target commodity lookups.
4. Otherwise: perform a standard market price lookup.

### 3.3 Priority for Cost Basis Computation

When computing gain/loss in `xact_t::finalize()`:

- `basis_cost` is derived from `acquisition_cost` if present, otherwise from `price`.
- `final_cost` comes from `post_t::cost` (the `@` amount).
- Gain/loss = `basis_cost - final_cost`.

### 3.4 Commodity Identity

Two annotated commodities are considered identical if:

- Same base commodity.
- Same `acquisition_cost` value (compared by value).
- Same `date`, `tag`, and `value_expr`.
- Same `ANNOTATION_SEMANTIC_FLAGS` (specifically `ANNOTATION_COST_FIXATED`).

The `price` field does NOT participate in commodity identity. This is consistent with the current behavior where calculated prices (with `ANNOTATION_PRICE_CALCULATED`) do not affect commodity identity, but makes the distinction explicit and structural rather than flag-dependent.

---

## 4. Flag Changes

### 4.1 Renamed Flags

| Old Name | New Name | Value |
|----------|----------|-------|
| `ANNOTATION_PRICE_CALCULATED` | `ANNOTATION_COST_CALCULATED` | 0x01 |
| `ANNOTATION_PRICE_FIXATED` | `ANNOTATION_COST_FIXATED` | 0x02 |
| `ANNOTATION_PRICE_NOT_PER_UNIT` | `ANNOTATION_COST_NOT_PER_UNIT` | 0x04 |

### 4.2 New Flag

| Name | Value | Purpose |
|------|-------|---------|
| `ANNOTATION_PRICE_CALCULATED` | 0x40 | Indicates the `price` field was auto-computed during finalization. |

### 4.3 Backward-Compatibility Aliases (Phase 1)

During Phase 1 (see Section 8), the old flag names are retained as aliases:

```cpp
#define ANNOTATION_PRICE_FIXATED       ANNOTATION_COST_FIXATED       // deprecated alias
#define ANNOTATION_PRICE_NOT_PER_UNIT  ANNOTATION_COST_NOT_PER_UNIT  // deprecated alias
```

These aliases are removed in Phase 2.

### 4.4 ANNOTATION_SEMANTIC_FLAGS Update

```cpp
// Before:
#define ANNOTATION_SEMANTIC_FLAGS (ANNOTATION_PRICE_FIXATED)

// After:
#define ANNOTATION_SEMANTIC_FLAGS (ANNOTATION_COST_FIXATED)
```

The semantic meaning is identical; only the name changes.

### 4.5 Commodity-Level Flags

The flags on `commodity_t::base_t` that track annotation characteristics remain structurally the same but are renamed for clarity:

| Old Name | New Name |
|----------|----------|
| `COMMODITY_SAW_ANN_PRICE_FLOAT` | `COMMODITY_SAW_ANN_COST_FLOAT` |
| `COMMODITY_SAW_ANN_PRICE_FIXATED` | `COMMODITY_SAW_ANN_COST_FIXATED` |

### 4.6 Post-Level Flags

The `post_t` flags (`POST_COST_FIXATED`, `POST_COST_CALCULATED`, `POST_COST_IN_FULL`, `POST_COST_VIRTUAL`) are unchanged. They continue to describe properties of `post_t::cost`, which is a separate concept from annotation fields.

---

## 5. Parser Changes

### 5.1 annotation_t::parse() (annotate.cc:101-206)

The `{...}` parsing block (lines 109-144) changes the target field from `price` to `acquisition_cost`:

**Before:**
```cpp
if (price)
    throw_(amount_error, _("Commodity specifies more than one price"));
// ...
price = temp;
```

**After:**
```cpp
if (acquisition_cost)
    throw_(amount_error, _("Commodity specifies more than one acquisition cost"));
// ...
acquisition_cost = temp;
```

All flag references within this block change from `ANNOTATION_PRICE_*` to `ANNOTATION_COST_*`. The parser behavior is otherwise identical.

### 5.2 annotation_t::print() (annotate.cc:208-221)

The `{...}` output block changes from reading `price` to reading `acquisition_cost`:

**Before:**
```cpp
if (price && (!no_computed_annotations || !has_flags(ANNOTATION_PRICE_CALCULATED)))
    out << " {" << (has_flags(ANNOTATION_PRICE_FIXATED) ? "=" : "")
        << (keep_base ? *price : price->unreduced()) << '}';
```

**After:**
```cpp
if (acquisition_cost &&
    (!no_computed_annotations || !has_flags(ANNOTATION_COST_CALCULATED)))
    out << " {"
        << (has_flags(ANNOTATION_COST_FIXATED) ? "=" : "")
        << (keep_base ? *acquisition_cost : acquisition_cost->unreduced())
        << '}';
```

No new output syntax is introduced. The `@` price is not printed as part of the annotation because it belongs to `post_t::cost` in the output representation.

### 5.3 textual.cc Cost Parsing (lines 1557-1634)

The `@`/`@@` parsing code continues to populate `post_t::cost` and `post_t::given_cost` as before. No changes to this section are needed.

### 5.4 textual.cc Fixed Rate Application (lines 1536-1549)

The fixed-rate annotation creation changes from:

```cpp
annotation_t details(rate.second);
details.add_flags(ANNOTATION_PRICE_FIXATED);
```

To:

```cpp
annotation_t details;
details.acquisition_cost = rate.second;
details.add_flags(ANNOTATION_COST_FIXATED);
```

This requires updating the `annotation_t` constructor call since the positional constructor argument previously mapped to `price`.

### 5.5 put_annotation() (annotate.cc:223-235)

The XML/property-tree serialization updates to emit both fields:

```cpp
void put_annotation(property_tree::ptree& st, const annotation_t& details) {
    if (details.acquisition_cost)
        put_amount(st.put("acquisition-cost", ""), *details.acquisition_cost);
    if (details.price)
        put_amount(st.put("price", ""), *details.price);
    // date, tag, value_expr unchanged
}
```

---

## 6. keep_details_t Changes

### 6.1 New Field

```cpp
struct keep_details_t {
    bool keep_cost;        // NEW: controls retention of acquisition_cost
    bool keep_price;       // controls retention of price (exchange rate)
    bool keep_date;
    bool keep_tag;
    bool only_actuals;

    explicit keep_details_t(
        bool _keep_cost  = false,
        bool _keep_price = false,
        bool _keep_date  = false,
        bool _keep_tag   = false,
        bool _only_actuals = false);
};
```

### 6.2 Updated keep_all() and keep_any()

```cpp
bool keep_all() const {
    return keep_cost && keep_price && keep_date && keep_tag && !only_actuals;
}

bool keep_any() const {
    return keep_cost || keep_price || keep_date || keep_tag;
}
```

### 6.3 Updated strip_annotations()

In `annotated_commodity_t::strip_annotations()` (annotate.cc:302-345), the logic splits into separate retention decisions for `acquisition_cost` and `price`:

```cpp
bool do_keep_cost =
    (what_to_keep.keep_cost &&
     (!what_to_keep.only_actuals ||
      !details.has_flags(ANNOTATION_COST_CALCULATED)));
bool do_keep_price =
    (what_to_keep.keep_price &&
     (!what_to_keep.only_actuals ||
      !details.has_flags(ANNOTATION_PRICE_CALCULATED)));
```

When constructing the new annotation for the stripped commodity:

```cpp
annotation_t new_ann(
    do_keep_cost  ? details.acquisition_cost : none,
    do_keep_price ? details.price : none,
    do_keep_date  ? details.date : none,
    do_keep_tag   ? details.tag : none);
```

Flag transfer logic splits accordingly:

```cpp
if (do_keep_cost)
    new_details.add_flags(details.flags() &
        (ANNOTATION_COST_CALCULATED | ANNOTATION_COST_FIXATED));
if (do_keep_price)
    new_details.add_flags(details.flags() & ANNOTATION_PRICE_CALCULATED);
```

### 6.4 Updated report.h what_to_keep()

```cpp
keep_details_t what_to_keep() {
    bool lots = HANDLED(lots) || HANDLED(lots_actual);
    return keep_details_t(
        lots || HANDLED(lot_prices),  // keep_cost (lot_prices controls cost retention)
        lots || HANDLED(lot_prices),  // keep_price (follows same option)
        lots || HANDLED(lot_dates),   // keep_date
        lots || HANDLED(lot_notes),   // keep_tag
        HANDLED(lots_actual));        // only_actuals
}
```

Note: In the initial implementation, `keep_cost` and `keep_price` are both controlled by the existing `--lot-prices` option. A future enhancement could introduce `--lot-costs` to separate them. This preserves backward compatibility of command-line behavior.

---

## 7. Impact Analysis: ANNOTATION_PRICE_FIXATED Usage Sites

Each of the 15 known usage sites of `ANNOTATION_PRICE_FIXATED` is described below, along with the required change.

### 7.1 annotate.h:54 -- Flag Definition

```cpp
// Before:
#define ANNOTATION_PRICE_FIXATED 0x02

// After:
#define ANNOTATION_COST_FIXATED 0x02
// Phase 1 only: #define ANNOTATION_PRICE_FIXATED ANNOTATION_COST_FIXATED
```

### 7.2 annotate.h:61 -- ANNOTATION_SEMANTIC_FLAGS

```cpp
// Before:
#define ANNOTATION_SEMANTIC_FLAGS (ANNOTATION_PRICE_FIXATED)

// After:
#define ANNOTATION_SEMANTIC_FLAGS (ANNOTATION_COST_FIXATED)
```

No behavioral change. The fixation of cost basis continues to define commodity identity.

### 7.3 annotate.cc:123 -- parse(), fixated cost flag

```cpp
// Before:
add_flags(ANNOTATION_PRICE_FIXATED);

// After:
add_flags(ANNOTATION_COST_FIXATED);
```

Within the `{...}` parsing block, where `{=...}` is detected. The flag is set on the same annotation; only the name changes.

### 7.4 annotate.cc:210 -- print(), fixated indicator output

```cpp
// Before:
out << " {" << (has_flags(ANNOTATION_PRICE_FIXATED) ? "=" : "")

// After:
out << " {" << (has_flags(ANNOTATION_COST_FIXATED) ? "=" : "")
```

Reads `acquisition_cost` instead of `price` (see Section 5.2).

### 7.5 annotate.cc:282 -- find_price(), fixated short-circuit

```cpp
// Before:
if (details.has_flags(ANNOTATION_PRICE_FIXATED)) {
    return price_point_t(when, *details.price);
}

// After:
if (details.has_flags(ANNOTATION_COST_FIXATED)) {
    return price_point_t(when, *details.acquisition_cost);
}
```

This is the central valuation short-circuit. When the cost is fixated, it returns the `acquisition_cost` directly as the price point, bypassing all market lookups.

### 7.6 annotate.cc:334 -- strip_annotations(), flag transfer

```cpp
// Before:
new_details.add_flags(details.flags() &
    (ANNOTATION_PRICE_CALCULATED | ANNOTATION_PRICE_FIXATED));

// After:
new_details.add_flags(details.flags() &
    (ANNOTATION_COST_CALCULATED | ANNOTATION_COST_FIXATED));
```

See Section 6.3 for the full strip_annotations() restructuring.

### 7.7 pool.cc:181 -- create(), commodity flag propagation

```cpp
// Before:
if (details.has_flags(ANNOTATION_PRICE_FIXATED))
    comm.add_flags(COMMODITY_SAW_ANN_PRICE_FIXATED);
else
    comm.add_flags(COMMODITY_SAW_ANN_PRICE_FLOAT);

// After:
if (details.has_flags(ANNOTATION_COST_FIXATED))
    comm.add_flags(COMMODITY_SAW_ANN_COST_FIXATED);
else
    comm.add_flags(COMMODITY_SAW_ANN_COST_FLOAT);
```

This logic tracks whether any annotated variant of a base commodity has been created with fixated vs. floating costs. The check changes to reference `acquisition_cost` presence:

```cpp
if (details.acquisition_cost) {
    if (details.has_flags(ANNOTATION_COST_FIXATED))
        comm.add_flags(COMMODITY_SAW_ANN_COST_FIXATED);
    else
        comm.add_flags(COMMODITY_SAW_ANN_COST_FLOAT);
}
```

### 7.8 pool.cc:249 -- exchange(), skip price recording for fixated

```cpp
// Before:
!(current_annotation->price &&
  current_annotation->has_flags(ANNOTATION_PRICE_FIXATED))

// After:
!(current_annotation->acquisition_cost &&
  current_annotation->has_flags(ANNOTATION_COST_FIXATED))
```

When a commodity has a fixated acquisition cost, its exchange transactions should not update the market price database. This continues to check the `acquisition_cost` field rather than `price`.

### 7.9 pool.cc:271-272 -- exchange(), fixated flag propagation

```cpp
// Before:
if (current_annotation && current_annotation->has_flags(ANNOTATION_PRICE_FIXATED))
    annotation.add_flags(ANNOTATION_PRICE_FIXATED);

// After:
if (current_annotation && current_annotation->has_flags(ANNOTATION_COST_FIXATED))
    annotation.add_flags(ANNOTATION_COST_FIXATED);
```

When `exchange()` creates a new annotation for the exchanged amount, it propagates the fixated status from the existing annotation. The existing annotation's `ANNOTATION_PRICE_CALCULATED` flag is also set on the new annotation; in the new model, the new annotation records both:
- `acquisition_cost` from the existing annotation (if fixated).
- `price` from the per-unit-cost computed by exchange.

The exchange() function requires broader restructuring to populate both fields:

```cpp
annotation.acquisition_cost = current_annotation
    ? current_annotation->acquisition_cost
    : optional<amount_t>(per_unit_cost);
annotation.price = per_unit_cost;
annotation.add_flags(ANNOTATION_COST_CALCULATED);
annotation.add_flags(ANNOTATION_PRICE_CALCULATED);
if (current_annotation && current_annotation->has_flags(ANNOTATION_COST_FIXATED))
    annotation.add_flags(ANNOTATION_COST_FIXATED);
```

### 7.10 xact.cc:273 -- finalize(), fixated annotation to posting cost

```cpp
// Before:
post->amount.annotation().has_flags(ANNOTATION_PRICE_FIXATED)) {
    const annotation_t& ann(post->amount.annotation());
    post->cost = *ann.price;

// After:
post->amount.annotation().has_flags(ANNOTATION_COST_FIXATED)) {
    const annotation_t& ann(post->amount.annotation());
    post->cost = *ann.acquisition_cost;
```

This block in `finalize()` extracts the fixated annotation cost to create a posting cost when no explicit `@` cost was written. It reads `acquisition_cost` instead of `price`.

### 7.11 xact.cc:286 -- finalize(), POST_COST_FIXATED flag

```cpp
post->add_flags(POST_COST_FIXATED);
```

No change. `POST_COST_FIXATED` is a `post_t` flag and is already named correctly.

### 7.12 xact.cc:411 -- finalize(), sync fixated flag to annotation

```cpp
// Before:
if (post->has_flags(POST_COST_FIXATED) &&
    post->amount.has_annotation() &&
    post->amount.annotation().price) {
    post->amount.annotation().add_flags(ANNOTATION_PRICE_FIXATED);
}

// After:
if (post->has_flags(POST_COST_FIXATED) &&
    post->amount.has_annotation() &&
    post->amount.annotation().acquisition_cost) {
    post->amount.annotation().add_flags(ANNOTATION_COST_FIXATED);
}
```

This syncs the `POST_COST_FIXATED` flag from the posting level back to the annotation level. The check and target both change to use `acquisition_cost` and `ANNOTATION_COST_FIXATED`.

### 7.13 textual.cc:1542 -- fixed rate directive

```cpp
// Before:
annotation_t details(rate.second);
details.add_flags(ANNOTATION_PRICE_FIXATED);

// After:
annotation_t details;
details.acquisition_cost = rate.second;
details.add_flags(ANNOTATION_COST_FIXATED);
```

See Section 5.4.

### 7.14 amount.cc:714 -- value(), fixated short-circuit

```cpp
// Before:
if (annotation().has_flags(ANNOTATION_PRICE_FIXATED)) {
    point = price_point_t();
    point->price = *annotation().price;

// After:
if (annotation().has_flags(ANNOTATION_COST_FIXATED)) {
    point = price_point_t();
    point->price = *annotation().acquisition_cost;
```

This is the `amount_t::value()` method's fixated price short-circuit. It returns the fixated `acquisition_cost` as the valuation price, exactly as before.

### 7.15 commodity.cc:484 -- compare_by_commodity(), semantic flags

```cpp
// Before:
// Compare semantic flags (ANNOTATION_PRICE_FIXATED and ANNOTATION_PRICE_NOT_PER_UNIT)
unsigned int left_flags = aleftcomm.details.flags() & ANNOTATION_SEMANTIC_FLAGS;

// After:
// Compare semantic flags (ANNOTATION_COST_FIXATED)
unsigned int left_flags = aleftcomm.details.flags() & ANNOTATION_SEMANTIC_FLAGS;
```

The code is unchanged because `ANNOTATION_SEMANTIC_FLAGS` is already used as the mask. Only the comment needs updating.

---

## 8. Migration Strategy

### 8.1 Phase 1: Internal Refactor (Backward Compatible)

**Goal:** Rename the field and flags without changing any external behavior. All existing journal files, command-line options, and output formats remain identical.

**Steps:**

1. Add `acquisition_cost` field to `annotation_t`. Keep `price` field.
2. Define `ANNOTATION_COST_*` flags alongside `ANNOTATION_PRICE_*` as aliases:
   ```cpp
   #define ANNOTATION_COST_CALCULATED     ANNOTATION_PRICE_CALCULATED
   #define ANNOTATION_COST_FIXATED        ANNOTATION_PRICE_FIXATED
   #define ANNOTATION_COST_NOT_PER_UNIT   ANNOTATION_PRICE_NOT_PER_UNIT
   ```
3. Update `annotation_t::parse()` to populate `acquisition_cost` from `{...}` syntax.
4. Update all internal consumers to read `acquisition_cost` instead of `price` for lot cost operations.
5. During Phase 1, `price` on annotation_t continues to be populated as a mirror of `acquisition_cost` for any code not yet migrated. This is implemented via a compatibility accessor:
   ```cpp
   // Transitional: returns acquisition_cost if set, else price
   const optional<amount_t>& effective_price() const {
       return acquisition_cost ? acquisition_cost : price;
   }
   ```
6. Update `operator==`, `operator<`, `operator bool`, constructors, copy constructor.
7. Add `keep_cost` to `keep_details_t` and wire it alongside `keep_price`.
8. Ensure all tests pass with no output changes.

**CMake integration:**

```cmake
option(LEDGER_SPLIT_COST_PRICE
       "Enable separated acquisition cost and price fields in annotations"
       OFF)
```

When OFF, Phase 1 aliases are active and the old single-field behavior is preserved via `#ifdef` guards. When ON, the full two-field model is active.

### 8.2 Phase 2: Semantic Split

**Goal:** Complete the separation. The `price` field on `annotation_t` becomes exclusively an exchange rate populated by `commodity_pool_t::exchange()`. The `acquisition_cost` field is the sole container for `{...}` syntax values.

**Steps:**

1. Remove the backward-compatibility aliases.
2. Remove the `effective_price()` transitional accessor.
3. Update `commodity_pool_t::exchange()` to populate both `annotation.acquisition_cost` and `annotation.price` distinctly (see Section 7.9).
4. Update `annotation_t::operator bool` to check both fields:
   ```cpp
   operator bool() const {
       return acquisition_cost || price || date || tag || value_expr;
   }
   ```
5. Update `annotation_t::operator==` and `operator<` to compare `acquisition_cost` (not `price`) for commodity identity.
6. Remove the CMake feature flag; the split is now the permanent model.

### 8.3 Runtime Compatibility Flag

A runtime flag `--legacy-annotation-semantics` is provided during the transition period:

- When set, the old single-field behavior is emulated: `acquisition_cost` and `price` are treated as synonyms, and all reads go through `effective_price()`.
- This flag is intended for users who discover edge-case regressions and need an immediate workaround.
- The flag is deprecated from introduction and removed in the release following Phase 2 completion.

Implementation: a global boolean in `session_t` checked by `annotation_t::effective_price()`.

---

## 9. Backward Compatibility

### 9.1 Journal File Compatibility

All existing journal file syntax is unchanged:

- `{$100}` continues to set an acquisition cost (previously called annotation price).
- `{=$100}` continues to set a fixated acquisition cost.
- `{{$100}}` continues to set a total (not-per-unit) acquisition cost.
- `@ $105` continues to set a posting cost.
- `@@ $1050` continues to set a full posting cost.

No new syntax is introduced. Existing files parse and produce identical results.

### 9.2 Command-Line Option Compatibility

- `--lot-prices` continues to control retention of lot cost information in output.
- `--lots` continues to enable all lot detail retention.
- `--lots-actual` continues to filter out computed annotations.

### 9.3 Output Compatibility

The `{...}` annotation output format is unchanged. The `=` prefix for fixated costs is preserved. Report output (register, balance, print) produces identical results.

### 9.4 API Compatibility (Python Bindings)

The Python bindings in `src/py_commodity.cc` expose `keep_details_t` properties. The `keep_price` property continues to work. A new `keep_cost` property is added. The `KeepDetails` class gains the new attribute without removing any existing ones.

The `annotation_t` Python wrapper needs updating to expose `acquisition_cost` alongside `price`. The old `price` accessor continues to work via the transitional `effective_price()` method during Phase 1.

---

## 10. Testing Strategy

### 10.1 Existing Tests That Must Continue to Pass

All existing tests must produce identical output after the refactor:

- `test/baseline/feat-fixated-prices.test` -- exercises `{=$2}` fixated pricing.
- `test/baseline/feat-annotations.test` -- exercises `{...}` annotation parsing and display.
- `test/baseline/feat-commodity_swap.test` -- exercises cross-commodity exchange with annotations.
- `test/baseline/opt-price.test` -- exercises `-V` valuation.
- `test/baseline/opt-historical.test` -- exercises `--historical` pricing.
- `test/baseline/opt-base.test` -- exercises `--base` display.
- `test/baseline/opt-register-format.test` -- exercises register output formatting.
- `test/regress/712-a.test` -- fixated price regression.
- `test/regress/1454.test` -- annotation-related regression.
- `test/regress/2130.test` -- fixated price annotation display.
- `test/regress/2498.test` -- annotation regression.
- `test/regress/C0212EAC.test` -- annotation regression.
- `test/regress/CAE63F5C-b.test` and `CAE63F5C-c.test` -- annotation regressions.

### 10.2 New Unit Tests

Add to `test/unit/`:

1. **`test_annotation_cost_price_separation.cc`**: Verify that `annotation_t` correctly stores `acquisition_cost` and `price` independently.
   - Parse `{$100}` and verify `acquisition_cost` is set, `price` is not.
   - Parse `{=$100}` and verify `acquisition_cost` is set with `ANNOTATION_COST_FIXATED`.
   - Construct annotation with both fields and verify `operator==` only compares `acquisition_cost`.
   - Verify `operator<` ordering with split fields.

2. **`test_annotation_strip.cc`**: Verify `strip_annotations()` handles both fields correctly.
   - `keep_cost=true, keep_price=false` retains `acquisition_cost` only.
   - `keep_cost=false, keep_price=true` retains `price` only.
   - `only_actuals=true` strips calculated fields of both types.

### 10.3 New Regression Tests

Add to `test/regress/`:

1. **Cost-only annotation**: `10 AAPL {$100}` with `-V` should use market price, not lot cost.
2. **Fixated cost with market price**: `10 AAPL {=$100} @ $105` with `-V` should show $100 (fixated cost), not $105.
3. **Gain/loss with split fields**: Verify that gain/loss computation uses `acquisition_cost` for basis when both fields are present.
4. **Fixed rate directive**: Verify that `fixed` rate directives set `acquisition_cost` with `ANNOTATION_COST_FIXATED`.
5. **Two-commodity auto-balance**: Verify that auto-computed exchange rates populate the `price` field with `ANNOTATION_PRICE_CALCULATED`.

### 10.4 New Baseline Tests

Add to `test/baseline/`:

1. **`feat-cost-price-split.test`**: Comprehensive test of the split semantics, exercising all combinations from the source-of-truth matrix (Section 3.1).

---

## 11. Rollback Strategy

### 11.1 CMake Feature Flag

The `LEDGER_SPLIT_COST_PRICE` CMake option provides a compile-time gate. When set to `OFF` (the default), the old single-field behavior is compiled. This allows distributors and users to build without the new behavior if issues arise.

Implementation uses preprocessor guards:

```cpp
#ifdef LEDGER_SPLIT_COST_PRICE
    optional<amount_t> acquisition_cost;
    optional<amount_t> price;
#else
    optional<amount_t> price;  // legacy single-field
#endif
```

Internal code uses accessor macros during the transition:

```cpp
#ifdef LEDGER_SPLIT_COST_PRICE
#define ANN_COST(ann)  ((ann).acquisition_cost)
#define ANN_PRICE(ann) ((ann).price)
#else
#define ANN_COST(ann)  ((ann).price)
#define ANN_PRICE(ann) ((ann).price)
#endif
```

### 11.2 Runtime Flag

The `--legacy-annotation-semantics` flag (Section 8.3) provides runtime rollback without recompilation.

### 11.3 Git Reversion

Because Phase 1 is designed to produce no behavioral changes, reverting the Phase 1 commits produces a clean rollback. Phase 2 introduces behavioral changes and should be merged as a single, revertible commit or a clearly bounded set of commits on a feature branch.

### 11.4 Release Plan

- Phase 1 ships in the next minor release with the feature flag OFF by default.
- Phase 1 is tested with the feature flag ON in CI.
- Phase 2 ships in the subsequent minor release with the feature flag ON by default.
- The feature flag and `--legacy-annotation-semantics` are removed in the release after Phase 2.

---

## Appendix A: Full List of Files Requiring Modification

| File | Changes |
|------|---------|
| `src/annotate.h` | Struct fields, flag definitions, `keep_details_t`, constructor signatures |
| `src/annotate.cc` | `parse()`, `print()`, `operator<`, `operator==`, `put_annotation()`, `strip_annotations()`, `find_price()`, `keep_all()`, `keep_any()` |
| `src/pool.cc` | `create()` flag propagation, `exchange()` field population and fixated check |
| `src/xact.cc` | `finalize()` fixated-to-cost extraction, flag sync |
| `src/textual.cc` | Fixed rate directive annotation creation |
| `src/amount.cc` | `value()` fixated short-circuit, `parse()` NOT_PER_UNIT handling |
| `src/commodity.cc` | `compare_by_commodity()` comment update |
| `src/commodity.h` | `COMMODITY_SAW_ANN_*` flag renames |
| `src/report.h` | `what_to_keep()` updated constructor call |
| `src/post.h` | No changes (post_t flags remain as-is) |
| `src/py_commodity.cc` | Python binding updates for `keep_details_t` and `annotation_t` |
| `CMakeLists.txt` | New `LEDGER_SPLIT_COST_PRICE` option |

## Appendix B: Constructor Migration

The `annotation_t` constructor currently takes `optional<amount_t>` as the first positional argument, which maps to `price`:

```cpp
explicit annotation_t(const optional<amount_t>& _price = none, ...);
```

All call sites that construct `annotation_t` with a positional price argument must be audited. Known call sites:

1. `pool.cc:266` -- `annotation_t(per_unit_cost, ...)` in `exchange()`. This becomes a two-argument form: `annotation_t(acquisition_cost, per_unit_cost, ...)`.
2. `annotate.cc:325` -- `annotation_t(keep_price ? details.price : none, ...)` in `strip_annotations()`. This becomes `annotation_t(do_keep_cost ? details.acquisition_cost : none, do_keep_price ? details.price : none, ...)`.
3. `xact.cc:397` -- `annotation_t(breakdown.amount.annotation().price, ...)` in `finalize()`. This must be split to pass both fields from `breakdown.amount.annotation()`.

The new constructor signature:

```cpp
explicit annotation_t(
    const optional<amount_t>& _acquisition_cost = none,
    const optional<amount_t>& _price = none,
    const optional<date_t>&   _date = none,
    const optional<string>&   _tag = none,
    const optional<expr_t>&   _value_expr = none);
```
