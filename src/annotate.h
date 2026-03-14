/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * @addtogroup math
 */

/**
 * @file   annotate.h
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief  Types for annotating commodities
 *
 * Types for annotating commodities with lot prices, acquisition dates,
 * tags, and valuation expressions.  Annotations distinguish lots of the
 * same commodity purchased at different prices or times -- for example,
 * two purchases of AAPL at $150 and $185 produce two distinct annotated
 * commodities that can be tracked independently for capital gains.
 *
 * The annotation system has three main types:
 *   - annotation_t: holds the lot metadata (price, date, tag, value_expr)
 *     along with flags indicating which fields were computed vs. explicit.
 *   - keep_details_t: a filter specifying which annotation fields to
 *     preserve when stripping annotations for display or comparison.
 *   - annotated_commodity_t: a commodity_t subclass that pairs a base
 *     commodity with an annotation, registered in the pool's
 *     annotated_commodities map for identity-based deduplication.
 */
#pragma once

#include "expr.h"

namespace ledger {

/**
 * @brief Lot annotation metadata attached to a commodity.
 *
 * An annotation records the circumstances under which a commodity lot was
 * acquired: the per-unit purchase price, the acquisition date, an optional
 * free-form tag, and an optional valuation expression.  In journal syntax
 * these appear after the amount quantity:
 *
 *     10 AAPL {$150.00} [2024-01-15] (lot1) ((market(amount, date, t)))
 *             ^price      ^date       ^tag    ^value_expr
 *
 * Each field may be either user-supplied or computed by Ledger (indicated
 * by the *_CALCULATED flags).  Two annotations are semantically equal
 * when all four fields match and the ANNOTATION_SEMANTIC_FLAGS agree.
 */
struct annotation_t : public flags::supports_flags<>, public equality_comparable<annotation_t> {
#define ANNOTATION_PRICE_CALCULATED                                                                \
  0x01 ///< Price was computed from a cost expression, not user-supplied.
#define ANNOTATION_PRICE_FIXATED 0x02 ///< Price is fixated ({=...}), not a market price.
#define ANNOTATION_PRICE_NOT_PER_UNIT                                                              \
  0x04 ///< Price was given as total cost (@@), stored per-unit internally.
#define ANNOTATION_DATE_CALCULATED                                                                 \
  0x08 ///< Date was derived from the transaction date, not explicit.
#define ANNOTATION_TAG_CALCULATED 0x10 ///< Tag was computed, not user-supplied.
#define ANNOTATION_VALUE_EXPR_CALCULATED                                                           \
  0x20 ///< Value expression was computed (e.g., by nail_down).

/// Mask for flags that affect semantic equality of annotations.
/// Currently only ANNOTATION_PRICE_FIXATED matters: a fixated lot at {=$5}
/// is distinct from a floating lot at {$5} even if the numeric price matches.
#define ANNOTATION_SEMANTIC_FLAGS (ANNOTATION_PRICE_FIXATED)

  std::optional<amount_t> price;    ///< Per-unit lot price (e.g., $150.00 per share).
  std::optional<date_t> date;       ///< Acquisition date of the lot.
  std::optional<string> tag;        ///< Free-form tag for lot identification.
  std::optional<expr_t> value_expr; ///< Custom valuation expression overriding find_price.

  explicit annotation_t(const std::optional<amount_t>& _price = {},
                        const std::optional<date_t>& _date = {},
                        const std::optional<string>& _tag = {},
                        const std::optional<expr_t>& _value_expr = {})
      : supports_flags<>(), price(_price), date(_date), tag(_tag), value_expr(_value_expr) {
    TRACE_CTOR(annotation_t, "optional<amount_t> + date_t + string + expr_t");
  }
  annotation_t(const annotation_t& other)
      : supports_flags<>(other.flags()), price(other.price), date(other.date), tag(other.tag),
        value_expr(other.value_expr) {
    TRACE_CTOR(annotation_t, "copy");
  }
  annotation_t& operator=(const annotation_t&) = default;
  ~annotation_t() { TRACE_DTOR(annotation_t); }

  operator bool() const { return price || date || tag || value_expr; }

  bool operator<(const annotation_t& rhs) const;
  bool operator==(const annotation_t& rhs) const {
    return (price == rhs.price && date == rhs.date && tag == rhs.tag &&
            (value_expr && rhs.value_expr ? value_expr->text() == rhs.value_expr->text()
                                          : value_expr == rhs.value_expr) &&
            (flags() & ANNOTATION_SEMANTIC_FLAGS) == (rhs.flags() & ANNOTATION_SEMANTIC_FLAGS));
  }

  /**
   * @brief Parse annotation fields from an input stream.
   *
   * Reads zero or more annotation components in any order:
   *   - `{price}` or `{{total_price}}` -- lot price (single or total cost)
   *   - `{=price}` -- fixated lot price
   *   - `[date]` -- acquisition date
   *   - `(tag)` -- free-form lot tag
   *   - `((expr))` -- valuation expression
   *
   * Parsing stops when a character that cannot start an annotation is seen
   * (or at `(@` which begins a different construct).
   */
  void parse(std::istream& in);

  /**
   * @brief Render annotation fields back to their textual representation.
   *
   * Outputs only the fields that are present and not suppressed by the
   * @p no_computed_annotations flag.  When ANNOTATION_PRICE_NOT_PER_UNIT
   * is set and a quantity is available, the total-cost form `{{...}}` is
   * reconstructed by multiplying the stored per-unit price by abs(qty).
   */
  void print(std::ostream& out, bool keep_base = false, bool no_computed_annotations = false,
             const amount_t* qty = nullptr, uint_least8_t print_flags = 0) const;

  bool valid() const {
    assert(*this);
    return true;
  }
};

void put_annotation(property_tree::ptree& pt, const annotation_t& details);

/**
 * @brief Controls which annotation details survive stripping.
 *
 * When Ledger needs to simplify commodities for display or comparison
 * (e.g., the --lots, --lot-prices, --lot-dates, --lot-tags options),
 * a keep_details_t specifies which annotation fields to retain.  The
 * only_actuals flag further restricts retention to user-supplied
 * annotations, discarding those that Ledger computed automatically.
 */
struct keep_details_t {
  bool keep_price;   ///< Retain the lot price annotation.
  bool keep_date;    ///< Retain the lot date annotation.
  bool keep_tag;     ///< Retain the lot tag annotation.
  bool only_actuals; ///< If true, discard computed (*_CALCULATED) annotations even if kept.

  explicit keep_details_t(bool _keep_price = false, bool _keep_date = false, bool _keep_tag = false,
                          bool _only_actuals = false)
      : keep_price(_keep_price), keep_date(_keep_date), keep_tag(_keep_tag),
        only_actuals(_only_actuals) {
    TRACE_CTOR(keep_details_t, "bool, bool, bool, bool");
  }
  keep_details_t(const keep_details_t& other)
      : keep_price(other.keep_price), keep_date(other.keep_date), keep_tag(other.keep_tag),
        only_actuals(other.only_actuals) {
    TRACE_CTOR(keep_details_t, "copy");
  }
  keep_details_t& operator=(const keep_details_t&) = default;
  ~keep_details_t() noexcept { TRACE_DTOR(keep_details_t); }

  bool keep_all() const { return keep_price && keep_date && keep_tag && !only_actuals; }
  bool keep_all(const commodity_t& comm) const;

  bool keep_any() const { return keep_price || keep_date || keep_tag; }
  bool keep_any(const commodity_t& comm) const;
};

inline std::ostream& operator<<(std::ostream& out, const annotation_t& details) {
  details.print(out);
  return out;
}

/**
 * @brief A commodity with attached lot annotation information.
 *
 * An annotated_commodity_t wraps a base commodity_t (pointed to by ptr)
 * and adds lot-specific metadata via its `details` member.  It shares
 * the base commodity's base_t (symbol, precision, flags) but overrides
 * find_price() to consult fixated prices and valuation expressions
 * before falling through to the price history graph.
 *
 * Annotated commodities are deduplicated: two amounts with the same base
 * symbol and identical annotations point to the same annotated_commodity_t
 * instance in the pool's annotated_commodities map.
 */
class annotated_commodity_t
    : public commodity_t,
      public equality_comparable<
          annotated_commodity_t,
          equality_comparable2<annotated_commodity_t, commodity_t, noncopyable>> {
protected:
  friend class commodity_pool_t;

  commodity_t* ptr; ///< The underlying unannotated base commodity.

  explicit annotated_commodity_t(commodity_t* _ptr, const annotation_t& _details)
      : commodity_t(_ptr->parent_, _ptr->base), ptr(_ptr), details(_details) {
    annotated = true;
    qualified_symbol = _ptr->qualified_symbol;
    TRACE_CTOR(annotated_commodity_t, "commodity_t *, annotation_t");
  }

public:
  annotation_t details; ///< The lot annotation (price, date, tag, value_expr).

  ~annotated_commodity_t() override { TRACE_DTOR(annotated_commodity_t); }

  bool operator==(const commodity_t& comm) const override;
  virtual bool operator==(const annotated_commodity_t& comm) const {
    return *this == static_cast<const commodity_t&>(comm);
  }

  commodity_t& referent() override { return *ptr; }
  const commodity_t& referent() const override { return *ptr; }

  std::optional<expr_t> value_expr() const override {
    if (details.value_expr)
      return details.value_expr;
    return commodity_t::value_expr();
  }

  /**
   * @brief Price lookup with fixated-price and valuation-expression support.
   *
   * If the annotation has a fixated price ({=...}), that price is returned
   * immediately regardless of market conditions.  Otherwise, the price
   * commodity is used to set the target (if none was specified), and any
   * valuation expression is evaluated before falling through to the base
   * commodity_t::find_price() graph lookup.
   */
  std::optional<price_point_t> find_price(const commodity_t* commodity = nullptr,
                                          const datetime_t& moment = datetime_t(),
                                          const datetime_t& oldest = datetime_t()) const override;

  /**
   * @brief Selectively remove annotation fields based on a keep_details_t filter.
   *
   * Returns a new commodity with only the annotation fields specified by
   * @p what_to_keep.  If only_actuals is set, computed annotations
   * (*_CALCULATED) are also discarded.  If no annotation fields survive,
   * the unannotated base commodity is returned.  The result is always
   * obtained via find_or_create to ensure pool deduplication.
   */
  commodity_t& strip_annotations(const keep_details_t& what_to_keep) override;

  void print(std::ostream& out, bool elide_quotes = false,
             bool print_annotations = false) const override {
    if (print_annotations) {
      std::ostringstream buf;
      commodity_t::print(buf, elide_quotes);
      write_annotations(buf);
      out << buf.str();
    } else {
      commodity_t::print(out, elide_quotes);
    }
  }

  void write_annotations(std::ostream& out, bool no_computed_annotations = false,
                         const amount_t* qty = nullptr,
                         uint_least8_t print_flags = 0) const override;
};

inline annotated_commodity_t& as_annotated_commodity(commodity_t& commodity) {
  return downcast<annotated_commodity_t>(commodity);
}
inline const annotated_commodity_t& as_annotated_commodity(const commodity_t& commodity) {
  return downcast<const annotated_commodity_t>(commodity);
}

} // namespace ledger
