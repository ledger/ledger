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
 * @file   annotate.cc
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief Implements commodity annotation parsing, printing, comparison,
 *        and the annotated commodity's price lookup and stripping logic.
 *
 * This file contains the runtime logic for annotation_t (parsing the
 * bracket syntax, rendering annotations back to text, and ordering them)
 * and for annotated_commodity_t (finding prices with fixated-vs-floating
 * semantics, and selectively removing annotations based on keep_details_t).
 */

#include <system.hh>

#include "amount.h"
#include "commodity.h"
#include "expr.h"
#include "annotate.h"
#include "pool.h"

namespace ledger {

/// @name annotation_t implementation
/// @{

/**
 * @brief Strict weak ordering for annotations.
 *
 * Annotations are ordered lexicographically by (price, date, tag, value_expr,
 * semantic flags).  Within each field, "absent" sorts before "present"; when
 * both are present, the field values are compared normally.  For prices the
 * commodity symbol is compared first (alphabetically), then the numeric value,
 * so that annotations in different currencies sort predictably.
 *
 * This ordering is the key comparator for the pool's annotated_commodities
 * map, ensuring that each unique annotation is stored exactly once.
 */
bool annotation_t::operator<(const annotation_t& rhs) const {
  if (!price && rhs.price)
    return true;
  if (price && !rhs.price)
    return false;
  if (!date && rhs.date)
    return true;
  if (date && !rhs.date)
    return false;
  if (!tag && rhs.tag)
    return true;
  if (tag && !rhs.tag)
    return false;

  if (!value_expr && rhs.value_expr)
    return true;
  if (value_expr && !rhs.value_expr)
    return false;

  if (price) {
    if (price->commodity().symbol() < rhs.price->commodity().symbol())
      return true;
    if (price->commodity().symbol() > rhs.price->commodity().symbol())
      return false;

    if (*price < *rhs.price)
      return true;
    if (*price > *rhs.price)
      return false;
  }
  if (date) {
    if (*date < *rhs.date)
      return true;
    if (*date > *rhs.date)
      return false;
  }
  if (tag) {
    if (*tag < *rhs.tag)
      return true;
    if (*tag > *rhs.tag)
      return false;
  }
  if (value_expr) {
    DEBUG("annotate.less",
          "Comparing (" << value_expr->text() << ") < (" << rhs.value_expr->text());
    if (value_expr->text() < rhs.value_expr->text())
      return true;
    // if (value_expr->text() > rhs.value_expr->text()) return false;
  }

  // Compare semantic flags last
  unsigned int lhs_flags = flags() & ANNOTATION_SEMANTIC_FLAGS;
  unsigned int rhs_flags = rhs.flags() & ANNOTATION_SEMANTIC_FLAGS;
  if (lhs_flags < rhs_flags)
    return true;

  return false;
}

/**
 * @brief Parse annotation fields from the input stream.
 *
 * Reads annotation components in a loop, stopping when a non-annotation
 * character is encountered.  The supported bracket syntax is:
 *
 *   - `{price}`   -- per-unit lot price
 *   - `{{price}}` -- total lot price (sets ANNOTATION_PRICE_NOT_PER_UNIT)
 *   - `{=price}`  -- fixated lot price (sets ANNOTATION_PRICE_FIXATED)
 *   - `[date]`    -- acquisition date
 *   - `(tag)`     -- free-form lot tag
 *   - `((expr))`  -- valuation expression (supports nested parentheses)
 *
 * The `(@` sequence is not consumed (it belongs to the cost syntax).
 * Each field may appear at most once; duplicates throw amount_error.
 */
void annotation_t::parse(std::istream& in) {
  do {
    std::istream::pos_type pos = in.tellg();
    if (pos == std::istream::pos_type(std::streamoff(-1)))
      return;

    char buf[256];
    int c = peek_next_nonws(in);
    if (c == '{') {
      if (price)
        throw_(amount_error, _("Commodity specifies more than one price"));

      in.get();
      c = in.peek();
      if (c == '{') {
        in.get();
        add_flags(ANNOTATION_PRICE_NOT_PER_UNIT);
      }

      c = peek_next_nonws(in);
      if (c == '=') {
        in.get();
        add_flags(ANNOTATION_PRICE_FIXATED);
      }

      READ_INTO(in, buf, 255, c, c != '}');
      if (c == '}') {
        in.get();
        if (has_flags(ANNOTATION_PRICE_NOT_PER_UNIT)) {
          c = in.peek();
          if (c != '}')
            throw_(amount_error, _("Commodity lot price lacks double closing brace"));
          else
            in.get();
        }
      } else {
        throw_(amount_error, _("Commodity lot price lacks closing brace"));
      }

      amount_t temp;
      (void)temp.parse(buf, PARSE_NO_MIGRATE);

      DEBUG("commodity.annotations", "Parsed annotation price: " << temp);
      price = temp;
    } else if (c == '[') {
      if (date)
        throw_(amount_error, _("Commodity specifies more than one date"));

      in.get();
      READ_INTO(in, buf, 255, c, c != ']');
      if (c == ']')
        in.get();
      else
        throw_(amount_error, _("Commodity date lacks closing bracket"));

      date = parse_date(buf);
    } else if (c == '(') {
      in.get();
      c = in.peek();
      if (c == '@') {
        in.clear();
        in.seekg(pos, std::ios::beg);
        break;
      } else if (c == '(') {
        in.get();

        if (value_expr)
          throw_(amount_error, _("Commodity specifies more than one valuation expression"));

        // Read expression with balanced parentheses to handle nested parens
        // like "market($10, date, t)" inside lot value expressions
        {
          char* _p = buf;
          int depth = 0;
          c = in.peek();
          while (in.good() && !in.eof() && c != '\n' && _p - buf < 255) {
            if (c == ')' && depth == 0)
              break;
            c = in.get();
            if (in.eof())
              break;
            if (c == '(')
              depth++;
            else if (c == ')')
              depth--;
            *_p++ = c;
            c = in.peek();
          }
          *_p = '\0';
        }
        if (c == ')') {
          in.get();
          c = in.peek();
          if (c == ')')
            in.get();
          else
            throw_(amount_error, _("Commodity valuation expression lacks closing parentheses"));
        } else {
          throw_(amount_error, _("Commodity valuation expression lacks closing parentheses"));
        }

        value_expr = expr_t(buf);
      } else {
        if (tag)
          throw_(amount_error, _("Commodity specifies more than one tag"));

        READ_INTO(in, buf, 255, c, c != ')');
        if (c == ')')
          in.get();
        else
          throw_(amount_error, _("Commodity tag lacks closing parenthesis"));

        tag = buf;
      }
    } else {
      in.clear();
      in.seekg(pos, std::ios::beg);
      break;
    }
  } while (true);

#if DEBUG_ON
  if (SHOW_DEBUG("amount.commodities") && *this) {
    DEBUG("amount.commodities", "Parsed commodity annotations: " << '\n' << *this);
  }
#endif
}

/**
 * @brief Render annotation fields back to their textual form.
 *
 * Each present field is output with its appropriate delimiters.  The
 * @p no_computed_annotations flag suppresses fields marked with
 * *_CALCULATED flags (used when printing journal output that should
 * only show user-supplied annotations).
 *
 * For total-cost prices (ANNOTATION_PRICE_NOT_PER_UNIT), the original
 * `{{total}}` form is reconstructed by multiplying the stored per-unit
 * price by abs(qty), preserving exact GMP rational arithmetic.
 */
void annotation_t::print(std::ostream& out, bool keep_base, bool no_computed_annotations,
                         const amount_t* qty, uint_least8_t print_flags) const {
  if (price && (!no_computed_annotations || !has_flags(ANNOTATION_PRICE_CALCULATED))) {
    if (has_flags(ANNOTATION_PRICE_NOT_PER_UNIT) && qty != nullptr &&
        (print_flags & AMOUNT_PRINT_PRESERVE_TOTAL_COST)) {
      // Reconstruct original total cost: stored per-unit price * abs(quantity).
      // Using GMP rational arithmetic this multiplication is exact (no rounding).
      amount_t total = *price * qty->abs();
      out << " {{" << (has_flags(ANNOTATION_PRICE_FIXATED) ? "=" : "")
          << (keep_base ? total : total.unreduced()) << "}}";
    } else {
      out << " {" << (has_flags(ANNOTATION_PRICE_FIXATED) ? "=" : "")
          << (keep_base ? *price : price->unreduced()) << '}';
    }
  }

  if (date && (!no_computed_annotations || !has_flags(ANNOTATION_DATE_CALCULATED)))
    out << " [" << format_date(*date, FMT_WRITTEN) << ']';

  if (tag && (!no_computed_annotations || !has_flags(ANNOTATION_TAG_CALCULATED)))
    out << " (" << *tag << ')';

  if (value_expr && !has_flags(ANNOTATION_VALUE_EXPR_CALCULATED))
    out << " ((" << *value_expr << "))";
}

/// @}

void put_annotation(property_tree::ptree& st, const annotation_t& details) {
  if (details.price)
    put_amount(st.put("price", ""), *details.price);

  if (details.date)
    put_date(st.put("date", ""), *details.date);

  if (details.tag)
    st.put("tag", *details.tag);

  if (details.value_expr)
    st.put("value_expr", details.value_expr->text());
}

/// @name keep_details_t implementation
/// @{

bool keep_details_t::keep_all(const commodity_t& comm) const {
  return (!comm.has_annotation() || (keep_price && keep_date && keep_tag && !only_actuals));
}

bool keep_details_t::keep_any(const commodity_t& comm) const {
  return comm.has_annotation() && (keep_price || keep_date || keep_tag);
}

/// @}

/// @name annotated_commodity_t implementation
/// @{

bool annotated_commodity_t::operator==(const commodity_t& comm) const {
  // If the base commodities don't match, the game's up.
  if (base != comm.base)
    return false;

  assert(annotated);
  if (!comm.annotated)
    return false;

  if (details != as_annotated_commodity(comm).details)
    return false;

  return true;
}

/**
 * @brief Price lookup with fixated-price and valuation-expression support.
 *
 * This override implements the lot-aware pricing semantics that make
 * `{=$50}` (fixated) different from `{$50}` (floating):
 *
 *   1. If the annotation has a fixated price (ANNOTATION_PRICE_FIXATED,
 *      written as `{=$50}` in journal syntax), that price is returned
 *      immediately -- fixated prices represent a contractual or book value
 *      that does not change with the market.
 *
 *   2. If the annotation has a non-fixated price and no explicit target
 *      commodity was requested, the price's commodity is used as the
 *      target.  This guides the price history graph search toward the
 *      right currency (e.g., if the lot was bought for $150, the target
 *      becomes "$").
 *
 *   3. If a valuation expression `((expr))` is present, it is evaluated
 *      to compute the price.
 *
 *   4. Otherwise, the call falls through to commodity_t::find_price()
 *      which walks the price history graph.
 */
std::optional<price_point_t> annotated_commodity_t::find_price(const commodity_t* commodity,
                                                               const datetime_t& moment,
                                                               const datetime_t& oldest) const {
  DEBUG("commodity.price.find", "annotated_commodity_t::find_price(" << symbol() << ")");

  datetime_t when;
  if (!moment.is_not_a_date_time())
    when = moment;
  else if (epoch)
    when = *epoch;
  else
    when = CURRENT_TIME();

  DEBUG("commodity.price.find", "reference time: " << when);

  const commodity_t* target = nullptr;
  if (commodity)
    target = commodity;

  if (details.price) {
    DEBUG("commodity.price.find", "price annotation: " << *details.price);

    if (details.has_flags(ANNOTATION_PRICE_FIXATED)) {
      DEBUG("commodity.price.find", "amount_t::value: fixated price =  " << *details.price);
      return price_point_t(when, *details.price);
    } else if (!target) {
      DEBUG("commodity.price.find", "setting target commodity from price");
      target = details.price->commodity_ptr();
    }
  }

#if DEBUG_ON
  if (target)
    DEBUG("commodity.price.find", "target commodity: " << target->symbol());
#endif

  if (details.value_expr)
    return find_price_from_expr(const_cast<expr_t&>(*details.value_expr), commodity, when);

  return commodity_t::find_price(target, when, oldest);
}

/**
 * @brief Selectively remove annotation fields based on a keep_details_t filter.
 *
 * This is the mechanism behind `--basis` (which strips all annotations,
 * showing amounts in their base commodity) and the `--lots` family of
 * options (which selectively preserve lot prices, dates, or tags).
 *
 * For each annotation field, retention requires both:
 *   - The corresponding keep_* flag is true in @p what_to_keep.
 *   - If only_actuals is set, the field must not carry a *_CALCULATED flag
 *     (i.e., it must have been explicitly written in the journal).
 *
 * If at least one field survives, a (possibly new) annotated_commodity_t
 * is obtained from the pool via find_or_create, and the relevant flags
 * are transferred.  If no fields survive, the unannotated base commodity
 * (referent()) is returned.
 */
commodity_t& annotated_commodity_t::strip_annotations(const keep_details_t& what_to_keep) {
  DEBUG("commodity.annotated.strip", "Reducing commodity "
                                         << *this << '\n'
                                         << "  keep price " << what_to_keep.keep_price << " "
                                         << "  keep date " << what_to_keep.keep_date << " "
                                         << "  keep tag " << what_to_keep.keep_tag);

  commodity_t* new_comm;

  bool keep_price = (what_to_keep.keep_price && (!what_to_keep.only_actuals ||
                                                 !details.has_flags(ANNOTATION_PRICE_CALCULATED)));
  bool keep_date = (what_to_keep.keep_date &&
                    (!what_to_keep.only_actuals || !details.has_flags(ANNOTATION_DATE_CALCULATED)));
  bool keep_tag = (what_to_keep.keep_tag &&
                   (!what_to_keep.only_actuals || !details.has_flags(ANNOTATION_TAG_CALCULATED)));

  DEBUG("commodity.annotated.strip", "Reducing commodity " << *this << '\n'
                                                           << "  keep price " << keep_price << " "
                                                           << "  keep date " << keep_date << " "
                                                           << "  keep tag " << keep_tag);

  if ((keep_price && details.price) || (keep_date && details.date) || (keep_tag && details.tag)) {
    new_comm =
        pool().find_or_create(referent(), annotation_t(keep_price ? details.price : std::nullopt,
                                                       keep_date ? details.date : std::nullopt,
                                                       keep_tag ? details.tag : std::nullopt));

    // Transfer over any relevant annotation flags, as they still apply.
    if (new_comm->annotated) {
      annotation_t& new_details(as_annotated_commodity(*new_comm).details);
      if (keep_price)
        new_details.add_flags(details.flags() &
                              (ANNOTATION_PRICE_CALCULATED | ANNOTATION_PRICE_FIXATED));
      if (keep_date)
        new_details.add_flags(details.flags() & ANNOTATION_DATE_CALCULATED);
      if (keep_tag)
        new_details.add_flags(details.flags() & ANNOTATION_TAG_CALCULATED);
    }

    return *new_comm;
  }

  return referent();
}

/**
 * @brief Delegate annotation printing to annotation_t::print.
 *
 * Passes the pool's keep_base setting so that amounts are displayed
 * in their base or unreduced form as appropriate.
 */
void annotated_commodity_t::write_annotations(std::ostream& out, bool no_computed_annotations,
                                              const amount_t* qty,
                                              uint_least8_t print_flags) const {
  details.print(out, pool().keep_base, no_computed_annotations, qty, print_flags);
}

/// @}

} // namespace ledger
