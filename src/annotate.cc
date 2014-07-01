/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "amount.h"
#include "commodity.h"
#include "expr.h"
#include "annotate.h"
#include "pool.h"

namespace ledger {

bool annotation_t::operator<(const annotation_t& rhs) const
{
  if (! price && rhs.price) return true;
  if (price && ! rhs.price) return false;
  if (! date && rhs.date)   return true;
  if (date && ! rhs.date)   return false;
  if (! tag && rhs.tag)     return true;
  if (tag && ! rhs.tag)     return false;

  if (! value_expr && rhs.value_expr) return true;
  if (value_expr && ! rhs.value_expr) return false;

  if (price) {
    if (price->commodity().symbol() < rhs.price->commodity().symbol())
      return true;
    if (price->commodity().symbol() > rhs.price->commodity().symbol())
      return false;

    if (*price < *rhs.price) return true;
    if (*price > *rhs.price) return false;
  }
  if (date) {
    if (*date < *rhs.date)   return true;
    if (*date > *rhs.date)   return false;
  }
  if (tag) {
    if (*tag < *rhs.tag)     return true;
    if (*tag > *rhs.tag)     return false;
  }
  if (value_expr) {
    DEBUG("annotate.less", "Comparing (" << value_expr->text()
          << ") < (" << rhs.value_expr->text());
    if (value_expr->text() < rhs.value_expr->text()) return true;
    //if (value_expr->text() > rhs.value_expr->text()) return false;
  }

  return false;
}

void annotation_t::parse(std::istream& in)
{
  do {
    istream_pos_type pos = in.tellg();
    if (static_cast<int>(pos) < 0)
      return;

    char buf[256];
    char c = peek_next_nonws(in);
    if (c == '{') {
      if (price)
        throw_(amount_error, _("Commodity specifies more than one price"));

      in.get(c);
      c = static_cast<char>(in.peek());
      if (c == '{') {
        in.get(c);
        add_flags(ANNOTATION_PRICE_NOT_PER_UNIT);
      }

      c = peek_next_nonws(in);
      if (c == '=') {
        in.get(c);
        add_flags(ANNOTATION_PRICE_FIXATED);
      }

      READ_INTO(in, buf, 255, c, c != '}');
      if (c == '}') {
        in.get(c);
        if (has_flags(ANNOTATION_PRICE_NOT_PER_UNIT)) {
          c = static_cast<char>(in.peek());
          if (c != '}')
            throw_(amount_error, _("Commodity lot price lacks double closing brace"));
          else
            in.get(c);
        }
      } else {
        throw_(amount_error, _("Commodity lot price lacks closing brace"));
      }

      amount_t temp;
      temp.parse(buf, PARSE_NO_MIGRATE);

      DEBUG("commodity.annotations", "Parsed annotation price: " << temp);
      price = temp;
    }
    else if (c == '[') {
      if (date)
        throw_(amount_error, _("Commodity specifies more than one date"));

      in.get(c);
      READ_INTO(in, buf, 255, c, c != ']');
      if (c == ']')
        in.get(c);
      else
        throw_(amount_error, _("Commodity date lacks closing bracket"));

      date = parse_date(buf);
    }
    else if (c == '(') {
      in.get(c);
      c = static_cast<char>(in.peek());
      if (c == '@') {
        in.clear();
        in.seekg(pos, std::ios::beg);
        break;
      }
      else if (c == '(') {
        if (value_expr)
          throw_(amount_error,
                 _("Commodity specifies more than one valuation expresion"));

        in.get(c);
        READ_INTO(in, buf, 255, c, c != ')');
        if (c == ')') {
          in.get(c);
          c = static_cast<char>(in.peek());
          if (c == ')')
            in.get(c);
          else
            throw_(amount_error,
                   _("Commodity valuation expression lacks closing parentheses"));
        } else {
          throw_(amount_error,
                 _("Commodity valuation expression lacks closing parentheses"));
        }

        value_expr = expr_t(buf);
      } else {
        if (tag)
          throw_(amount_error, _("Commodity specifies more than one tag"));

        READ_INTO(in, buf, 255, c, c != ')');
        if (c == ')')
          in.get(c);
        else
          throw_(amount_error, _("Commodity tag lacks closing parenthesis"));

        tag = buf;
      }
    }
    else {
      in.clear();
      in.seekg(pos, std::ios::beg);
      break;
    }
  } while (true);

#if DEBUG_ON
  if (SHOW_DEBUG("amount.commodities") && *this) {
    DEBUG("amount.commodities",
          "Parsed commodity annotations: " << std::endl << *this);
  }
#endif
}

void annotation_t::print(std::ostream& out, bool keep_base,
                         bool no_computed_annotations) const
{
  if (price &&
      (! no_computed_annotations || ! has_flags(ANNOTATION_PRICE_CALCULATED)))
    out << " {"
        << (has_flags(ANNOTATION_PRICE_FIXATED) ? "=" : "")
        << (keep_base ? *price : price->unreduced())
        << '}';

  if (date &&
      (! no_computed_annotations || ! has_flags(ANNOTATION_DATE_CALCULATED)))
    out << " [" << format_date(*date, FMT_PRINTED) << ']';

  if (tag &&
      (! no_computed_annotations || ! has_flags(ANNOTATION_TAG_CALCULATED)))
    out << " (" << *tag << ')';

  if (value_expr && ! has_flags(ANNOTATION_VALUE_EXPR_CALCULATED))
    out << " ((" << *value_expr << "))";
}

void put_annotation(property_tree::ptree& st, const annotation_t& details)
{
  if (details.price)
    put_amount(st.put("price", ""), *details.price);

  if (details.date)
    put_date(st.put("date", ""), *details.date);

  if (details.tag)
    st.put("tag", *details.tag);

  if (details.value_expr)
    st.put("value_expr", details.value_expr->text());
}

bool keep_details_t::keep_all(const commodity_t& comm) const
{
  return (! comm.has_annotation() ||
          (keep_price && keep_date && keep_tag && ! only_actuals));
}

bool keep_details_t::keep_any(const commodity_t& comm) const
{
  return comm.has_annotation() && (keep_price || keep_date || keep_tag);
}

bool annotated_commodity_t::operator==(const commodity_t& comm) const
{
  // If the base commodities don't match, the game's up.
  if (base != comm.base)
    return false;

  assert(annotated);
  if (! comm.annotated)
    return false;

  if (details != as_annotated_commodity(comm).details)
    return false;

  return true;
}

optional<price_point_t>
annotated_commodity_t::find_price(const commodity_t * commodity,
                                  const datetime_t&   moment,
                                  const datetime_t&   oldest) const
{
  DEBUG("commodity.price.find",
        "annotated_commodity_t::find_price(" << symbol() << ")");

  datetime_t when;
  if (! moment.is_not_a_date_time())
    when = moment;
  else if (epoch)
    when = *epoch;
  else
    when = CURRENT_TIME();

  DEBUG("commodity.price.find", "reference time: " << when);

  const commodity_t * target = NULL;
  if (commodity)
    target = commodity;

  if (details.price) {
    DEBUG("commodity.price.find", "price annotation: " << *details.price);

    if (details.has_flags(ANNOTATION_PRICE_FIXATED)) {
      DEBUG("commodity.price.find",
            "amount_t::value: fixated price =  " << *details.price);
      return price_point_t(when, *details.price);
    }
    else if (! target) {
      DEBUG("commodity.price.find", "setting target commodity from price");
      target = details.price->commodity_ptr();
    }
  }

#if DEBUG_ON
  if (target)
    DEBUG("commodity.price.find", "target commodity: " << target->symbol());
#endif

  if (details.value_expr)
    return find_price_from_expr(const_cast<expr_t&>(*details.value_expr),
                                commodity, when);

  return commodity_t::find_price(target, when, oldest);
}

commodity_t&
annotated_commodity_t::strip_annotations(const keep_details_t& what_to_keep)
{
  DEBUG("commodity.annotated.strip",
        "Reducing commodity " << *this << std::endl
         << "  keep price " << what_to_keep.keep_price << " "
         << "  keep date "  << what_to_keep.keep_date << " "
         << "  keep tag "   << what_to_keep.keep_tag);

  commodity_t * new_comm;

  bool keep_price =
    ((what_to_keep.keep_price ||
      (details.has_flags(ANNOTATION_PRICE_FIXATED) &&
       has_flags(COMMODITY_SAW_ANN_PRICE_FLOAT) &&
       has_flags(COMMODITY_SAW_ANN_PRICE_FIXATED))) &&
     (! what_to_keep.only_actuals ||
      ! details.has_flags(ANNOTATION_PRICE_CALCULATED)));
  bool keep_date  =
    (what_to_keep.keep_date       &&
     (! what_to_keep.only_actuals ||
      ! details.has_flags(ANNOTATION_DATE_CALCULATED)));
  bool keep_tag   =
    (what_to_keep.keep_tag        &&
     (! what_to_keep.only_actuals ||
      ! details.has_flags(ANNOTATION_TAG_CALCULATED)));

  DEBUG("commodity.annotated.strip",
        "Reducing commodity " << *this << std::endl
         << "  keep price " << keep_price << " "
         << "  keep date "  << keep_date << " "
         << "  keep tag "   << keep_tag);

  if ((keep_price && details.price) ||
      (keep_date  && details.date)  ||
      (keep_tag   && details.tag)) {
    new_comm = pool().find_or_create
      (referent(), annotation_t(keep_price ? details.price : none,
                                keep_date  ? details.date  : none,
                                keep_tag   ? details.tag   : none));

    // Transfer over any relevant annotation flags, as they still apply.
    if (new_comm->annotated) {
      annotation_t& new_details(as_annotated_commodity(*new_comm).details);
      if (keep_price)
        new_details.add_flags(details.flags() &
                              (ANNOTATION_PRICE_CALCULATED |
                               ANNOTATION_PRICE_FIXATED));
      if (keep_date)
        new_details.add_flags(details.flags() & ANNOTATION_DATE_CALCULATED);
      if (keep_tag)
        new_details.add_flags(details.flags() & ANNOTATION_TAG_CALCULATED);
    }

    return *new_comm;
  }

  return referent();
}

void annotated_commodity_t::write_annotations
  (std::ostream& out, bool no_computed_annotations) const
{
  details.print(out, pool().keep_base, no_computed_annotations);
}

} // namespace ledger
