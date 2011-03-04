/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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
#include "annotate.h"
#include "pool.h"

namespace ledger {

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
      c = peek_next_nonws(in);
      if (c == '=') {
        in.get(c);
        add_flags(ANNOTATION_PRICE_FIXATED);
      }

      READ_INTO(in, buf, 255, c, c != '}');
      if (c == '}')
        in.get(c);
      else
        throw_(amount_error, _("Commodity price lacks closing brace"));

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
      if (tag)
        throw_(amount_error, _("Commodity specifies more than one tag"));

      in.get(c);
      READ_INTO(in, buf, 255, c, c != ')');
      if (c == ')')
        in.get(c);
      else
        throw_(amount_error, _("Commodity tag lacks closing parenthesis"));

      tag = buf;
    }
    else {
      in.clear();
      in.seekg(pos, std::ios::beg);
      break;
    }
  } while (true);

#if defined(DEBUG_ON)
  if (SHOW_DEBUG("amounts.commodities") && *this) {
    DEBUG("amounts.commodities",
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
    out << " [" << format_date(*date, FMT_WRITTEN) << ']';

  if (tag &&
      (! no_computed_annotations || ! has_flags(ANNOTATION_TAG_CALCULATED)))
    out << " (" << *tag << ')';
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
      (keep_tag   && details.tag))
  {
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
