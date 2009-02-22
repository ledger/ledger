/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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

#include "commodity.h"

namespace ledger {

void commodity_t::base_t::history_t::add_price(commodity_t&      source,
					       const datetime_t& date,
					       const amount_t&	 price,
					       const bool	 reflexive)
{
  DEBUG("commodity.prices",
	"add_price to " << source << " : " << date << ", " << price);

  history_map::iterator i = prices.find(date);
  if (i != prices.end()) {
    (*i).second = price;
  } else {
    std::pair<history_map::iterator, bool> result
      = prices.insert(history_map::value_type(date, price));
    assert(result.second);
  }

  if (reflexive) {
    if (! price.commodity().has_flags(COMMODITY_NOMARKET)) {
      amount_t inverse = price.inverted();
      inverse.set_commodity(const_cast<commodity_t&>(source));
      price.commodity().add_price(date, inverse, false);
    }
    source.add_flags(COMMODITY_PRIMARY);
  }
}

bool commodity_t::base_t::history_t::remove_price(const datetime_t& date)
{
  DEBUG("commodity.prices", "remove_price: " << date);

  history_map::size_type n = prices.erase(date);
  if (n > 0)
    return true;
  return false;
}

void commodity_t::base_t::varied_history_t::
  add_price(commodity_t&      source,
	    const datetime_t& date,
	    const amount_t&   price,
	    const bool	      reflexive)
{
  optional<history_t&> hist = history(price.commodity());
  if (! hist) {
    std::pair<history_by_commodity_map::iterator, bool> result
      = histories.insert(history_by_commodity_map::value_type
			 (&price.commodity(), history_t()));
    assert(result.second);

    hist = (*result.first).second;
  }
  assert(hist);

  hist->add_price(source, date, price, reflexive);
}

bool commodity_t::base_t::varied_history_t::remove_price(const datetime_t&  date,
							 commodity_t& comm)
{
  DEBUG("commodity.prices", "varied_remove_price: " << date << ", " << comm);

  if (optional<history_t&> hist = history(comm))
    return hist->remove_price(date);
  return false;
}

optional<price_point_t>
  commodity_t::base_t::history_t::
    find_price(const optional<datetime_t>&   moment,
	       const optional<datetime_t>&   oldest
#if defined(DEBUG_ON)
	       , const int indent
#endif
	       ) const
{
  price_point_t point;
  bool          found = false;

#define DEBUG_INDENT(cat, indent)		\
  do {						\
    if (SHOW_DEBUG(cat))			\
      for (int i = 0; i < indent; i++)		\
	ledger::_log_buffer << "  ";		\
  } while (false)

#if defined(DEBUG_ON)
  DEBUG_INDENT("commodity.prices", indent);
  if (moment)
    DEBUG("commodity.prices", "find price nearest before or on: " << *moment);
  else
    DEBUG("commodity.prices", "find any price");

  if (oldest) {
    DEBUG_INDENT("commodity.prices", indent);
    DEBUG("commodity.prices", "  but no older than: " << *oldest);
  }
#endif

  if (prices.size() == 0) {
#if defined(DEBUG_ON)
    DEBUG_INDENT("commodity.prices", indent);
    DEBUG("commodity.prices", "  there are no prices in this history");
#endif
    return none;
  }

  if (! moment) {
    history_map::const_reverse_iterator r = prices.rbegin();
    point.when	= (*r).first;
    point.price = (*r).second;
    found = true;
#if defined(DEBUG_ON)
    DEBUG_INDENT("commodity.prices", indent);
    DEBUG("commodity.prices", "  using most recent price");
#endif
  } else {
    history_map::const_iterator i = prices.lower_bound(*moment);
    if (i == prices.end()) {
      history_map::const_reverse_iterator r = prices.rbegin();
      point.when  = (*r).first;
      point.price = (*r).second;
      found = true;
#if defined(DEBUG_ON)
      DEBUG_INDENT("commodity.prices", indent);
      DEBUG("commodity.prices", "  using last price");
#endif
    } else {
      point.when = (*i).first;
      if (*moment < point.when) {
	if (i != prices.begin()) {
	  --i;
	  point.when  = (*i).first;
	  point.price = (*i).second;
	  found = true;
	}
      } else {
	point.price = (*i).second;
	found = true;
      }
#if defined(DEBUG_ON)
      DEBUG_INDENT("commodity.prices", indent);
      DEBUG("commodity.prices", "  using found price");
#endif
    }
  }

#if 0
  if (! has_flags(COMMODITY_NOMARKET) && parent().get_quote) {
    if (optional<amount_t> quote = parent().get_quote
	(*this, age, moment,
	 (hist && hist->prices.size() > 0 ?
	  (*hist->prices.rbegin()).first : optional<datetime_t>())))
      return *quote;
  }
#endif

  if (! found) {
#if defined(DEBUG_ON)
    DEBUG_INDENT("commodity.prices", indent);
    DEBUG("commodity.prices", "  could not find a price");
#endif
    return none;
  }
  else if (moment && point.when > *moment) {
#if defined(DEBUG_ON)
    DEBUG_INDENT("commodity.prices", indent);
    DEBUG("commodity.prices", "  price is too young ");
#endif
    return none;
  }
  else if (oldest && point.when < *oldest) {
#if defined(DEBUG_ON)
    DEBUG_INDENT("commodity.prices", indent);
    DEBUG("commodity.prices", "  price is too old ");
#endif
    return none;
  }
  else {
#if defined(DEBUG_ON)
    DEBUG_INDENT("commodity.prices", indent);
    DEBUG("commodity.prices",
	  "  returning price: " << point.when << ", " << point.price);
#endif
    return point;
  }
}

optional<price_point_t>
  commodity_t::base_t::varied_history_t::
    find_price(const commodity_t&            source,
	       const optional<commodity_t&>& commodity,
	       const optional<datetime_t>&   moment,
	       const optional<datetime_t>&   oldest
#if defined(DEBUG_ON)
	       , const int indent
#endif
	       ) const
{
  optional<price_point_t> point;
  optional<datetime_t>	  limit = oldest;

  assert(! commodity || source != *commodity);

#if defined(DEBUG_ON)
  DEBUG_INDENT("commodity.prices", indent);
  DEBUG("commodity.prices", "varied_find_price for: " << source);

  DEBUG_INDENT("commodity.prices", indent);
  if (commodity)
    DEBUG("commodity.prices", "  looking for: commodity '" << *commodity << "'");
  else
    DEBUG("commodity.prices", "  looking for: any commodity");

  if (moment) {
    DEBUG_INDENT("commodity.prices", indent);
    DEBUG("commodity.prices", "  time index: " << *moment);
  }

  if (oldest) {
    DEBUG_INDENT("commodity.prices", indent);
    DEBUG("commodity.prices", "  only consider prices younger than: " << *oldest);
  }
#endif

  // Either we couldn't find a history for the target commodity, or we
  // couldn't find a price.  In either case, search all histories known
  // to this commodity for a price which we can calculate in terms of
  // the goal commodity.
  price_point_t best;
  bool          found = false;

  foreach (history_by_commodity_map::value_type hist, histories) {
    commodity_t& comm(*hist.first);
    if (comm == source)
      continue;

#if defined(DEBUG_ON)
    DEBUG_INDENT("commodity.prices", indent + 1);
    DEBUG("commodity.prices",
	  "  searching for price via commodity '" << comm << "'");
#endif

    point = hist.second.find_price(moment, limit
#if defined(DEBUG_ON)
				   , indent + 2
#endif
				   );
    assert(! point || point->price.commodity() == comm);

    if (point) {
      optional<price_point_t> xlat;

      if (commodity && comm != *commodity) {
#if defined(DEBUG_ON)
	DEBUG_INDENT("commodity.prices", indent + 1);
	DEBUG("commodity.prices", "  looking for translation price");
#endif

	xlat = comm.find_price(commodity, moment, limit
#if defined(DEBUG_ON)
			       , indent + 2
#endif
			       );
	if (xlat) {
#if defined(DEBUG_ON)
	  DEBUG_INDENT("commodity.prices", indent + 1);
	  DEBUG("commodity.prices", "  found translated price "
		<< xlat->price << " from " << xlat->when);
#endif

	  point->price = xlat->price * point->price;
	  if (xlat->when < point->when) {
	    point->when = xlat->when;
#if defined(DEBUG_ON)
	    DEBUG_INDENT("commodity.prices", indent + 1);
	    DEBUG("commodity.prices",
		  "  adjusting date of result back to " << point->when);
#endif
	  }
	} else {
#if defined(DEBUG_ON)
	  DEBUG_INDENT("commodity.prices", indent + 1);
	  DEBUG("commodity.prices", "  saw no translated price there");
#endif
	  continue;
	}
      }

      assert(! commodity || point->price.commodity() == *commodity);
#if defined(DEBUG_ON)
      DEBUG_INDENT("commodity.prices", indent + 1);
      DEBUG("commodity.prices",
	    "  saw a price there: " << point->price << " from " << point->when);
#endif
      if (! limit || point->when > *limit) {
	limit = point->when;
	best  = *point;
	found = true;
      }
    } else {
#if defined(DEBUG_ON)
      DEBUG_INDENT("commodity.prices", indent + 1);
      DEBUG("commodity.prices", "  saw no price there");
#endif
    }
  }

  if (found) {
#if defined(DEBUG_ON)
    DEBUG_INDENT("commodity.prices", indent);
    DEBUG("commodity.prices",
	  "  found price " << best.price << " from " << best.when);
#endif
    return best;
  }
  return none;
}

optional<price_point_t>
  commodity_t::base_t::varied_history_t::
    find_price(const commodity_t&                source,
	       const std::vector<commodity_t *>& commodities,
	       const optional<datetime_t>&       moment,
	       const optional<datetime_t>&       oldest
#if defined(DEBUG_ON)
	       , const int indent
#endif
	       ) const
{
  foreach (commodity_t * commodity, commodities) {
    if (optional<price_point_t> point = find_price(source, *commodity,
						   moment, oldest
#if defined(DEBUG_ON)
						   , indent
#endif
						   ))
      return point;
  }
  return none;
}

optional<commodity_t::base_t::history_t&>
  commodity_t::base_t::varied_history_t::
    history(const optional<commodity_t&>& commodity)
{
  commodity_t * comm = NULL;
  if (! commodity) {
    if (histories.size() > 1)
      return none;
#if 0
      // jww (2008-09-20): Document which option switch to use here
      throw_(commodity_error,
	     "Cannot determine price history: prices known for multiple commodities (use -?)");
#endif
    comm = (*histories.begin()).first;
  } else {
    comm = &(*commodity);
  }

  history_by_commodity_map::iterator i = histories.find(comm);
  if (i != histories.end())
    return (*i).second;

  return none;
}

optional<commodity_t::history_t&>
commodity_t::base_t::varied_history_t::history
  (const std::vector<commodity_t *>& commodities)
{
  // This function differs from the single commodity case avoid in that
  // 'commodities' represents a list of preferred valuation commodities.
  // If no price can be located in terms of the first commodity, then
  // the second is chosen, etc.

  foreach (commodity_t * commodity, commodities) {
    if (optional<history_t&> hist = history(*commodity))
      return hist;
  }
  return none;
}

void commodity_t::exchange(commodity_t&	     commodity,
			   const amount_t&   per_unit_cost,
			   const datetime_t& moment)
{
  if (! commodity.has_flags(COMMODITY_NOMARKET)) {
    DEBUG("commodity.prices", "exchanging commodity " << commodity
	  << " at per unit cost " << per_unit_cost << " on " << moment);

    commodity_t& base_commodity
      (commodity.annotated ?
       as_annotated_commodity(commodity).referent() : commodity);

    base_commodity.add_price(moment, per_unit_cost);
  }
}

commodity_t::cost_breakdown_t
commodity_t::exchange(const amount_t&		  amount,
		      const amount_t&		  cost,
		      const bool		  is_per_unit,
		      const optional<datetime_t>& moment,
		      const optional<string>&     tag)
{
  // (let* ((commodity (amount-commodity amount))
  //        (current-annotation
  //         (and (annotated-commodity-p commodity)
  //              (commodity-annotation commodity)))
  //        (base-commodity (if (annotated-commodity-p commodity)
  //                            (get-referent commodity)
  //                            commodity))
  //        (per-unit-cost (or per-unit-cost
  //                           (divide total-cost amount)))
  //        (total-cost (or total-cost
  //                        (multiply per-unit-cost amount))))

  commodity_t& commodity(amount.commodity());

  annotation_t * current_annotation = NULL;
  if (commodity.annotated)
    current_annotation = &as_annotated_commodity(commodity).details;

  commodity_t& base_commodity
    (current_annotation ?
     as_annotated_commodity(commodity).referent() : commodity);

  amount_t per_unit_cost(is_per_unit ? cost : cost / amount);

  cost_breakdown_t breakdown;
  breakdown.final_cost = ! is_per_unit ? cost : cost * amount;

  // Add a price history entry for this conversion if we know when it took
  // place

  // (if (and moment (not (commodity-no-market-price-p base-commodity)))
  //     (add-price base-commodity per-unit-cost moment))

  if (moment && ! commodity.has_flags(COMMODITY_NOMARKET))
    base_commodity.add_price(*moment, per_unit_cost);

  // ;; returns: ANNOTATED-AMOUNT TOTAL-COST BASIS-COST
  // (values (annotate-commodity
  //          amount
  //          (make-commodity-annotation :price per-unit-cost
  //                                     :date  moment
  //                                     :tag   tag))
  //         total-cost
  //         (if current-annotation
  //             (multiply (annotation-price current-annotation) amount)
  //             total-cost))))

  if (current_annotation && current_annotation->price)
    breakdown.basis_cost = *current_annotation->price * amount;
  else
    breakdown.basis_cost = breakdown.final_cost;

  breakdown.amount =
    amount_t(amount, annotation_t(per_unit_cost, moment ?
				  moment->date() : optional<date_t>(), tag));

  return breakdown;
}

commodity_t::operator bool() const
{
  return this != parent().null_commodity;
}

bool commodity_t::symbol_needs_quotes(const string& symbol)
{
  foreach (char ch, symbol)
    if (std::isspace(ch) || std::isdigit(ch) || ch == '-' || ch == '.')
      return true;

  return false;
}

namespace {
  bool is_reserved_token(const char * buf)
  {
    switch (buf[0]) {
    case 'a':
      return std::strcmp(buf, "and") == 0;
    case 'd':
      return std::strcmp(buf, "div") == 0;
    case 'f':
      return std::strcmp(buf, "false") == 0;
    case 'o':
      return std::strcmp(buf, "or") == 0;
    case 'n':
      return std::strcmp(buf, "not") == 0;
    case 't':
      return std::strcmp(buf, "true") == 0;
    }
    return false;
  }
}

void commodity_t::parse_symbol(std::istream& in, string& symbol)
{
  // Invalid commodity characters:
  //   SPACE, TAB, NEWLINE, RETURN
  //   0-9 . , ; - + * / ^ ? : & | ! =
  //   < > { } [ ] ( ) @

  static int invalid_chars[256] = {
    /* 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f */
    /* 00 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0,
    /* 10 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 20 */ 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
    /* 30 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    /* 40 */ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 50 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0,
    /* 60 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 70 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0,
    /* 80 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* 90 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* a0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* b0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* c0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* d0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* e0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    /* f0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  };

  istream_pos_type pos = in.tellg();

  char buf[256];
  char c = peek_next_nonws(in);
  if (c == '"') {
    in.get(c);
    READ_INTO(in, buf, 255, c, c != '"');
    if (c == '"')
      in.get(c);
    else
      throw_(amount_error, "Quoted commodity symbol lacks closing quote");
  } else {
    char * _p = buf;
    c = in.peek();
    while (_p - buf < 255 && in.good() && ! in.eof() && c != '\n') {
      int bytes = 0;
      int size  = _p - buf;

      unsigned char d = c;

      // Check for the start of a UTF-8 multi-byte encoded string
      if (d >= 192 && d <= 223 && size < 254)
	bytes = 2;
      else if (d >= 224 && d <= 239 && size < 253)
	bytes = 3;
      else if (d >= 240 && d <= 247 && size < 252)
	bytes = 4;
      else if (d >= 248 && d <= 251 && size < 251)
	bytes = 5;
      else if (d >= 252 && d <= 253 && size < 250)
	bytes = 6;
      else if (d >= 254) // UTF-8 encoding error
	break;

      if (bytes > 0) {		// we're looking at a UTF-8 encoding
	for (int i = 0; i < bytes; i++) {
	  in.get(c);
	  if (in.bad() || in.eof())
	    break;
	  *_p++ = c;
	}
      }
      else if (invalid_chars[static_cast<unsigned char>(c)]) {
	break;
      }
      else {
	in.get(c);
	if (in.eof())
	  break;
	if (c == '\\') {
	  in.get(c);
	  if (in.eof())
	    break;
	}
	*_p++ = c;
      }

      c = in.peek();
    }
    *_p = '\0';

    if (is_reserved_token(buf))
      buf[0] = '\0';
  }
  symbol = buf;

  if (symbol.length() == 0) {
    in.clear();
    in.seekg(pos, std::ios::beg);
  }
}

void commodity_t::parse_symbol(char *& p, string& symbol)
{
  if (*p == '"') {
    char * q = std::strchr(p + 1, '"');
    if (! q)
      throw_(amount_error, "Quoted commodity symbol lacks closing quote");
    symbol = string(p + 1, 0, q - p - 1);
    p = q + 2;
  } else {
    char * q = next_element(p);
    symbol = p;
    if (q)
      p = q;
    else
      p += symbol.length();
  }
  if (symbol.empty())
    throw_(amount_error, "Failed to parse commodity");
}

bool commodity_t::valid() const
{
  if (symbol().empty() && this != parent().null_commodity) {
    DEBUG("ledger.validate",
	   "commodity_t: symbol().empty() && this != null_commodity");
    return false;
  }

  if (annotated && ! base) {
    DEBUG("ledger.validate", "commodity_t: annotated && ! base");
    return false;
  }

  if (precision() > 16) {
    DEBUG("ledger.validate", "commodity_t: precision() > 16");
    return false;
  }

  return true;
}

void annotation_t::parse(std::istream& in)
{
  do {
    istream_pos_type pos = in.tellg();

    char buf[256];
    char c = peek_next_nonws(in);
    if (c == '{') {
      if (price)
	throw_(amount_error, "Commodity specifies more than one price");

      in.get(c);
      READ_INTO(in, buf, 255, c, c != '}');
      if (c == '}')
	in.get(c);
      else
	throw_(amount_error, "Commodity price lacks closing brace");

      amount_t temp;
      temp.parse(buf, amount_t::PARSE_NO_MIGRATE);
      temp.in_place_reduce();

      // Since this price will maintain its own precision, make sure
      // it is at least as large as the base commodity, since the user
      // may have only specified {$1} or something similar.

      if (temp.has_commodity() &&
	  temp.precision() > temp.commodity().precision())
	temp = temp.rounded();	// no need to retain individual precision

      price = temp;
    }
    else if (c == '[') {
      if (date)
	throw_(amount_error, "Commodity specifies more than one date");

      in.get(c);
      READ_INTO(in, buf, 255, c, c != ']');
      if (c == ']')
	in.get(c);
      else
	throw_(amount_error, "Commodity date lacks closing bracket");

      date = parse_date(buf);
    }
    else if (c == '(') {
      if (tag)
	throw_(amount_error, "Commodity specifies more than one tag");

      in.get(c);
      READ_INTO(in, buf, 255, c, c != ')');
      if (c == ')')
	in.get(c);
      else
	throw_(amount_error, "Commodity tag lacks closing parenthesis");

      tag = buf;
    }
    else {
      in.clear();
      in.seekg(pos, std::ios::beg);
      break;
    }
  } while (true);

  DEBUG("amounts.commodities",
	"Parsed commodity annotations: " << std::endl << *this);
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

  if (what_to_keep.keep_any(*this) &&
      ((what_to_keep.keep_price && details.price) ||
       (what_to_keep.keep_date  && details.date) ||
       (what_to_keep.keep_tag   && details.tag)))
  {
    new_comm = parent().find_or_create
      (referent(),
       annotation_t(what_to_keep.keep_price ? details.price : none,
		    what_to_keep.keep_date  ? details.date  : none,
		    what_to_keep.keep_tag   ? details.tag   : none));
  } else {
    new_comm = parent().find_or_create(base_symbol());
  }

  assert(new_comm);
  return *new_comm;
}

void annotated_commodity_t::write_annotations(std::ostream&       out,
					      const annotation_t& info)
{
  if (info.price)
    out << " {" << *info.price << '}';

  if (info.date)
    out << " [" << format_date(*info.date) << ']';

  if (info.tag)
    out << " (" << *info.tag << ')';
}

bool compare_amount_commodities::operator()(const amount_t * left,
					    const amount_t * right) const
{
  commodity_t& leftcomm(left->commodity());
  commodity_t& rightcomm(right->commodity());

  DEBUG("commodity.compare", " left symbol (" << leftcomm << ")");
  DEBUG("commodity.compare", "right symbol (" << rightcomm << ")");

  int cmp = leftcomm.base_symbol().compare(rightcomm.base_symbol());
  if (cmp != 0)
    return cmp < 0;

  if (! leftcomm.annotated) {
    return rightcomm.annotated;
  }
  else if (! rightcomm.annotated) {
    return ! leftcomm.annotated;
  }
  else {
    annotated_commodity_t& aleftcomm(static_cast<annotated_commodity_t&>(leftcomm));
    annotated_commodity_t& arightcomm(static_cast<annotated_commodity_t&>(rightcomm));

    if (! aleftcomm.details.price && arightcomm.details.price)
      return true;
    if (aleftcomm.details.price && ! arightcomm.details.price)
      return false;

    if (aleftcomm.details.price && arightcomm.details.price) {
      amount_t leftprice(*aleftcomm.details.price);
      leftprice.in_place_reduce();
      amount_t rightprice(*arightcomm.details.price);
      rightprice.in_place_reduce();

      if (leftprice.commodity() == rightprice.commodity()) {
	return (leftprice - rightprice).sign() < 0;
      } else {
	// Since we have two different amounts, there's really no way
	// to establish a true sorting order; we'll just do it based
	// on the numerical values.
	leftprice.clear_commodity();
	rightprice.clear_commodity();
	return (leftprice - rightprice).sign() < 0;
      }
    }

    if (! aleftcomm.details.date && arightcomm.details.date)
      return true;
    if (aleftcomm.details.date && ! arightcomm.details.date)
      return false;

    if (aleftcomm.details.date && arightcomm.details.date) {
      date_duration_t diff = *aleftcomm.details.date - *arightcomm.details.date;
      return diff.is_negative();
    }

    if (! aleftcomm.details.tag && arightcomm.details.tag)
      return true;
    if (aleftcomm.details.tag && ! arightcomm.details.tag)
      return false;

    if (aleftcomm.details.tag && arightcomm.details.tag)
      return *aleftcomm.details.tag < *arightcomm.details.tag;

    assert(false);
    return true;
  }
}

commodity_pool_t::commodity_pool_t() : default_commodity(NULL)
{
  TRACE_CTOR(commodity_pool_t, "");
  null_commodity = create("");
  null_commodity->add_flags(COMMODITY_BUILTIN | COMMODITY_NOMARKET);
}

commodity_t * commodity_pool_t::create(const string& symbol)
{
  shared_ptr<commodity_t::base_t>
    base_commodity(new commodity_t::base_t(symbol));
  std::auto_ptr<commodity_t> commodity(new commodity_t(this, base_commodity));

  DEBUG("amounts.commodities", "Creating base commodity " << symbol);

  // Create the "qualified symbol" version of this commodity's symbol
  if (commodity_t::symbol_needs_quotes(symbol)) {
    commodity->qualified_symbol = "\"";
    *commodity->qualified_symbol += symbol;
    *commodity->qualified_symbol += "\"";
  }

  DEBUG("amounts.commodities",
	"Creating commodity '" << commodity->symbol() << "'");

  std::pair<commodities_map::iterator, bool> result
    = commodities.insert(commodities_map::value_type(commodity->mapping_key(),
						     commodity.get()));
  assert(result.second);

  return commodity.release();
}

commodity_t * commodity_pool_t::find_or_create(const string& symbol)
{
  DEBUG("amounts.commodities", "Find-or-create commodity " << symbol);

  commodity_t * commodity = find(symbol);
  if (commodity)
    return commodity;
  return create(symbol);
}

commodity_t * commodity_pool_t::find(const string& symbol)
{
  DEBUG("amounts.commodities", "Find commodity " << symbol);

  commodities_map::const_iterator i = commodities.find(symbol);
  if (i != commodities.end())
    return (*i).second;
  return NULL;
}

commodity_t *
commodity_pool_t::create(const string& symbol, const annotation_t& details)
{
  commodity_t * new_comm = create(symbol);
  if (! new_comm)
    return NULL;

  if (details)
    return find_or_create(*new_comm, details);
  else
    return new_comm;
}

namespace {
  string make_qualified_name(const commodity_t&  comm,
			     const annotation_t& details)
  {
    assert(details);

    if (details.price && details.price->sign() < 0)
      throw_(amount_error, "A commodity's price may not be negative");

    std::ostringstream name;
    comm.print(name);
    annotated_commodity_t::write_annotations(name, details);

    DEBUG("amounts.commodities", "make_qualified_name for "
	  << comm.qualified_symbol << std::endl << details);
    DEBUG("amounts.commodities", "qualified_name is " << name.str());

    return name.str();
  }
}

commodity_t *
commodity_pool_t::find(const string& symbol, const annotation_t& details)
{
  commodity_t * comm = find(symbol);
  if (! comm)
    return NULL;

  if (details) {
    string name = make_qualified_name(*comm, details);

    if (commodity_t * ann_comm = find(name)) {
      assert(ann_comm->annotated && as_annotated_commodity(*ann_comm).details);
      return ann_comm;
    }
    return NULL;
  } else {
    return comm;
  }
}

commodity_t *
commodity_pool_t::find_or_create(const string& symbol,
				 const annotation_t& details)
{
  commodity_t * comm = find(symbol);
  if (! comm)
    return NULL;

  if (details)
    return find_or_create(*comm, details);
  else
    return comm;
}

commodity_t *
commodity_pool_t::create(commodity_t&	     comm,
			 const annotation_t& details,
			 const string&	     mapping_key)
{
  assert(comm);
  assert(details);
  assert(! mapping_key.empty());

  std::auto_ptr<commodity_t> commodity
    (new annotated_commodity_t(&comm, details));

  commodity->qualified_symbol = comm.symbol();
  assert(! commodity->qualified_symbol->empty());

  DEBUG("amounts.commodities", "Creating annotated commodity "
	<< "symbol " << commodity->symbol()
	<< " key "   << mapping_key << std::endl << details);

  // Add the fully annotated name to the map, so that this symbol may
  // quickly be found again.
  commodity->mapping_key_ = mapping_key;

  std::pair<commodities_map::iterator, bool> result
    = commodities.insert(commodities_map::value_type(mapping_key,
						     commodity.get()));
  assert(result.second);

  return commodity.release();
}

commodity_t * commodity_pool_t::find_or_create(commodity_t&	   comm,
					       const annotation_t& details)
{
  assert(comm);
  assert(details);

  string name = make_qualified_name(comm, details);
  assert(! name.empty());

  if (commodity_t * ann_comm = find(name)) {
    assert(ann_comm->annotated && as_annotated_commodity(*ann_comm).details);
    return ann_comm;
  }
  return create(comm, details, name);
}

void commodity_pool_t::parse_commodity_price(char * optarg)
{
  char * equals = std::strchr(optarg, '=');
  if (! equals)
    return;

  optarg = skip_ws(optarg);
  while (equals > optarg && std::isspace(*(equals - 1)))
    equals--;

  std::string symbol(optarg, 0, equals - optarg);
  amount_t	price(equals + 1);

  if (commodity_t * commodity = find_or_create(symbol))
    commodity->add_price(CURRENT_TIME(), price);
}

} // namespace ledger
