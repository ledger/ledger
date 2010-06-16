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

bool commodity_t::decimal_comma_by_default = false;

void commodity_t::history_t::add_price(commodity_t&      source,
                                       const datetime_t& date,
                                       const amount_t&   price,
                                       const bool        reflexive)
{
  DEBUG("commodity.prices.add", "add_price to " << source
        << (reflexive ? " (secondary)" : " (primary)")
        << " : " << date << ", " << price);

  history_map::iterator i = prices.find(date);
  if (i != prices.end()) {
    (*i).second = price;
  } else {
    std::pair<history_map::iterator, bool> result
      = prices.insert(history_map::value_type(date, price));
    assert(result.second);
  }

  if (reflexive) {
    amount_t inverse = price.inverted();
    inverse.set_commodity(const_cast<commodity_t&>(source));
    price.commodity().add_price(date, inverse, false);
  } else {
    DEBUG("commodity.prices.add",
          "marking commodity " << source.symbol() << " as primary");
    source.add_flags(COMMODITY_PRIMARY);
  }
}

bool commodity_t::history_t::remove_price(const datetime_t& date)
{
  DEBUG("commodity.prices.add", "remove_price: " << date);

  history_map::size_type n = prices.erase(date);
  if (n > 0)
    return true;
  return false;
}

void commodity_t::varied_history_t::
  add_price(commodity_t&      source,
            const datetime_t& date,
            const amount_t&   price,
            const bool        reflexive)
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

bool commodity_t::varied_history_t::remove_price(const datetime_t& date,
                                                 commodity_t&      comm)
{
  DEBUG("commodity.prices.add", "varied_remove_price: " << date << ", " << comm);

  if (optional<history_t&> hist = history(comm))
    return hist->remove_price(date);
  return false;
}

optional<price_point_t>
commodity_t::history_t::find_price(const optional<datetime_t>& moment,
                                   const optional<datetime_t>& oldest
#if defined(DEBUG_ON)
                                   , const int indent
#endif
                                   ) const
{
  price_point_t point;
  bool          found = false;

#if defined(DEBUG_ON)
#define DEBUG_INDENT(cat, indent)               \
  do {                                          \
    if (SHOW_DEBUG(cat))                        \
      for (int i = 0; i < indent; i++)          \
        ledger::_log_buffer << "  ";            \
  } while (false)
#else
#define DEBUG_INDENT(cat, indent)
#endif

#if defined(DEBUG_ON)
  DEBUG_INDENT("commodity.prices.find", indent);
  if (moment)
    DEBUG("commodity.prices.find", "find price nearest before or on: " << *moment);
  else
    DEBUG("commodity.prices.find", "find any price");

  if (oldest) {
    DEBUG_INDENT("commodity.prices.find", indent);
    DEBUG("commodity.prices.find", "but no older than: " << *oldest);
  }
#endif

  if (prices.size() == 0) {
    DEBUG_INDENT("commodity.prices.find", indent);
    DEBUG("commodity.prices.find", "there are no prices in this history");
    return none;
  }

  if (! moment) {
    history_map::const_reverse_iterator r = prices.rbegin();
    point.when  = (*r).first;
    point.price = (*r).second;
    found = true;

    DEBUG_INDENT("commodity.prices.find", indent);
    DEBUG("commodity.prices.find", "using most recent price");
  } else {
    history_map::const_iterator i = prices.upper_bound(*moment);
    if (i == prices.end()) {
      history_map::const_reverse_iterator r = prices.rbegin();
      point.when  = (*r).first;
      point.price = (*r).second;
      found = true;

      DEBUG_INDENT("commodity.prices.find", indent);
      DEBUG("commodity.prices.find", "using last price");
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

      DEBUG_INDENT("commodity.prices.find", indent);
      DEBUG("commodity.prices.find", "using found price");
    }
  }

  if (! found) {
    DEBUG_INDENT("commodity.prices.find", indent);
    DEBUG("commodity.prices.find", "could not find a price");
    return none;
  }
  else if (moment && point.when > *moment) {
    DEBUG_INDENT("commodity.prices.find", indent);
    DEBUG("commodity.prices.find", "price is too young ");
    return none;
  }
  else if (oldest && point.when < *oldest) {
    DEBUG_INDENT("commodity.prices.find", indent);
    DEBUG("commodity.prices.find", "price is too old ");
    return none;
  }
  else {
    DEBUG_INDENT("commodity.prices.find", indent);
    DEBUG("commodity.prices.find",
          "returning price: " << point.when << ", " << point.price);
    return point;
  }
}

optional<price_point_t>
commodity_t::varied_history_t::find_price(const commodity_t&            source,
                                          const optional<commodity_t&>& commodity,
                                          const optional<datetime_t>&   moment,
                                          const optional<datetime_t>&   oldest
#if defined(DEBUG_ON)
                                          , const int indent
#endif
                                          ) const
{
  optional<price_point_t> point;
  optional<datetime_t>    limit = oldest;

#if defined(VERIFY_ON)
  if (commodity) {
    VERIFY(source != *commodity);
    VERIFY(! commodity->has_annotation());
    VERIFY(source.referent() != commodity->referent());
  }
#endif

#if defined(DEBUG_ON)
  DEBUG_INDENT("commodity.prices.find", indent);
  DEBUG("commodity.prices.find", "varied_find_price for: " << source);

  DEBUG_INDENT("commodity.prices.find", indent);
  if (commodity)
    DEBUG("commodity.prices.find", "looking for: commodity '" << *commodity << "'");
  else
    DEBUG("commodity.prices.find", "looking for: any commodity");

  if (moment) {
    DEBUG_INDENT("commodity.prices.find", indent);
    DEBUG("commodity.prices.find", "time index: " << *moment);
  }

  if (oldest) {
    DEBUG_INDENT("commodity.prices.find", indent);
    DEBUG("commodity.prices.find", "only consider prices younger than: " << *oldest);
  }
#endif

  // Either we couldn't find a history for the target commodity, or we
  // couldn't find a price.  In either case, search all histories known
  // to this commodity for a price which we can calculate in terms of
  // the goal commodity.
  price_point_t best;
  bool          found = false;

  foreach (const history_by_commodity_map::value_type& hist, histories) {
    commodity_t& comm(*hist.first);
    if (comm == source)
      continue;

    DEBUG_INDENT("commodity.prices.find", indent + 1);
    DEBUG("commodity.prices.find",
          "searching for price via commodity '" << comm << "'");

    point = hist.second.find_price(moment, limit
#if defined(DEBUG_ON)
                                   , indent + 2
#endif
                                   );
    assert(! point || point->price.commodity() == comm);

    if (point) {
      optional<price_point_t> xlat;

      if (commodity && comm != *commodity) {
        DEBUG_INDENT("commodity.prices.find", indent + 1);
        DEBUG("commodity.prices.find", "looking for translation price");

        xlat = comm.find_price(commodity, moment, limit, true
#if defined(DEBUG_ON)
                               , indent + 2
#endif
                               );
        if (xlat) {
          DEBUG_INDENT("commodity.prices.find", indent + 1);
          DEBUG("commodity.prices.find", "found translated price "
                << xlat->price << " from " << xlat->when);

          point->price = xlat->price * point->price;
          if (xlat->when < point->when) {
            point->when = xlat->when;

            DEBUG_INDENT("commodity.prices.find", indent + 1);
            DEBUG("commodity.prices.find",
                  "adjusting date of result back to " << point->when);
          }
        } else {
          DEBUG_INDENT("commodity.prices.find", indent + 1);
          DEBUG("commodity.prices.find", "saw no translated price there");
          continue;
        }
      }

      assert(! commodity || point->price.commodity() == *commodity);

      DEBUG_INDENT("commodity.prices.find", indent + 1);
      DEBUG("commodity.prices.find",
            "saw a price there: " << point->price << " from " << point->when);

      if (! limit || point->when > *limit) {
        limit = point->when;
        best  = *point;
        found = true;

        DEBUG_INDENT("commodity.prices.find", indent + 1);
        DEBUG("commodity.prices.find",
              "search limit adjusted to " << *limit);
      }
    } else {
      DEBUG_INDENT("commodity.prices.find", indent + 1);
      DEBUG("commodity.prices.find", "saw no price there");
    }
  }

  if (found) {
    DEBUG_INDENT("commodity.prices.find", indent);
    DEBUG("commodity.download",
          "found price " << best.price << " from " << best.when);
    return best;
  }
  return none;
}

optional<commodity_t::history_t&>
commodity_t::varied_history_t::history(const optional<commodity_t&>& commodity)
{
  commodity_t * comm = NULL;
  if (! commodity) {
    if (histories.size() > 1)
      return none;
    comm = (*histories.begin()).first;
  } else {
    comm = &(*commodity);
  }

  history_by_commodity_map::iterator i = histories.find(comm);
  if (i != histories.end())
    return (*i).second;

  return none;
}

optional<price_point_t>
commodity_t::find_price(const optional<commodity_t&>& commodity,
                        const optional<datetime_t>&   moment,
                        const optional<datetime_t>&   oldest,
                        const bool                    nested
#if defined(DEBUG_ON)
                        , const int indent
#endif
                        ) const
{
  if (! has_flags(COMMODITY_WALKED) && base->varied_history) {
    optional<base_t::time_and_commodity_t> pair;
#if defined(VERIFY_ON)
    optional<price_point_t> checkpoint;
    bool found = false;
#endif

    if (! nested) {
      pair = base_t::time_and_commodity_t
        (base_t::optional_time_pair_t(moment, oldest),
         commodity ? &(*commodity) : NULL);
      DEBUG_INDENT("commodity.prices.find", indent);
      DEBUG("commodity.prices.find", "looking for memoized args: "
            << (moment ? format_datetime(*moment) : "NONE") << ", "
            << (oldest ? format_datetime(*oldest) : "NONE") << ", "
            << (commodity ? commodity->symbol() : "NONE"));

      base_t::memoized_price_map::iterator i = base->price_map.find(*pair);
      if (i != base->price_map.end()) {
        DEBUG_INDENT("commodity.prices.find", indent);
        DEBUG("commodity.prices.find", "found! returning: "
              << ((*i).second ? (*i).second->price : amount_t(0L)));
#if defined(VERIFY_ON)
        IF_VERIFY() {
          found = true;
          checkpoint = (*i).second;
        } else
#endif // defined(VERIFY_ON)
        return (*i).second;
      }
    }

    optional<price_point_t> point;

    const_cast<commodity_t&>(*this).add_flags(COMMODITY_WALKED);
    try {
      DEBUG_INDENT("commodity.prices.find", indent);
      DEBUG("commodity.prices.find", "manually finding price...");

      point = base->varied_history->find_price(*this, commodity,
                                               moment, oldest
#if defined(DEBUG_ON)
                                               , indent
#endif
                                               );
    }
    catch (...) {
      const_cast<commodity_t&>(*this).drop_flags(COMMODITY_WALKED);
      throw;
    }
    const_cast<commodity_t&>(*this).drop_flags(COMMODITY_WALKED);

#if defined(VERIFY_ON)
    if (DO_VERIFY() && found) {
      VERIFY(checkpoint == point);
      return checkpoint;
    }
#endif // defined(VERIFY_ON)

    if (! nested && pair) {
      if (base->price_map.size() > base_t::max_price_map_size) {
        DEBUG_INDENT("commodity.prices.find", indent);
        DEBUG("commodity.prices.find",
              "price map has grown too large, clearing it by half");

        for (std::size_t i = 0; i < base_t::max_price_map_size >> 1; i++)
          base->price_map.erase(base->price_map.begin());
      }

      DEBUG_INDENT("commodity.prices.find", indent);
      DEBUG("commodity.prices.find",
            "remembered: " << (point ? point->price : amount_t(0L)));
      base->price_map.insert
        (base_t::memoized_price_map::value_type(*pair, point));
    }
    return point;
  }
  return none;
}    

optional<price_point_t>
commodity_t::check_for_updated_price(const optional<price_point_t>& point,
                                     const optional<datetime_t>&    moment,
                                     const optional<commodity_t&>&  in_terms_of)
{
  if (pool().get_quotes && ! has_flags(COMMODITY_NOMARKET)) {
    bool exceeds_leeway = true;

    if (point) {
      time_duration_t::sec_type seconds_diff;
      if (moment) {
        seconds_diff = (*moment - point->when).total_seconds();
        DEBUG("commodity.download", "moment = " << *moment);
        DEBUG("commodity.download", "slip.moment = " << seconds_diff);
      } else {
        seconds_diff = (TRUE_CURRENT_TIME() - point->when).total_seconds();
        DEBUG("commodity.download", "slip.now = " << seconds_diff);
      }

      DEBUG("commodity.download", "leeway = " << pool().quote_leeway);
      if (seconds_diff < pool().quote_leeway)
        exceeds_leeway = false;
    }

    if (exceeds_leeway) {
      DEBUG("commodity.download",
            "attempting to download a more current quote...");
      if (optional<price_point_t> quote =
          pool().get_commodity_quote(*this, in_terms_of)) {
        if (! in_terms_of ||
            (quote->price.has_commodity() &&
             quote->price.commodity() == *in_terms_of))
          return quote;
      }
    }
  }
  return point;
}

commodity_t::operator bool() const
{
  return this != pool().null_commodity;
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
    case 'e':
      return std::strcmp(buf, "else") == 0;
    case 'f':
      return std::strcmp(buf, "false") == 0;
    case 'i':
      return std::strcmp(buf, "if") == 0;
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
  //   0-9 . , ; : ? ! - + * / ^ & | =
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
    /* 70 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
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
      throw_(amount_error, _("Quoted commodity symbol lacks closing quote"));
  } else {
    char * _p = buf;
    c = static_cast<char>(in.peek());
    while (_p - buf < 255 && in.good() && ! in.eof() && c != '\n') {
      std::size_t    bytes = 0;
      std::ptrdiff_t size  = _p - buf;
      unsigned char  d     = c;

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

      if (bytes > 0) {          // we're looking at a UTF-8 encoding
        for (std::size_t i = 0; i < bytes; i++) {
          in.get(c);
          if (in.bad() || in.eof())
            throw_(amount_error, _("Invalid UTF-8 encoding for commodity name"));
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
            throw_(amount_error, _("Backslash at end of commodity name"));
        }
        *_p++ = c;
      }

      c = static_cast<char>(in.peek());
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
      throw_(amount_error, _("Quoted commodity symbol lacks closing quote"));
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
    throw_(amount_error, _("Failed to parse commodity"));
}

void commodity_t::print(std::ostream& out, bool elide_quotes) const
{
  string sym = symbol();
  if (elide_quotes && has_flags(COMMODITY_STYLE_SEPARATED) &&
      ! sym.empty() && sym[0] == '"' &&
      ! std::strchr(sym.c_str(), ' ')) {
    string subsym(sym, 1, sym.length() - 2);
    if (! all(subsym, is_digit()))
      out << subsym;
    else
      out << sym;
  } else
    out << sym;
}

bool commodity_t::valid() const
{
  if (symbol().empty() && this != pool().null_commodity) {
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

bool commodity_t::compare_by_commodity::operator()(const amount_t * left,
                                                   const amount_t * right) const
{
  commodity_t& leftcomm(left->commodity());
  commodity_t& rightcomm(right->commodity());

  DEBUG("commodity.compare", " left symbol (" << leftcomm << ")");
  DEBUG("commodity.compare", "right symbol (" << rightcomm << ")");

  int cmp = leftcomm.base_symbol().compare(rightcomm.base_symbol());
  if (cmp != 0)
    return cmp < 0;

  if (! leftcomm.has_annotation()) {
    return rightcomm.has_annotation();
  }
  else if (! rightcomm.has_annotation()) {
    return ! leftcomm.has_annotation();
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
      amount_t rightprice(*arightcomm.details.price);

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
      gregorian::date_duration diff =
        *aleftcomm.details.date - *arightcomm.details.date;
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

void to_xml(std::ostream& out, const commodity_t& comm,
            bool commodity_details)
{
  push_xml x(out, "commodity", true);

  out << " flags=\"";
  if (! (comm.has_flags(COMMODITY_STYLE_SUFFIXED)))  out << 'P';
  if (comm.has_flags(COMMODITY_STYLE_SEPARATED))     out << 'S';
  if (comm.has_flags(COMMODITY_STYLE_THOUSANDS))     out << 'T';
  if (comm.has_flags(COMMODITY_STYLE_DECIMAL_COMMA)) out << 'D';
  out << '"';

  x.close_attrs();
  
  {
    push_xml y(out, "symbol");
    out << y.guard(comm.symbol());
  }

  if (commodity_details) {
    if (comm.has_annotation())
      to_xml(out, as_annotated_commodity(comm).details);

    if (comm.varied_history()) {
      push_xml y(out, "varied-history");

      foreach (const commodity_t::history_by_commodity_map::value_type& pair,
               comm.varied_history()->histories) {
        {
          push_xml z(out, "symbol");
          out << y.guard(pair.first->symbol());
        }
        {
          push_xml z(out, "history");

          foreach (const commodity_t::history_map::value_type& inner_pair,
                   pair.second.prices) {
            push_xml w(out, "price-point");
            to_xml(out, inner_pair.first);
            to_xml(out, inner_pair.second);
          }
        }
      }
    }
  }
}

} // namespace ledger
