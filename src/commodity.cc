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

#include <system.hh>

#include "amount.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"
#include "scope.h"

namespace ledger {

bool commodity_t::decimal_comma_by_default = false;
bool commodity_t::time_colon_by_default = false;

void commodity_t::add_price(const datetime_t& date, const amount_t& price, const bool reflexive) {
  if (reflexive) {
    DEBUG("history.find", "Marking " << price.commodity().symbol() << " as a primary commodity");
    price.commodity().add_flags(COMMODITY_PRIMARY);
  } else {
    DEBUG("history.find", "Marking " << symbol() << " as a primary commodity");
    add_flags(COMMODITY_PRIMARY);
  }

  DEBUG("history.find", "Adding price: " << symbol() << " for " << price << " on " << date);

  pool().commodity_price_history.add_price(referent(), date, price);

  base->price_map.clear(); // a price was added, invalid the map
}

void commodity_t::remove_price(const datetime_t& date, commodity_t& commodity) {
  pool().commodity_price_history.remove_price(referent(), commodity, date);

  DEBUG("history.find", "Removing price: " << symbol() << " on " << date);

  base->price_map.clear(); // a price was added, invalid the map
}

void commodity_t::map_prices(function<void(datetime_t, const amount_t&)> fn,
                             const datetime_t& moment, const datetime_t& _oldest,
                             bool bidirectionally) {
  datetime_t when;
  if (!moment.is_not_a_date_time())
    when = moment;
  else if (epoch)
    when = *epoch;
  else
    when = CURRENT_TIME();

  pool().commodity_price_history.map_prices(fn, referent(), when, _oldest, bidirectionally);
}

optional<price_point_t> commodity_t::find_price_from_expr(expr_t& expr,
                                                          const commodity_t* commodity,
                                                          const datetime_t& moment) const {
#if DEBUG_ON
  if (SHOW_DEBUG("commodity.price.find")) {
    ledger::_log_buffer << "valuation expr: ";
    expr.dump(ledger::_log_buffer);
    DEBUG("commodity.price.find", "");
  }
#endif
  value_t result(expr.calc(*scope_t::default_scope));

  if (is_expr(result)) {
    value_t call_args;

    call_args.push_back(string_value(base_symbol()));
    call_args.push_back(moment);
    if (commodity)
      call_args.push_back(string_value(commodity->symbol()));

    result = as_expr(result)->call(call_args, *scope_t::default_scope);
  }

  return price_point_t(moment, result.to_amount());
}

optional<price_point_t> commodity_t::find_price(const commodity_t* commodity,
                                                const datetime_t& moment,
                                                const datetime_t& oldest) const {
  DEBUG("commodity.price.find", "commodity_t::find_price(" << symbol() << ")");

  const commodity_t* target = NULL;
  if (commodity)
    target = commodity;
  else if (pool().default_commodity)
    target = &*pool().default_commodity;

  if (target && this == target)
    return none;

  base_t::memoized_price_entry entry(moment, oldest, commodity ? commodity : NULL);

  DEBUG("commodity.price.find",
        "looking for memoized args: "
            << (!moment.is_not_a_date_time() ? format_datetime(moment) : "NONE") << ", "
            << (!oldest.is_not_a_date_time() ? format_datetime(oldest) : "NONE") << ", "
            << (commodity ? commodity->symbol() : "NONE"));
  {
    base_t::memoized_price_map::iterator i = base->price_map.find(entry);
    if (i != base->price_map.end()) {
      DEBUG("commodity.price.find",
            "found! returning: " << ((*i).second ? (*i).second->price : amount_t(0L)));
      return (*i).second;
    }
  }

  datetime_t when;
  if (!moment.is_not_a_date_time())
    when = moment;
  else if (epoch)
    when = *epoch;
  else
    when = CURRENT_TIME();

  if (base->value_expr)
    return find_price_from_expr(*base->value_expr, commodity, when);

  optional<price_point_t> point(
      target ? pool().commodity_price_history.find_price(referent(), *target, when, oldest)
             : pool().commodity_price_history.find_price(referent(), when, oldest));

  // Record this price point in the memoization map
  if (base->price_map.size() > base_t::max_price_map_size) {
    DEBUG("history.find", "price map has grown too large, clearing it by half");
    for (std::size_t i = 0; i < base_t::max_price_map_size >> 1; i++)
      base->price_map.erase(base->price_map.begin());
  }

  DEBUG("history.find", "remembered: " << (point ? point->price : amount_t(0L)));
  base->price_map.insert(base_t::memoized_price_map::value_type(entry, point));

  return point;
}

optional<price_point_t> commodity_t::check_for_updated_price(const optional<price_point_t>& point,
                                                             const datetime_t& moment,
                                                             const commodity_t* in_terms_of) {
  if (pool().get_quotes && !has_flags(COMMODITY_NOMARKET)) {
    bool exceeds_leeway = true;

    if (point) {
      time_duration_t::sec_type seconds_diff;
      if (!moment.is_not_a_date_time()) {
        seconds_diff = (moment - point->when).total_seconds();
        DEBUG("commodity.download", "moment = " << moment);
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
      DEBUG("commodity.download", "attempting to download a more current quote...");
      if (optional<price_point_t> quote = pool().get_commodity_quote(referent(), in_terms_of)) {
        if (!in_terms_of ||
            (quote->price.has_commodity() && quote->price.commodity_ptr() == in_terms_of))
          return quote;
      }
    }
  }
  return point;
}

commodity_t& commodity_t::nail_down(const expr_t& expr) {
  annotation_t new_details;

  new_details.value_expr = expr;
  new_details.add_flags(ANNOTATION_VALUE_EXPR_CALCULATED);

  return *pool().find_or_create(symbol(), new_details);
}

commodity_t::operator bool() const {
  return this != pool().null_commodity;
}

namespace {
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

bool is_reserved_token(const char* buf) {
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
} // namespace

bool commodity_t::symbol_needs_quotes(const string& symbol) {
  foreach (char ch, symbol)
    if (invalid_chars[static_cast<unsigned char>(ch)])
      return true;

  return false;
}

void commodity_t::parse_symbol(std::istream& in, string& symbol) {
  std::istream::pos_type pos = in.tellg();

  char buf[256];
  int c = peek_next_nonws(in);
  if (c == '"') {
    in.get();
    READ_INTO(in, buf, 255, c, c != '"');
    if (c == '"')
      in.get();
    else
      throw_(amount_error, _("Quoted commodity symbol lacks closing quote"));
  } else {
    char* _p = buf;
    while (_p - buf < 255 && in.good() && !in.eof() && !invalid_chars[c]) {
      c = in.get();
      if (c == '\\') {
        c = in.get();
        if (in.eof())
          throw_(amount_error, _("Backslash at end of commodity name"));
      }
      *_p++ = c;
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

void commodity_t::parse_symbol(char*& p, string& symbol) {
  if (*p == '"') {
    char* q = std::strchr(p + 1, '"');
    if (!q)
      throw_(amount_error, _("Quoted commodity symbol lacks closing quote"));
    symbol = string(p + 1, 0, static_cast<std::string::size_type>(q - p - 1));
    p = q + 2;
  } else {
    char* q = next_element(p);
    symbol = p;
    if (q)
      p = q;
    else
      p += symbol.length();
  }
  if (symbol.empty())
    throw_(amount_error, _("Failed to parse commodity"));
}

void commodity_t::print(std::ostream& out, bool elide_quotes, bool) const {
  string sym = symbol();
  if (elide_quotes && has_flags(COMMODITY_STYLE_SEPARATED) && !sym.empty() && sym[0] == '"' &&
      !std::strchr(sym.c_str(), ' ')) {
    string subsym(sym, 1, sym.length() - 2);
    if (!all(subsym, is_digit()))
      out << subsym;
    else
      out << sym;
  } else
    out << sym;
}

bool commodity_t::valid() const {
  if (symbol().empty() && this != pool().null_commodity) {
    DEBUG("ledger.validate", "commodity_t: symbol().empty() && this != null_commodity");
    return false;
  }

  if (annotated && !base) {
    DEBUG("ledger.validate", "commodity_t: annotated && ! base");
    return false;
  }

  if (precision() > 16) {
    DEBUG("ledger.validate", "commodity_t: precision() > 16");
    return false;
  }

  return true;
}

int commodity_t::compare_by_commodity::operator()(const amount_t* left,
                                                  const amount_t* right) const {
  commodity_t& leftcomm(left->commodity());
  commodity_t& rightcomm(right->commodity());

  DEBUG("commodity.compare", " left symbol (" << leftcomm << ")");
  DEBUG("commodity.compare", "right symbol (" << rightcomm << ")");

  int cmp = leftcomm.base_symbol().compare(rightcomm.base_symbol());
  if (cmp != 0) {
    DEBUG("commodity.compare", "symbol is <");
    return cmp;
  }

  if (!leftcomm.has_annotation() && rightcomm.has_annotation()) {
    DEBUG("commodity.compare", "left has no annotation, right does");
    return -1;
  } else if (leftcomm.has_annotation() && !rightcomm.has_annotation()) {
    DEBUG("commodity.compare", "right has no annotation, left does");
    return 1;
  } else if (!leftcomm.has_annotation() && !rightcomm.has_annotation()) {
    DEBUG("commodity.compare", "there are no annotations, commodities match");
    return 0;
  }

  annotated_commodity_t& aleftcomm(static_cast<annotated_commodity_t&>(leftcomm));
  annotated_commodity_t& arightcomm(static_cast<annotated_commodity_t&>(rightcomm));

  if (!aleftcomm.details.acquisition_cost && arightcomm.details.acquisition_cost) {
    DEBUG("commodity.compare", "left has no acquisition cost, right does");
    return -1;
  }
  if (aleftcomm.details.acquisition_cost && !arightcomm.details.acquisition_cost) {
    DEBUG("commodity.compare", "right has no acquisition cost, left does");
    return 1;
  }

  if (aleftcomm.details.acquisition_cost && arightcomm.details.acquisition_cost) {
    amount_t leftprice(*aleftcomm.details.acquisition_cost);
    amount_t rightprice(*arightcomm.details.acquisition_cost);

    if (leftprice.commodity() != rightprice.commodity()) {
      // Since we have two different amounts, there's really no way to
      // establish a true sorting order; we'll just do it based on the
      // numerical values, before falling back to comparing the prices
      // with their original commodity.
      amount_t leftpricenumeric(leftprice);
      amount_t rightpricenumeric(rightprice);
      leftpricenumeric.clear_commodity();
      rightpricenumeric.clear_commodity();
      DEBUG("commodity.compare", "both have price, commodities don't match, recursing");
      int cmp2 = commodity_t::compare_by_commodity()(&leftpricenumeric, &rightpricenumeric);
      if (cmp2 != 0) {
        DEBUG("commodity.compare", "recursion found a disparity");
        return cmp2;
      }
      DEBUG("commodity.compare", "recursion found no difference, comparing prices with commodity");
      return commodity_t::compare_by_commodity()(&leftprice, &rightprice);
    } else {
      if (leftprice < rightprice) {
        DEBUG("commodity.compare", "left price is less");
        return -1;
      } else if (leftprice > rightprice) {
        DEBUG("commodity.compare", "left price is more");
        return 1;
      }
    }
  }

  if (!aleftcomm.details.date && arightcomm.details.date) {
    DEBUG("commodity.compare", "left has no date, right does");
    return -1;
  }
  if (aleftcomm.details.date && !arightcomm.details.date) {
    DEBUG("commodity.compare", "right has no date, left does");
    return 1;
  }

  if (aleftcomm.details.date && arightcomm.details.date) {
    gregorian::date_duration diff = *aleftcomm.details.date - *arightcomm.details.date;
    DEBUG("commodity.compare", "both have dates, comparing on difference");
    if (diff.is_negative()) {
      DEBUG("commodity.compare", "dates differ");
      return -1;
    }

    gregorian::date_duration diff2 = *arightcomm.details.date - *aleftcomm.details.date;
    if (diff2.is_negative()) {
      DEBUG("commodity.compare", "dates differ");
      return 1;
    }
  }

  if (!aleftcomm.details.tag && arightcomm.details.tag) {
    DEBUG("commodity.compare", "left has no tag, right does");
    return -1;
  }
  if (aleftcomm.details.tag && !arightcomm.details.tag) {
    DEBUG("commodity.compare", "right has no tag, left does");
    return 1;
  }

  if (aleftcomm.details.tag && arightcomm.details.tag) {
    DEBUG("commodity.compare", "both have tags, comparing lexically");
    if (*aleftcomm.details.tag < *arightcomm.details.tag)
      return -1;
    else if (*aleftcomm.details.tag > *arightcomm.details.tag)
      return 1;
  }

  if (!aleftcomm.details.value_expr && arightcomm.details.value_expr) {
    DEBUG("commodity.compare", "left has no value expr, right does");
    return -1;
  }
  if (aleftcomm.details.value_expr && !arightcomm.details.value_expr) {
    DEBUG("commodity.compare", "right has no value expr, left does");
    return 1;
  }

  if (aleftcomm.details.value_expr && arightcomm.details.value_expr) {
    DEBUG("commodity.compare", "both have value exprs, comparing text reprs");
    return (aleftcomm.details.value_expr->text() < arightcomm.details.value_expr->text());
  }

  // Compare semantic flags (ANNOTATION_COST_FIXATED)
  unsigned int left_flags = aleftcomm.details.flags() & ANNOTATION_SEMANTIC_FLAGS;
  unsigned int right_flags = arightcomm.details.flags() & ANNOTATION_SEMANTIC_FLAGS;
  if (left_flags < right_flags) {
    DEBUG("commodity.compare", "left has fewer semantic flags");
    return -1;
  }
  if (left_flags > right_flags) {
    DEBUG("commodity.compare", "left has more semantic flags");
    return 1;
  }

  // All attributes match - the commodities are equal
  DEBUG("commodity.compare", "all attributes match, commodities are equal");
  return 0;
}

void put_commodity(property_tree::ptree& st, const commodity_t& comm, bool commodity_details) {
  std::string flags;
  if (!(comm.has_flags(COMMODITY_STYLE_SUFFIXED)))
    flags += 'P';
  if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
    flags += 'S';
  if (comm.has_flags(COMMODITY_STYLE_THOUSANDS))
    flags += 'T';
  if (comm.has_flags(COMMODITY_STYLE_DECIMAL_COMMA))
    flags += 'D';
  st.put("<xmlattr>.flags", flags);

  st.put("symbol", comm.symbol());

  if (commodity_details && comm.has_annotation())
    put_annotation(st.put("annotation", ""), as_annotated_commodity(comm).details);
}

} // namespace ledger
