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
#include "annotate.h"
#include "pool.h"
#include "history.h"
#include "quotes.h"

namespace ledger {

shared_ptr<commodity_pool_t> commodity_pool_t::current_pool;

commodity_pool_t::commodity_pool_t()
  : default_commodity(NULL), keep_base(false),
    quote_leeway(86400), get_quotes(false),
    get_commodity_quote(commodity_quote_from_script)
{
  null_commodity = create("");
  null_commodity->add_flags(COMMODITY_BUILTIN | COMMODITY_NOMARKET);
  TRACE_CTOR(commodity_pool_t, "");
}

commodity_t * commodity_pool_t::create(const string& symbol)
{
  shared_ptr<commodity_t::base_t>
    base_commodity(new commodity_t::base_t(symbol));
  shared_ptr<commodity_t> commodity(new commodity_t(this, base_commodity));

  DEBUG("pool.commodities", "Creating base commodity " << symbol);

  // Create the "qualified symbol" version of this commodity's symbol
  if (commodity_t::symbol_needs_quotes(symbol)) {
    commodity->qualified_symbol = "\"";
    *commodity->qualified_symbol += symbol;
    *commodity->qualified_symbol += "\"";
  }

  DEBUG("pool.commodities", "Creating commodity '" << symbol << "'");

#if DEBUG_ON
  std::pair<commodities_map::iterator, bool> result =
#endif
    commodities.insert(commodities_map::value_type(symbol, commodity));
#if DEBUG_ON
  assert(result.second);
#endif

  commodity_price_history.add_commodity(*commodity.get());

  return commodity.get();
}

commodity_t * commodity_pool_t::find(const string& symbol)
{
  DEBUG("pool.commodities", "Find commodity " << symbol);

  commodities_map::const_iterator i = commodities.find(symbol);
  if (i != commodities.end())
    return (*i).second.get();
  return NULL;
}

commodity_t * commodity_pool_t::find_or_create(const string& symbol)
{
  DEBUG("pool.commodities", "Find-or-create commodity " << symbol);
  if (commodity_t * commodity = find(symbol))
    return commodity;
  return create(symbol);
}

commodity_t * commodity_pool_t::alias(const string& name, commodity_t& referent)
{
  commodities_map::const_iterator i = commodities.find(referent.base_symbol());
  assert(i != commodities.end());

  std::pair<commodities_map::iterator, bool> result
    = commodities.insert(commodities_map::value_type(name, (*i).second));
  assert(result.second);

  return (*result.first).second.get();
}

commodity_t *
commodity_pool_t::create(const string& symbol, const annotation_t& details)
{
  DEBUG("pool.commodities", "commodity_pool_t::create[ann] "
        << "symbol " << symbol << std::endl << details);

  if (details)
    return create(*find_or_create(symbol), details);
  else
    return create(symbol);
}

commodity_t *
commodity_pool_t::find(const string& symbol, const annotation_t& details)
{
  DEBUG("pool.commodities", "commodity_pool_t::find[ann] "
        << "symbol " << symbol << std::endl << details);

  annotated_commodities_map::const_iterator i =
    annotated_commodities.find
    (annotated_commodities_map::key_type(symbol, details));
  if (i != annotated_commodities.end()) {
    DEBUG("pool.commodities", "commodity_pool_t::find[ann] found "
          << "symbol " << (*i).second->base_symbol() << std::endl
          << as_annotated_commodity(*(*i).second.get()).details);
    return (*i).second.get();
  } else {
    return NULL;
  }
}

commodity_t *
commodity_pool_t::find_or_create(const string& symbol,
                                 const annotation_t& details)
{
  DEBUG("pool.commodities", "commodity_pool_t::find_or_create[ann] "
        << "symbol " << symbol << std::endl << details);

  if (details) {
    if (commodity_t * ann_comm = find(symbol, details)) {
      assert(ann_comm->annotated && as_annotated_commodity(*ann_comm).details);
      return ann_comm;
    } else {
      return create(symbol, details);
    }
  } else {
    return find_or_create(symbol);
  }
}

commodity_t *
commodity_pool_t::find_or_create(commodity_t& comm, const annotation_t& details)
{
  DEBUG("pool.commodities", "commodity_pool_t::find_or_create[ann:comm] "
        << "symbol " << comm.base_symbol() << std::endl << details);

  if (details) {
    if (commodity_t * ann_comm = find(comm.base_symbol(), details)) {
      assert(ann_comm->annotated && as_annotated_commodity(*ann_comm).details);
      return ann_comm;
    } else {
      return create(comm, details);
    }
  } else {
    return &comm;
  }
}

annotated_commodity_t *
commodity_pool_t::create(commodity_t&        comm,
                         const annotation_t& details)
{
  DEBUG("pool.commodities", "commodity_pool_t::create[ann:comm] "
        << "symbol " << comm.base_symbol() << std::endl << details);

  assert(comm);
  assert(! comm.has_annotation());
  assert(details);

  shared_ptr<annotated_commodity_t>
    commodity(new annotated_commodity_t(&comm, details));

  comm.add_flags(COMMODITY_SAW_ANNOTATED);
  if (details.price) {
    if (details.has_flags(ANNOTATION_PRICE_FIXATED))
      comm.add_flags(COMMODITY_SAW_ANN_PRICE_FIXATED);
    else
      comm.add_flags(COMMODITY_SAW_ANN_PRICE_FLOAT);
  }

  DEBUG("pool.commodities", "Creating annotated commodity "
        << "symbol " << commodity->base_symbol()
        << std::endl << details);

#if DEBUG_ON
  std::pair<annotated_commodities_map::iterator, bool> result =
#endif
    annotated_commodities.insert(annotated_commodities_map::value_type
                                 (annotated_commodities_map::key_type
                                  (comm.base_symbol(), details), commodity));
#if DEBUG_ON
  assert(result.second);
#endif

  return commodity.get();
}

void commodity_pool_t::exchange(commodity_t&      commodity,
                                const amount_t&   per_unit_cost,
                                const datetime_t& moment)
{
  DEBUG("commodity.prices.add", "exchanging commodity " << commodity
        << " at per unit cost " << per_unit_cost << " on " << moment);

  commodity_t& base_commodity
    (commodity.annotated ?
     as_annotated_commodity(commodity).referent() : commodity);

  base_commodity.add_price(moment, per_unit_cost);
}

cost_breakdown_t
commodity_pool_t::exchange(const amount_t&             amount,
                           const amount_t&             cost,
                           const bool                  is_per_unit,
                           const bool                  add_price,
                           const optional<datetime_t>& moment,
                           const optional<string>&     tag)
{
  DEBUG("commodity.prices.add", "exchange: " << amount << " for " << cost);
  DEBUG("commodity.prices.add", "exchange: is-per-unit   = " << is_per_unit);
#if DEBUG_ON
  if (moment)
    DEBUG("commodity.prices.add", "exchange: moment        = " << *moment);
  if (tag)
    DEBUG("commodity.prices.add", "exchange: tag           = " << *tag);
#endif

  commodity_t& commodity(amount.commodity());

  annotation_t * current_annotation = NULL;
  if (commodity.annotated)
    current_annotation = &as_annotated_commodity(commodity).details;

  amount_t per_unit_cost =
    (is_per_unit || amount.is_realzero()) ? cost.abs() : (cost / amount).abs();

  if (! cost.has_commodity())
    per_unit_cost.clear_commodity();

  DEBUG("commodity.prices.add", "exchange: per-unit-cost = " << per_unit_cost);

  // Do not record commodity exchanges where amount's commodity has a
  // fixated price, since this does not establish a market value for the
  // base commodity.
  if (add_price &&
      ! per_unit_cost.is_realzero() &&
      (current_annotation == NULL ||
       ! (current_annotation->price &&
          current_annotation->has_flags(ANNOTATION_PRICE_FIXATED))) &&
      commodity.referent() != per_unit_cost.commodity().referent()) {
    exchange(commodity, per_unit_cost, moment ? *moment : CURRENT_TIME());
  }

  cost_breakdown_t breakdown;
  breakdown.final_cost = ! is_per_unit ? cost : cost * amount.abs();

  DEBUG("commodity.prices.add",
        "exchange: final-cost    = " << breakdown.final_cost);

  if (current_annotation && current_annotation->price)
    breakdown.basis_cost
      = (*current_annotation->price * amount).unrounded();
  else
    breakdown.basis_cost = breakdown.final_cost;

  DEBUG("commodity.prices.add",
        "exchange: basis-cost    = " << breakdown.basis_cost);

  annotation_t annotation(per_unit_cost, moment ?
                          moment->date() : optional<date_t>(), tag);

  annotation.add_flags(ANNOTATION_PRICE_CALCULATED);
  if (current_annotation &&
      current_annotation->has_flags(ANNOTATION_PRICE_FIXATED))
    annotation.add_flags(ANNOTATION_PRICE_FIXATED);
  if (moment)
    annotation.add_flags(ANNOTATION_DATE_CALCULATED);
  if (tag)
    annotation.add_flags(ANNOTATION_TAG_CALCULATED);

  breakdown.amount = amount_t(amount, annotation);

  DEBUG("commodity.prices.add",
        "exchange: amount        = " << breakdown.amount);

  return breakdown;
}

optional<std::pair<commodity_t *, price_point_t> >
commodity_pool_t::parse_price_directive
  (char * line, bool do_not_add_price, bool no_date)
{
  char * date_field_ptr = line;
  char * time_field_ptr = next_element(date_field_ptr);
  if (! time_field_ptr) return none;
  string date_field = date_field_ptr;

  char *     symbol_and_price;
  datetime_t datetime;
  string     symbol;

  if (! no_date && std::isdigit(time_field_ptr[0])) {
    symbol_and_price = next_element(time_field_ptr);
    if (! symbol_and_price) return none;

    datetime = parse_datetime(date_field + " " + time_field_ptr);
  }
  else if (! no_date && std::isdigit(date_field_ptr[0])) {
    symbol_and_price = time_field_ptr;
    datetime = datetime_t(parse_date(date_field));
  }
  else {
    symbol = date_field_ptr;
    symbol_and_price = time_field_ptr;
    datetime = CURRENT_TIME();
  }

  if (symbol.empty())
    commodity_t::parse_symbol(symbol_and_price, symbol);

  price_point_t point;
  point.when = datetime;
  point.price.parse(symbol_and_price, PARSE_NO_MIGRATE);
  VERIFY(point.price.valid());

  DEBUG("commodity.download", "Looking up symbol: " << symbol);
  if (commodity_t * commodity = find_or_create(symbol)) {
    DEBUG("commodity.download", "Adding price for " << symbol << ": "
          << point.when << " " << point.price);
    if (! do_not_add_price)
      commodity->add_price(point.when, point.price, true);
    commodity->add_flags(COMMODITY_KNOWN);
    return std::pair<commodity_t *, price_point_t>(commodity, point);
  }

  return none;
}

commodity_t *
commodity_pool_t::parse_price_expression(const std::string&          str,
                                         const bool                  add_prices,
                                         const optional<datetime_t>& moment)
{
  scoped_array<char> buf(new char[str.length() + 1]);

  std::strcpy(buf.get(), str.c_str());

  char * price = std::strchr(buf.get(), '=');
  if (price)
    *price++ = '\0';

  if (commodity_t * commodity = find_or_create(trim_ws(buf.get()))) {
    if (price && add_prices) {
      for (char * p = std::strtok(price, ";");
           p;
           p = std::strtok(NULL, ";")) {
        commodity->add_price(moment ? *moment : CURRENT_TIME(), amount_t(p));
      }
    }
    return commodity;
  }
  return NULL;
}

} // namespace ledger
