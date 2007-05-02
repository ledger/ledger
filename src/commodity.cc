/**
 * @file   commodity.cc
 * @author John Wiegley
 * @date   Thu Apr 26 15:19:46 2007
 * 
 * @brief  Types for dealing with commodities.
 * 
 * This file defines member functions for flavors of commodity_t.
 */

/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#include "amount.h"

namespace ledger {

#ifndef THREADSAFE
base_commodities_map commodity_base_t::commodities;

commodity_base_t::updater_t * commodity_base_t::updater = NULL;

commodities_map	    commodity_t::commodities;
commodities_array * commodity_t::commodities_by_ident;
bool		    commodity_t::commodities_sorted = false;
commodity_t *	    commodity_t::null_commodity;
commodity_t *	    commodity_t::default_commodity  = NULL;
#endif

void commodity_base_t::add_price(const moment_t& date,
				 const amount_t& price)
{
  if (! history)
    history = new history_t;

  history_map::iterator i = history->prices.find(date);
  if (i != history->prices.end()) {
    (*i).second = price;
  } else {
    std::pair<history_map::iterator, bool> result
      = history->prices.insert(history_pair(date, price));
    assert(result.second);
  }
}

bool commodity_base_t::remove_price(const moment_t& date)
{
  if (history) {
    history_map::size_type n = history->prices.erase(date);
    if (n > 0) {
      if (history->prices.empty())
	history = NULL;
      return true;
    }
  }
  return false;
}

commodity_base_t * commodity_base_t::create(const string& symbol)
{
  commodity_base_t * commodity = new commodity_base_t(symbol);

  DEBUG("amounts.commodities", "Creating base commodity " << symbol);

  std::pair<base_commodities_map::iterator, bool> result
    = commodities.insert(base_commodities_pair(symbol, commodity));
  assert(result.second);

  return commodity;
}

bool commodity_t::needs_quotes(const string& symbol)
{
  for (const char * p = symbol.c_str(); *p; p++)
    if (std::isspace(*p) || std::isdigit(*p) || *p == '-' || *p == '.')
      return true;

  return false;
}

bool commodity_t::valid() const
{
  if (symbol().empty() && this != null_commodity) {
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

commodity_t * commodity_t::create(const string& symbol)
{
  std::auto_ptr<commodity_t> commodity(new commodity_t);

  commodity->base = commodity_base_t::create(symbol);

  if (needs_quotes(symbol)) {
    commodity->qualified_symbol = "\"";
    commodity->qualified_symbol += symbol;
    commodity->qualified_symbol += "\"";
  } else {
    commodity->qualified_symbol = symbol;
  }

  DEBUG("amounts.commodities",
	 "Creating commodity " << commodity->qualified_symbol);

  std::pair<commodities_map::iterator, bool> result
    = commodities.insert(commodities_pair(symbol, commodity.get()));
  if (! result.second)
    return NULL;

  commodity->ident = commodities_by_ident->size();
  commodities_by_ident->push_back(commodity.get());

  // Start out the new commodity with the default commodity's flags
  // and precision, if one has been defined.
  if (default_commodity)
    commodity->drop_flags(COMMODITY_STYLE_THOUSANDS |
			  COMMODITY_STYLE_NOMARKET);

  return commodity.release();
}

commodity_t * commodity_t::find_or_create(const string& symbol)
{
  DEBUG("amounts.commodities", "Find-or-create commodity " << symbol);

  commodity_t * commodity = find(symbol);
  if (commodity)
    return commodity;
  return create(symbol);
}

commodity_t * commodity_t::find(const string& symbol)
{
  DEBUG("amounts.commodities", "Find commodity " << symbol);

  commodities_map::const_iterator i = commodities.find(symbol);
  if (i != commodities.end())
    return (*i).second;
  return NULL;
}

amount_t commodity_base_t::value(const moment_t& moment)
{
  moment_t age;
  amount_t price;

  if (history) {
    assert(history->prices.size() > 0);

    if (! is_valid_moment(moment)) {
      history_map::reverse_iterator r = history->prices.rbegin();
      age   = (*r).first;
      price = (*r).second;
    } else {
      history_map::iterator i = history->prices.lower_bound(moment);
      if (i == history->prices.end()) {
	history_map::reverse_iterator r = history->prices.rbegin();
	age   = (*r).first;
	price = (*r).second;
      } else {
	age = (*i).first;
	if (moment != age) {
	  if (i != history->prices.begin()) {
	    --i;
	    age	  = (*i).first;
	    price = (*i).second;
	  } else {
	    age   = moment_t();
	  }
	} else {
	  price = (*i).second;
	}
      }
    }
  }

  if (updater && ! (flags & COMMODITY_STYLE_NOMARKET))
    (*updater)(*this, moment, age,
	       (history && history->prices.size() > 0 ?
		(*history->prices.rbegin()).first : moment_t()), price);

  return price;
}

bool annotated_commodity_t::operator==(const commodity_t& comm) const
{
  // If the base commodities don't match, the game's up.
  if (base != comm.base)
    return false;

  if (price &&
      (! comm.annotated ||
       price != static_cast<const annotated_commodity_t&>(comm).price))
    return false;

  if (date &&
      (! comm.annotated ||
       date != static_cast<const annotated_commodity_t&>(comm).date))
    return false;

  if (tag &&
      (! comm.annotated ||
       tag != static_cast<const annotated_commodity_t&>(comm).tag))
    return false;

  return true;
}

void
annotated_commodity_t::write_annotations(std::ostream&		   out,
					 const optional<amount_t>& price,
					 const optional<moment_t>& date,
					 const optional<string>&   tag)
{
  if (price)
    out << " {" << *price << '}';

  if (date)
    out << " [" << *date << ']';

  if (tag)
    out << " (" << *tag << ')';
}

commodity_t *
annotated_commodity_t::create(const commodity_t&	comm,
			      const optional<amount_t>& price,
			      const optional<moment_t>&	date,
			      const optional<string>&	tag,
			      const string&		mapping_key)
{
  std::auto_ptr<annotated_commodity_t> commodity(new annotated_commodity_t);

  // Set the annotated bits
  commodity->price = price;
  commodity->date  = date;
  commodity->tag   = tag;

  commodity->ptr = &comm;
  assert(commodity->ptr);
  commodity->base = comm.base;
  assert(commodity->base);

  commodity->qualified_symbol = comm.symbol();

  DEBUG("amounts.commodities", "Creating annotated commodity "
	<< "symbol " << commodity->symbol()
	<< " key "   << mapping_key << std::endl
	<< "  price " << (price ? price->to_string() : "NONE") << " "
	<< "  date "  << (date  ? *date : moment_t()) << " "
	<< "  tag "   << (tag   ? *tag  : "NONE"));

  // Add the fully annotated name to the map, so that this symbol may
  // quickly be found again.
  std::pair<commodities_map::iterator, bool> result
    = commodities.insert(commodities_pair(mapping_key, commodity.get()));
  if (! result.second)
    return NULL;

  commodity->ident = commodities_by_ident->size();
  commodities_by_ident->push_back(commodity.get());

  return commodity.release();
}

namespace {
  string make_qualified_name(const commodity_t&	       comm,
			     const optional<amount_t>& price,
			     const optional<moment_t>& date,
			     const optional<string>&   tag)
  {
    if (price && price->sign() < 0)
      throw_(amount_error, "A commodity's price may not be negative");

    std::ostringstream name;

    comm.write(name);
    annotated_commodity_t::write_annotations(name, price, date, tag);

    DEBUG("amounts.commodities", "make_qualified_name for "
	  << comm.qualified_symbol << std::endl
	  << "  price " << (price ? price->to_string() : "NONE") << " "
	  << "  date "  << (date  ? *date : moment_t()) << " "
	  << "  tag "   << (tag   ? *tag  : "NONE"));

    DEBUG("amounts.commodities", "qualified_name is " << name.str());

    return name.str();
  }
}

commodity_t *
annotated_commodity_t::find_or_create(const commodity_t&	comm,
				      const optional<amount_t>& price,
				      const optional<moment_t>& date,
				      const optional<string>&	tag)
{
  string name = make_qualified_name(comm, price, date, tag);

  commodity_t * ann_comm = commodity_t::find(name);
  if (ann_comm) {
    assert(ann_comm->annotated);
    return ann_comm;
  }
  return create(comm, price, date, tag, name);
}

bool compare_amount_commodities::operator()(const amount_t * left,
					    const amount_t * right) const
{
  commodity_t& leftcomm(left->commodity());
  commodity_t& rightcomm(right->commodity());

  int cmp = leftcomm.base_symbol().compare(rightcomm.base_symbol());
  if (cmp != 0)
    return cmp < 0;

  if (! leftcomm.annotated) {
    assert(rightcomm.annotated);
    return true;
  }
  else if (! rightcomm.annotated) {
    assert(leftcomm.annotated);
    return false;
  }
  else {
    annotated_commodity_t& aleftcomm(static_cast<annotated_commodity_t&>(leftcomm));
    annotated_commodity_t& arightcomm(static_cast<annotated_commodity_t&>(rightcomm));

    if (! aleftcomm.price && arightcomm.price)
      return true;
    if (aleftcomm.price && ! arightcomm.price)
      return false;

    if (aleftcomm.price && arightcomm.price) {
      amount_t leftprice(*aleftcomm.price);
      leftprice.in_place_reduce();
      amount_t rightprice(*arightcomm.price);
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

    if (! aleftcomm.date && arightcomm.date)
      return true;
    if (aleftcomm.date && ! arightcomm.date)
      return false;

    if (aleftcomm.date && arightcomm.date) {
      duration_t diff = *aleftcomm.date - *arightcomm.date;
      return diff.is_negative();
    }

    if (! aleftcomm.tag && arightcomm.tag)
      return true;
    if (aleftcomm.tag && ! arightcomm.tag)
      return false;

    if (aleftcomm.tag && arightcomm.tag)
      return *aleftcomm.tag < *arightcomm.tag;

    assert(false);
    return true;
  }
}

} // namespace ledger
