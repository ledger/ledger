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

/**
 * @file   commodity.cc
 * @author John Wiegley
 * @date   Thu Apr 26 15:19:46 2007
 * 
 * @brief  Types for dealing with commodities.
 * 
 * This file defines member functions for flavors of commodity_t.
 */

#include "amount.h"

namespace ledger {

void commodity_t::add_price(const moment_t& date,
			    const amount_t& price)
{
  if (! base->history)
    base->history = history_t();

  history_map::iterator i = base->history->prices.find(date);
  if (i != base->history->prices.end()) {
    (*i).second = price;
  } else {
    std::pair<history_map::iterator, bool> result
      = base->history->prices.insert(history_pair(date, price));
    assert(result.second);
  }
}

bool commodity_t::remove_price(const moment_t& date)
{
  if (base->history) {
    history_map::size_type n = base->history->prices.erase(date);
    if (n > 0) {
      if (base->history->prices.empty())
	base->history.reset();
      return true;
    }
  }
  return false;
}

optional<amount_t> commodity_t::value(const optional<moment_t>& moment)
{
  optional<moment_t> age;
  optional<amount_t> price;

  if (base->history) {
    assert(base->history->prices.size() > 0);

    if (! moment) {
      history_map::reverse_iterator r = base->history->prices.rbegin();
      age   = (*r).first;
      price = (*r).second;
    } else {
      history_map::iterator i = base->history->prices.lower_bound(*moment);
      if (i == base->history->prices.end()) {
	history_map::reverse_iterator r = base->history->prices.rbegin();
	age   = (*r).first;
	price = (*r).second;
      } else {
	age = (*i).first;
	if (*moment != *age) {
	  if (i != base->history->prices.begin()) {
	    --i;
	    age	  = (*i).first;
	    price = (*i).second;
	  } else {
	    age   = optional<moment_t>();
	  }
	} else {
	  price = (*i).second;
	}
      }
    }
  }

  if (! (flags() & COMMODITY_STYLE_NOMARKET)) {
    if (optional<amount_t> quote = parent().get_quote
	(*this, age, moment,
	 (base->history && base->history->prices.size() > 0 ?
	  (*base->history->prices.rbegin()).first : optional<moment_t>())))
      return *quote;
  }
  return price;
}

commodity_t::operator bool() const
{
  return this != parent().null_commodity;
}

annotated_commodity_t& commodity_t::as_annotated()
{
  assert(annotated);
  return *polymorphic_downcast<annotated_commodity_t *>(this);
}

const annotated_commodity_t& commodity_t::as_annotated() const
{
  assert(annotated);
  return *polymorphic_downcast<const annotated_commodity_t *>(this);
}

bool commodity_t::symbol_needs_quotes(const string& symbol)
{
  for (const char * p = symbol.c_str(); *p; p++)
    if (std::isspace(*p) || std::isdigit(*p) || *p == '-' || *p == '.')
      return true;

  return false;
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

bool annotated_commodity_t::operator==(const commodity_t& comm) const
{
  // If the base commodities don't match, the game's up.
  if (base != comm.base)
    return false;

  assert(annotated);
  if (! comm.annotated)
    return false;

  if (details != comm.as_annotated().details)
    return false;

  return true;
}

void
annotated_commodity_t::write_annotations(std::ostream&       out,
					 const annotation_t& info)
{
  if (info.price)
    out << " {" << *info.price << '}';

  if (info.date)
    out << " [" << *info.date << ']';

  if (info.tag)
    out << " (" << *info.tag << ')';
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
      duration_t diff = *aleftcomm.details.date - *arightcomm.details.date;
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
  null_commodity = create("");
  null_commodity->add_flags(COMMODITY_STYLE_NOMARKET |
			    COMMODITY_STYLE_BUILTIN);
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

  // Start out the new commodity with the default commodity's flags
  // and precision, if one has been defined.
#if 0
  // jww (2007-05-02): This doesn't do anything currently!
  if (default_commodity)
    commodity->drop_flags(COMMODITY_STYLE_THOUSANDS |
			  COMMODITY_STYLE_NOMARKET);
#endif

  commodity->ident = commodities.size();

  std::pair<commodities_t::iterator, bool> result =
    commodities.insert(commodity.get());
  if (! result.second) {
    assert(false);
    return NULL;
  } else {
    return commodity.release();
  }
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

  typedef commodity_pool_t::commodities_t::nth_index<1>::type
    commodities_by_name;

  commodities_by_name& name_index = commodities.get<1>();
  commodities_by_name::const_iterator i = name_index.find(symbol);
  if (i != name_index.end())
    return *i;
  else
    return NULL;
}

commodity_t * commodity_pool_t::find(const commodity_t::ident_t ident)
{
  DEBUG("amounts.commodities", "Find commodity by ident " << ident);

  typedef commodity_pool_t::commodities_t::nth_index<0>::type
    commodities_by_ident;

  commodities_by_ident& ident_index = commodities.get<0>();
  commodities_by_ident::iterator i = ident_index.find(ident);
  if (i != ident_index.end())
    return *i;
  else
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
      assert(ann_comm->annotated &&
	     ann_comm->as_annotated().details);
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
  commodity->ident	  = commodities.size();
  commodity->mapping_key_ = mapping_key;

  std::pair<commodities_t::iterator, bool> result
    = commodities.insert(commodity.get());
  if (! result.second) {
    assert(false);
    return NULL;
  } else {
    return commodity.release();
  }
}

commodity_t * commodity_pool_t::find_or_create(commodity_t&	   comm,
					       const annotation_t& details)
{
  assert(comm);
  assert(details);

  string name = make_qualified_name(comm, details);
  assert(! name.empty());

  if (commodity_t * ann_comm = find(name)) {
    assert(ann_comm->annotated && ann_comm->as_annotated().details);
    return ann_comm;
  }
  return create(comm, details, name);
}

} // namespace ledger
