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

#include <system.hh>

#include "pyinterp.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"

namespace ledger {

using namespace boost::python;

namespace {

  commodity_t * py_create_1(commodity_pool_t& pool,
			    const string&     symbol)
  {
    return pool.create(symbol);
  }
  commodity_t * py_create_2(commodity_pool_t&	pool,
			    const string&	symbol,
			    const annotation_t& details)
  {
    return pool.create(symbol, details);
  }

  commodity_t * py_find_or_create_1(commodity_pool_t& pool,
				    const string&     symbol)
  {
    return pool.find_or_create(symbol);
  }
  commodity_t * py_find_or_create_2(commodity_pool_t&	 pool,
				     const string&	 symbol,
				     const annotation_t& details)
  {
    return pool.find_or_create(symbol, details);
  }

  commodity_t * py_find_1(commodity_pool_t& pool,
			  const string&	    name)
  {
    return pool.find(name);
  }

  commodity_t * py_find_2(commodity_pool_t&   pool,
			  const string&	      symbol,
			  const annotation_t& details)
  {
    return pool.find(symbol, details);
  }

  // Exchange one commodity for another, while recording the factored price.

  void py_exchange_3(commodity_pool_t& pool,
		     commodity_t&      commodity,
		     const amount_t&   per_unit_cost,
		     const datetime_t& moment)
  {
    pool.exchange(commodity, per_unit_cost, moment);
  }

  cost_breakdown_t py_exchange_5(commodity_pool_t&		    pool,
				 const amount_t&		    amount,
				 const amount_t&		    cost,
				 const bool			    is_per_unit,
				 const boost::optional<datetime_t>& moment,
				 const boost::optional<string>&     tag)
  {
    return pool.exchange(amount, cost, is_per_unit, moment, tag);
  }

  void py_add_price_2(commodity_t& commodity,
		      const datetime_t& date, const amount_t& price) {
    commodity.add_price(date, price);
  }
    
  void py_add_price_3(commodity_t& commodity, const datetime_t& date,
		      const amount_t& price, const bool reflexive) {
    commodity.add_price(date, price, reflexive);
  }

  bool py_keep_all_0(keep_details_t& details) {
    return details.keep_all();
  }
  bool py_keep_all_1(keep_details_t& details, const commodity_t& comm) {
    return details.keep_all(comm);
  }

  bool py_keep_any_0(keep_details_t& details) {
    return details.keep_any();
  }
  bool py_keep_any_1(keep_details_t& details, const commodity_t& comm) {
    return details.keep_any(comm);
  }

} // unnamed namespace

void export_commodity()
{
  class_< commodity_pool_t, boost::noncopyable > ("CommodityPool", no_init)
    .add_property("null_commodity",
		  make_getter(&commodity_pool_t::null_commodity,
			      return_internal_reference<>()),
		  make_setter(&commodity_pool_t::null_commodity,
			      with_custodian_and_ward<1, 2>()))
    .add_property("default_commodity",
		  make_getter(&commodity_pool_t::default_commodity,
			      return_internal_reference<>()),
		  make_setter(&commodity_pool_t::default_commodity,
			      with_custodian_and_ward<1, 2>()))

    .add_property("keep_base",
		  make_getter(&commodity_pool_t::keep_base),
		  make_setter(&commodity_pool_t::keep_base))
    .add_property("price_db",
		  make_getter(&commodity_pool_t::price_db),
		  make_setter(&commodity_pool_t::price_db))
    .add_property("quote_leeway",
		  make_getter(&commodity_pool_t::quote_leeway),
		  make_setter(&commodity_pool_t::quote_leeway))
    .add_property("get_quotes",
		  make_getter(&commodity_pool_t::get_quotes),
		  make_setter(&commodity_pool_t::get_quotes))
    .add_property("get_commodity_quote",
		  make_getter(&commodity_pool_t::get_commodity_quote),
		  make_setter(&commodity_pool_t::get_commodity_quote))

    .def("make_qualified_name", &commodity_pool_t::make_qualified_name)

    .def("create", py_create_1, return_internal_reference<>())
    .def("create", py_create_2, return_internal_reference<>())

    .def("find_or_create", py_find_or_create_1,
	 return_internal_reference<>())
    .def("find_or_create", py_find_or_create_2,
	 return_internal_reference<>())

    .def("find", py_find_1, return_internal_reference<>())
    .def("find", py_find_2, return_internal_reference<>())

    .def("exchange", py_exchange_3, with_custodian_and_ward<1, 2>())
    .def("exchange", py_exchange_5)

    .def("parse_price_directive", &commodity_pool_t::parse_price_directive)
    .def("parse_price_expression", &commodity_pool_t::parse_price_expression,
	 return_internal_reference<>())
    ;

  scope().attr("COMMODITY_STYLE_DEFAULTS")  = COMMODITY_STYLE_DEFAULTS;
  scope().attr("COMMODITY_STYLE_SUFFIXED")  = COMMODITY_STYLE_SUFFIXED;
  scope().attr("COMMODITY_STYLE_SEPARATED") = COMMODITY_STYLE_SEPARATED;
  scope().attr("COMMODITY_STYLE_EUROPEAN")  = COMMODITY_STYLE_EUROPEAN;
  scope().attr("COMMODITY_STYLE_THOUSANDS") = COMMODITY_STYLE_THOUSANDS;
  scope().attr("COMMODITY_NOMARKET")        = COMMODITY_NOMARKET;
  scope().attr("COMMODITY_BUILTIN")         = COMMODITY_BUILTIN;
  scope().attr("COMMODITY_WALKED")          = COMMODITY_WALKED;
  scope().attr("COMMODITY_KNOWN")           = COMMODITY_KNOWN;
  scope().attr("COMMODITY_PRIMARY")         = COMMODITY_PRIMARY;

  class_< commodity_t, boost::noncopyable > ("Commodity", no_init)
#if 1
    .add_property("flags",
		  &supports_flags<uint_least16_t>::flags,
		  &supports_flags<uint_least16_t>::set_flags)
    .def("has_flags", &delegates_flags<uint_least16_t>::has_flags)
    .def("clear_flags", &delegates_flags<uint_least16_t>::clear_flags)
    .def("add_flags", &delegates_flags<uint_least16_t>::add_flags)
    .def("drop_flags", &delegates_flags<uint_least16_t>::drop_flags)
#endif

    .add_static_property("european_by_default",
			 make_getter(&commodity_t::european_by_default),
			 make_setter(&commodity_t::european_by_default))

    .def("__nonzero__", &commodity_t::operator bool)

    .def(self == self)

    .def("symbol_needs_quotes", &commodity_t::symbol_needs_quotes)
    .staticmethod("symbol_needs_quotes")

#if 0
    .def("referent", &commodity_t::referent,
	 return_internal_reference<>())
#endif

    .def("is_annotated", &commodity_t::is_annotated)
    .def("strip_annotations", &commodity_t::strip_annotations,
	 return_internal_reference<>())
    .def("write_annotations", &commodity_t::write_annotations)

    .def("pool", &commodity_t::pool,
	 return_internal_reference<>())

    .def("base_symbol", &commodity_t::base_symbol)
    .def("symbol", &commodity_t::symbol)
    .def("mapping_key", &commodity_t::mapping_key)

    .def("name", &commodity_t::name)
    .def("set_name", &commodity_t::set_name)
    .def("note", &commodity_t::note)
    .def("set_note", &commodity_t::set_note)
    .def("precision", &commodity_t::precision)
    .def("set_precision", &commodity_t::set_precision)
    .def("smaller", &commodity_t::smaller)
    .def("set_smaller", &commodity_t::set_smaller)
    .def("larger", &commodity_t::larger)
    .def("set_larger", &commodity_t::set_larger)

    .def("add_price", py_add_price_2)
    .def("add_price", py_add_price_3)
    .def("remove_price", &commodity_t::remove_price,
	 with_custodian_and_ward<1, 3>())
    .def("find_price", &commodity_t::find_price)
    .def("check_for_updated_price", &commodity_t::check_for_updated_price)

    .def("valid", &commodity_t::valid)
    ;

  class_< annotation_t > ("Annotation", no_init)
#if 1
    .add_property("flags", &supports_flags<>::flags,
		  &supports_flags<>::set_flags)
    .def("has_flags", &supports_flags<>::has_flags)
    .def("clear_flags", &supports_flags<>::clear_flags)
    .def("add_flags", &supports_flags<>::add_flags)
    .def("drop_flags", &supports_flags<>::drop_flags)
#endif

    .add_property("price",
		  make_getter(&annotation_t::price),
		  make_setter(&annotation_t::price))
    .add_property("date",
		  make_getter(&annotation_t::date),
		  make_setter(&annotation_t::date))
    .add_property("tag",
		  make_getter(&annotation_t::tag),
		  make_setter(&annotation_t::tag))

    .def("__nonzero__", &annotation_t::operator bool)

    .def(self == self)

    .def("valid", &annotation_t::valid)
    ;

  class_< keep_details_t > ("KeepDetails")
    .def(init<bool, bool, bool, bool>())

    .add_property("keep_price",
		  make_getter(&keep_details_t::keep_price),
		  make_setter(&keep_details_t::keep_price))
    .add_property("keep_date",
		  make_getter(&keep_details_t::keep_date),
		  make_setter(&keep_details_t::keep_date))
    .add_property("keep_tag",
		  make_getter(&keep_details_t::keep_tag),
		  make_setter(&keep_details_t::keep_tag))
    .add_property("only_actuals",
		  make_getter(&keep_details_t::only_actuals),
		  make_setter(&keep_details_t::only_actuals))

    .def("keep_all", py_keep_all_0)
    .def("keep_all", py_keep_all_1)
    .def("keep_any", py_keep_any_0)
    .def("keep_any", py_keep_any_1)
    ;

  class_< annotated_commodity_t, bases<commodity_t>,
          annotated_commodity_t, boost::noncopyable >
    ("AnnotatedCommodity", no_init)
    .add_property("details",
		  make_getter(&annotated_commodity_t::details),
		  make_setter(&annotated_commodity_t::details))

    .def(self == self)
    .def(self == other<commodity_t>())

#if 0
    .def("referent", &annotated_commodity_t::referent,
	 return_internal_reference<>())
#endif

    .def("strip_annotations", &annotated_commodity_t::strip_annotations,
	 return_internal_reference<>())
    .def("write_annotations", &annotated_commodity_t::write_annotations)
    ;
}

} // namespace ledger
