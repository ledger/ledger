/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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
#include "pyutils.h"
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
  commodity_t * py_create_2(commodity_pool_t&   pool,
                            const string&       symbol,
                            const annotation_t& details)
  {
    return pool.create(symbol, details);
  }

  commodity_t * py_find_or_create_1(commodity_pool_t& pool,
                                    const string&     symbol)
  {
    return pool.find_or_create(symbol);
  }
  commodity_t * py_find_or_create_2(commodity_pool_t&    pool,
                                     const string&       symbol,
                                     const annotation_t& details)
  {
    return pool.find_or_create(symbol, details);
  }

  commodity_t * py_find_1(commodity_pool_t& pool,
                          const string&     name)
  {
    return pool.find(name);
  }

  commodity_t * py_find_2(commodity_pool_t&   pool,
                          const string&       symbol,
                          const annotation_t& details)
  {
    return pool.find(symbol, details);
  }

  // Exchange one commodity for another, while recording the factored price.

  void py_exchange_2(commodity_pool_t& pool,
                     commodity_t&      commodity,
                     const amount_t&   per_unit_cost)
  {
    pool.exchange(commodity, per_unit_cost, CURRENT_TIME());
  }
  void py_exchange_3(commodity_pool_t& pool,
                     commodity_t&      commodity,
                     const amount_t&   per_unit_cost,
                     const datetime_t& moment)
  {
    pool.exchange(commodity, per_unit_cost, moment);
  }

  cost_breakdown_t py_exchange_7(commodity_pool_t&                  pool,
                                 const amount_t&                    amount,
                                 const amount_t&                    cost,
                                 const bool                         is_per_unit,
                                 const bool                         add_prices,
                                 const boost::optional<datetime_t>& moment,
                                 const boost::optional<string>&     tag)
  {
    return pool.exchange(amount, cost, is_per_unit, add_prices, moment, tag);
  }

  commodity_t * py_pool_getitem(commodity_pool_t& pool, const string& symbol)
  {
    commodity_pool_t::commodities_map::iterator i =
      pool.commodities.find(symbol);
    if (i == pool.commodities.end()) {
      PyErr_SetString(PyExc_ValueError,
                      (string("Could not find commodity ") + symbol).c_str());
      throw_error_already_set();
    }
    return (*i).second.get();
  }

  python::list py_pool_keys(commodity_pool_t& pool) {
    python::list keys;
    BOOST_REVERSE_FOREACH
      (const commodity_pool_t::commodities_map::value_type& pair,
       pool.commodities) {
      keys.insert(0, pair.first);
    }
    return keys;
  }

  bool py_pool_contains(commodity_pool_t& pool, const string& symbol) {
    return pool.commodities.find(symbol) != pool.commodities.end();
  }

  commodity_pool_t::commodities_map::iterator
  py_pool_commodities_begin(commodity_pool_t& pool) {
    return pool.commodities.begin();
  }
  commodity_pool_t::commodities_map::iterator
  py_pool_commodities_end(commodity_pool_t& pool) {
    return pool.commodities.end();
  }

  typedef transform_iterator
      <function<string(commodity_pool_t::commodities_map::value_type&)>,
       commodity_pool_t::commodities_map::iterator>
    commodities_map_firsts_iterator;
  commodities_map_firsts_iterator

  py_pool_commodities_keys_begin(commodity_pool_t& pool) {
    return make_transform_iterator
      (pool.commodities.begin(),
       boost::bind(&commodity_pool_t::commodities_map::value_type::first, _1));
  }
  commodities_map_firsts_iterator
  py_pool_commodities_keys_end(commodity_pool_t& pool) {
    return make_transform_iterator
      (pool.commodities.end(),
       boost::bind(&commodity_pool_t::commodities_map::value_type::first, _1));
  }

  typedef transform_iterator
      <function<commodity_t *(commodity_pool_t::commodities_map::value_type&)>,
       commodity_pool_t::commodities_map::iterator>
    commodities_map_seconds_iterator;

  commodities_map_seconds_iterator
  py_pool_commodities_values_begin(commodity_pool_t& pool) {
    return make_transform_iterator
      (pool.commodities.begin(),
       boost::bind(&shared_ptr<commodity_t>::get,
            boost::bind(&commodity_pool_t::commodities_map::value_type::second, _1)));
  }
  commodities_map_seconds_iterator
  py_pool_commodities_values_end(commodity_pool_t& pool) {
    return make_transform_iterator
      (pool.commodities.end(),
       boost::bind(&shared_ptr<commodity_t>::get,
            boost::bind(&commodity_pool_t::commodities_map::value_type::second, _1)));
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

  commodity_t& py_commodity_referent(commodity_t& comm) {
    return comm.referent();
  }
  commodity_t& py_annotated_commodity_referent(annotated_commodity_t& comm) {
    return comm.referent();
  }

  commodity_t& py_strip_annotations_0(commodity_t& comm) {
    return comm.strip_annotations(keep_details_t());
  }
  commodity_t& py_strip_annotations_1(commodity_t& comm,
                                      const keep_details_t& keep) {
    return comm.strip_annotations(keep);
  }

  commodity_t& py_strip_ann_annotations_0(annotated_commodity_t& comm) {
    return comm.strip_annotations(keep_details_t());
  }
  commodity_t& py_strip_ann_annotations_1(annotated_commodity_t& comm,
                                          const keep_details_t& keep) {
    return comm.strip_annotations(keep);
  }

  boost::optional<amount_t> py_price(annotation_t& ann) {
    return ann.price;
  }
  boost::optional<amount_t> py_set_price(annotation_t& ann,
                                         const boost::optional<amount_t>& price) {
    return ann.price = price;
  }

  PyObject * py_commodity_unicode(commodity_t& commodity) {
    return str_to_py_unicode(commodity.symbol());
  }

} // unnamed namespace

void export_commodity()
{
  class_< commodity_pool_t, shared_ptr<commodity_pool_t>,
          boost::noncopyable > ("CommodityPool", no_init)
    .add_property("null_commodity",
                  make_getter(&commodity_pool_t::null_commodity,
                              return_internal_reference<>()))
    .add_property("default_commodity",
                  make_getter(&commodity_pool_t::default_commodity,
                              return_internal_reference<>()),
                  make_setter(&commodity_pool_t::default_commodity,
                              with_custodian_and_ward<1, 2>()))

    .add_property("keep_base",
                  make_getter(&commodity_pool_t::keep_base),
                  make_setter(&commodity_pool_t::keep_base))
    .add_property("price_db",
                  make_getter(&commodity_pool_t::price_db,
                              return_value_policy<return_by_value>()),
                  make_setter(&commodity_pool_t::price_db,
                              return_value_policy<return_by_value>()))
    .add_property("quote_leeway",
                  make_getter(&commodity_pool_t::quote_leeway),
                  make_setter(&commodity_pool_t::quote_leeway))
    .add_property("get_quotes",
                  make_getter(&commodity_pool_t::get_quotes),
                  make_setter(&commodity_pool_t::get_quotes))
    .add_property("get_commodity_quote",
                  make_getter(&commodity_pool_t::get_commodity_quote),
                  make_setter(&commodity_pool_t::get_commodity_quote))

    .def("create", py_create_1, return_internal_reference<>())
    .def("create", py_create_2, return_internal_reference<>())

    .def("find_or_create", py_find_or_create_1, return_internal_reference<>())
    .def("find_or_create", py_find_or_create_2, return_internal_reference<>())

    .def("find", py_find_1, return_internal_reference<>())
    .def("find", py_find_2, return_internal_reference<>())

    .def("exchange", py_exchange_2, with_custodian_and_ward<1, 2>())
    .def("exchange", py_exchange_3, with_custodian_and_ward<1, 2>())
    .def("exchange", py_exchange_7)

    .def("parse_price_directive", &commodity_pool_t::parse_price_directive)
    .def("parse_price_expression", &commodity_pool_t::parse_price_expression,
         return_internal_reference<>())

    .def("__getitem__", py_pool_getitem,
         return_internal_reference<>())
    .def("keys", py_pool_keys)
    .def("has_key", py_pool_contains)
    .def("__contains__", py_pool_contains)
    .def("__iter__",
         python::range<return_internal_reference<> >
         (py_pool_commodities_begin, py_pool_commodities_end))
    .def("iteritems",
         python::range<return_internal_reference<> >
         (py_pool_commodities_begin, py_pool_commodities_end))
    .def("iterkeys", python::range<>(py_pool_commodities_keys_begin,
                                     py_pool_commodities_keys_end))
    .def("itervalues",
         python::range<return_internal_reference<> >
         (py_pool_commodities_values_begin, py_pool_commodities_values_end))
    ;

  map_value_type_converter<commodity_pool_t::commodities_map>();

  scope().attr("commodities") = commodity_pool_t::current_pool;

  scope().attr("COMMODITY_STYLE_DEFAULTS")      = COMMODITY_STYLE_DEFAULTS;
  scope().attr("COMMODITY_STYLE_SUFFIXED")      = COMMODITY_STYLE_SUFFIXED;
  scope().attr("COMMODITY_STYLE_SEPARATED")     = COMMODITY_STYLE_SEPARATED;
  scope().attr("COMMODITY_STYLE_DECIMAL_COMMA") = COMMODITY_STYLE_DECIMAL_COMMA;
  scope().attr("COMMODITY_STYLE_TIME_COLON")    = COMMODITY_STYLE_TIME_COLON;
  scope().attr("COMMODITY_STYLE_THOUSANDS")     = COMMODITY_STYLE_THOUSANDS;
  scope().attr("COMMODITY_NOMARKET")            = COMMODITY_NOMARKET;
  scope().attr("COMMODITY_BUILTIN")             = COMMODITY_BUILTIN;
  scope().attr("COMMODITY_WALKED")              = COMMODITY_WALKED;
  scope().attr("COMMODITY_KNOWN")               = COMMODITY_KNOWN;
  scope().attr("COMMODITY_PRIMARY")             = COMMODITY_PRIMARY;

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

    .add_static_property("decimal_comma_by_default",
                         make_getter(&commodity_t::decimal_comma_by_default),
                         make_setter(&commodity_t::decimal_comma_by_default))

    .def("__str__", &commodity_t::symbol)
    .def("__unicode__", py_commodity_unicode)
    .def("__nonzero__", &commodity_t::operator bool)

    .def(self == self)

    .def("symbol_needs_quotes", &commodity_t::symbol_needs_quotes)
    .staticmethod("symbol_needs_quotes")

    .add_property("referent",
                  make_function(py_commodity_referent,
                                return_internal_reference<>()))

    .def("has_annotation", &commodity_t::has_annotation)
    .def("strip_annotations", py_strip_annotations_0,
         return_internal_reference<>())
    .def("strip_annotations", py_strip_annotations_1,
         return_internal_reference<>())
    .def("write_annotations", &commodity_t::write_annotations)

    .def("pool", &commodity_t::pool,
         return_internal_reference<>())

    .add_property("base_symbol", &commodity_t::base_symbol)
    .add_property("symbol", &commodity_t::symbol)

    .add_property("name", &commodity_t::name, &commodity_t::set_name)
    .add_property("note", &commodity_t::note, &commodity_t::set_note)
    .add_property("precision", &commodity_t::precision,
                  &commodity_t::set_precision)
    .add_property("smaller", &commodity_t::smaller, &commodity_t::set_smaller)
    .add_property("larger", &commodity_t::larger, &commodity_t::set_larger)

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

    .add_property("price", py_price, py_set_price)
    .add_property("date",
                  make_getter(&annotation_t::date,
                              return_value_policy<return_by_value>()),
                  make_setter(&annotation_t::date,
                              return_value_policy<return_by_value>()))
    .add_property("tag",
                  make_getter(&annotation_t::tag,
                              return_value_policy<return_by_value>()),
                  make_setter(&annotation_t::tag,
                              return_value_policy<return_by_value>()))

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

    .add_property("referent",
                  make_function(py_annotated_commodity_referent,
                                return_internal_reference<>()))

    .def("strip_annotations", py_strip_ann_annotations_0,
         return_internal_reference<>())
    .def("strip_annotations", py_strip_ann_annotations_1,
         return_internal_reference<>())
    .def("write_annotations", &annotated_commodity_t::write_annotations)
    ;
}

} // namespace ledger
