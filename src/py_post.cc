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
#include "post.h"
#include "xact.h"

namespace ledger {

using namespace boost::python;

namespace {

  bool py_has_tag_1s(post_t& post, const string& tag) {
    return post.has_tag(tag);
  }
  bool py_has_tag_1m(post_t& post, const mask_t& tag_mask) {
    return post.has_tag(tag_mask);
  }
  bool py_has_tag_2m(post_t& post, const mask_t& tag_mask,
                     const boost::optional<mask_t>& value_mask) {
    return post.has_tag(tag_mask, value_mask);
  }

  boost::optional<value_t> py_get_tag_1s(post_t& post, const string& tag) {
    return post.get_tag(tag);
  }
  boost::optional<value_t> py_get_tag_1m(post_t& post, const mask_t& tag_mask) {
    return post.get_tag(tag_mask);
  }
  boost::optional<value_t> py_get_tag_2m(post_t& post, const mask_t& tag_mask,
                                         const boost::optional<mask_t>& value_mask) {
    return post.get_tag(tag_mask, value_mask);
  }

  post_t::xdata_t& py_xdata(post_t& post) {
    return post.xdata();
  }

  account_t * py_reported_account(post_t& post) {
    return post.reported_account();
  }

} // unnamed namespace

void export_post()
{
  scope().attr("POST_EXT_RECEIVED")    = POST_EXT_RECEIVED;
  scope().attr("POST_EXT_HANDLED")     = POST_EXT_HANDLED;
  scope().attr("POST_EXT_DISPLAYED")   = POST_EXT_DISPLAYED;
  scope().attr("POST_EXT_DIRECT_AMT")  = POST_EXT_DIRECT_AMT;
  scope().attr("POST_EXT_SORT_CALC")   = POST_EXT_SORT_CALC;
  scope().attr("POST_EXT_COMPOUND")    = POST_EXT_COMPOUND;
  scope().attr("POST_EXT_VISITED")     = POST_EXT_VISITED;
  scope().attr("POST_EXT_MATCHES")     = POST_EXT_MATCHES;
  scope().attr("POST_EXT_CONSIDERED")  = POST_EXT_CONSIDERED;

  class_< post_t::xdata_t > ("PostingXData")
#if 1
    .add_property("flags",
                  &supports_flags<uint_least16_t>::flags,
                  &supports_flags<uint_least16_t>::set_flags)
    .def("has_flags", &supports_flags<uint_least16_t>::has_flags)
    .def("clear_flags", &supports_flags<uint_least16_t>::clear_flags)
    .def("add_flags", &supports_flags<uint_least16_t>::add_flags)
    .def("drop_flags", &supports_flags<uint_least16_t>::drop_flags)
#endif

    .add_property("visited_value",
                  make_getter(&post_t::xdata_t::visited_value),
                  make_setter(&post_t::xdata_t::visited_value))
    .add_property("compound_value",
                  make_getter(&post_t::xdata_t::compound_value),
                  make_setter(&post_t::xdata_t::compound_value))
    .add_property("total",
                  make_getter(&post_t::xdata_t::total),
                  make_setter(&post_t::xdata_t::total))
    .add_property("count",
                  make_getter(&post_t::xdata_t::count),
                  make_setter(&post_t::xdata_t::count))
    .add_property("date",
                  make_getter(&post_t::xdata_t::date),
                  make_setter(&post_t::xdata_t::date))
    .add_property("datetime",
                  make_getter(&post_t::xdata_t::datetime),
                  make_setter(&post_t::xdata_t::datetime))
    .add_property("account",
                  make_getter(&post_t::xdata_t::account,
                              return_internal_reference<>()),
                  make_setter(&post_t::xdata_t::account,
                              with_custodian_and_ward<1, 2>()))
    .add_property("sort_values",
                  make_getter(&post_t::xdata_t::sort_values),
                  make_setter(&post_t::xdata_t::sort_values))
    ;

  scope().attr("POST_VIRTUAL")         = POST_VIRTUAL;
  scope().attr("POST_MUST_BALANCE")    = POST_MUST_BALANCE;
  scope().attr("POST_CALCULATED")      = POST_CALCULATED;
  scope().attr("POST_COST_CALCULATED") = POST_COST_CALCULATED;

  class_< post_t, bases<item_t> > ("Posting")
    //.def(init<account_t *>())

    .def("id", &post_t::id)
    .def("seq", &post_t::seq)

    .add_property("xact",
                  make_getter(&post_t::xact,
                              return_internal_reference<>()),
                  make_setter(&post_t::xact,
                              with_custodian_and_ward<1, 2>()))
    .add_property("account",
                  make_getter(&post_t::account,
                              return_internal_reference<>()),
                  make_setter(&post_t::account,
                              with_custodian_and_ward<1, 2>()))
    .add_property("amount",
                  make_getter(&post_t::amount),
                  make_setter(&post_t::amount))
    .add_property("cost",
                  make_getter(&post_t::cost,
                              return_value_policy<return_by_value>()),
                  make_setter(&post_t::cost,
                              return_value_policy<return_by_value>()))
    .add_property("assigned_amount",
                  make_getter(&post_t::assigned_amount,
                              return_value_policy<return_by_value>()),
                  make_setter(&post_t::assigned_amount,
                              return_value_policy<return_by_value>()))

    .def("has_tag", py_has_tag_1s)
    .def("has_tag", py_has_tag_1m)
    .def("has_tag", py_has_tag_2m)
    .def("get_tag", py_get_tag_1s)
    .def("get_tag", py_get_tag_1m)
    .def("get_tag", py_get_tag_2m)

    .add_property("date", &post_t::date)
    .add_property("aux_date", &post_t::aux_date)

    .def("must_balance", &post_t::must_balance)

    .def("lookup", &post_t::lookup)

    .def("valid", &post_t::valid)

    .def("has_xdata", &post_t::has_xdata)
    .def("clear_xdata", &post_t::clear_xdata)
    .def("xdata", py_xdata, return_internal_reference<>())

    //.def("add_to_value", &post_t::add_to_value)
    .def("set_reported_account", &post_t::set_reported_account)

    .def("reported_account", py_reported_account,
         return_internal_reference<>())
    ;
}

} // namespace ledger
