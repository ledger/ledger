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
#include "scope.h"
#include "mask.h"
#include "item.h"

namespace ledger {

using namespace boost::python;

namespace {

  bool py_has_tag_1s(item_t& item, const string& tag) {
    return item.has_tag(tag);
  }
  bool py_has_tag_1m(item_t& item, const mask_t& tag_mask) {
    return item.has_tag(tag_mask);
  }
  bool py_has_tag_2m(item_t& item, const mask_t& tag_mask,
                     const boost::optional<mask_t>& value_mask) {
    return item.has_tag(tag_mask, value_mask);
  }

  boost::optional<value_t> py_get_tag_1s(item_t& item, const string& tag) {
    return item.get_tag(tag);
  }
  boost::optional<value_t> py_get_tag_1m(item_t& item, const mask_t& tag_mask) {
    return item.get_tag(tag_mask);
  }
  boost::optional<value_t> py_get_tag_2m(item_t& item, const mask_t& tag_mask,
                                         const boost::optional<mask_t>& value_mask) {
    return item.get_tag(tag_mask, value_mask);
  }

  std::string py_position_pathname(position_t const& pos) {
    return pos.pathname.native();
  }
  void py_position_set_pathname(position_t& pos, string const& s) {
    pos.pathname = s;
  }

} // unnamed namespace

#if 0
#define EXC_TRANSLATOR(type)                            \
  void exc_translate_ ## type(const type& err) {        \
    PyErr_SetString(PyExc_ArithmeticError, err.what()); \
  }

//EXC_TRANSLATOR(item_error)
#endif

void export_item()
{
  class_< position_t > ("Position")
    .add_property("pathname",
                  make_function(py_position_pathname),
                  make_function(py_position_set_pathname))
    .add_property("beg_pos",
                  make_getter(&position_t::beg_pos),
                  make_setter(&position_t::beg_pos))
    .add_property("beg_line",
                  make_getter(&position_t::beg_line),
                  make_setter(&position_t::beg_line))
    .add_property("end_pos",
                  make_getter(&position_t::end_pos),
                  make_setter(&position_t::end_pos))
    .add_property("end_line",
                  make_getter(&position_t::end_line),
                  make_setter(&position_t::end_line))
    ;

  scope().attr("ITEM_NORMAL")    = ITEM_NORMAL;
  scope().attr("ITEM_GENERATED") = ITEM_GENERATED;
  scope().attr("ITEM_TEMP")      = ITEM_TEMP;

  enum_< item_t::state_t > ("State")
    .value("Uncleared", item_t::UNCLEARED)
    .value("Cleared",   item_t::CLEARED)
    .value("Pending",   item_t::PENDING)
    ;

#if 0
  class_< item_t, bases<scope_t> > ("JournalItem", init<uint_least8_t>())
#else
  class_< item_t, noncopyable > ("JournalItem", no_init)
#endif
#if 1
    .add_property("flags", &supports_flags<>::flags,
                  &supports_flags<>::set_flags)
    .def("has_flags", &supports_flags<>::has_flags)
    .def("clear_flags", &supports_flags<>::clear_flags)
    .def("add_flags", &supports_flags<>::add_flags)
    .def("drop_flags", &supports_flags<>::drop_flags)
#endif

    .add_property("note",
                  make_getter(&item_t::note,
                              return_value_policy<return_by_value>()),
                  make_setter(&item_t::note,
                              return_value_policy<return_by_value>()))
    .add_property("pos",
                  make_getter(&item_t::pos,
                              return_value_policy<return_by_value>()),
                  make_setter(&item_t::pos,
                              return_value_policy<return_by_value>()))
    .add_property("metadata",
                  make_getter(&item_t::metadata,
                              return_value_policy<return_by_value>()),
                  make_setter(&item_t::metadata,
                              return_value_policy<return_by_value>()))

    .def("copy_details", &item_t::copy_details)

    .def(self == self)
    .def(self != self)

    .def("has_tag", py_has_tag_1s)
    .def("has_tag", py_has_tag_1m)
    .def("has_tag", py_has_tag_2m)
    .def("get_tag", py_get_tag_1s)
    .def("get_tag", py_get_tag_1m)
    .def("get_tag", py_get_tag_2m)
    .def("tag", py_get_tag_1s)
    .def("tag", py_get_tag_1m)
    .def("tag", py_get_tag_2m)

    .def("set_tag", &item_t::set_tag)

    .def("parse_tags", &item_t::parse_tags)
    .def("append_note", &item_t::append_note)

    .add_static_property("use_aux_date",
                         make_getter(&item_t::use_aux_date),
                         make_setter(&item_t::use_aux_date))

    .add_property("date", &item_t::date, make_setter(&item_t::_date))
    .add_property("aux_date", &item_t::aux_date,
                  make_setter(&item_t::_date_aux))

    .add_property("state", &item_t::state, &item_t::set_state)

    .def("lookup", &item_t::lookup)

    .def("valid", &item_t::valid)
    ;

  register_optional_to_python<position_t>();
}

} // namespace ledger
