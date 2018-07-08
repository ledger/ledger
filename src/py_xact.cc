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
#include "xact.h"
#include "post.h"

namespace ledger {

using namespace boost::python;

namespace {

  long posts_len(xact_base_t& xact)
  {
    return static_cast<long>(xact.posts.size());
  }

  post_t& posts_getitem(xact_base_t& xact, long i)
  {
    static long last_index = 0;
    static xact_base_t * last_xact = NULL;
    static posts_list::iterator elem;

    long len = static_cast<long>(xact.posts.size());

    if (labs(i) >= len) {
      PyErr_SetString(PyExc_IndexError, _("Index out of range"));
      throw_error_already_set();
    }

    if (&xact == last_xact && i == last_index + 1) {
      last_index = i;
      return **++elem;
    }

    long x = i < 0 ? len + i : i;
    elem = xact.posts.begin();
    while (--x >= 0)
      elem++;

    last_xact = &xact;
    last_index = i;

    return **elem;
  }

  string py_xact_to_string(xact_t&)
  {
    // jww (2012-03-01): TODO
    return empty_string;
  }

} // unnamed namespace

using namespace boost::python;

void export_xact()
{
  class_< xact_base_t, bases<item_t>, noncopyable > ("TransactionBase", no_init)
    .add_property("journal",
                  make_getter(&xact_base_t::journal,
                              return_internal_reference<>()),
                  make_setter(&xact_base_t::journal,
                              with_custodian_and_ward<1, 2>()))

    .def("__len__", posts_len)
    .def("__getitem__", posts_getitem,
         return_internal_reference<>())

    .def("add_post", &xact_base_t::add_post, with_custodian_and_ward<1, 2>())
    .def("remove_post", &xact_base_t::add_post)

    .def("finalize", &xact_base_t::finalize)

    .def("__iter__", python::range<return_internal_reference<> >
         (&xact_t::posts_begin, &xact_t::posts_end))
    .def("posts", python::range<return_internal_reference<> >
         (&xact_t::posts_begin, &xact_t::posts_end))

    .def("valid", &xact_base_t::valid)
    ;

  class_< xact_t, bases<xact_base_t> > ("Transaction")
    .def("id", &xact_t::id)
    .def("seq", &xact_t::seq)

    .def("__str__", py_xact_to_string)

    .add_property("code",
                  make_getter(&xact_t::code, return_value_policy<return_by_value>()),
                  make_setter(&xact_t::code, return_value_policy<return_by_value>()))
    .add_property("payee",
                  make_getter(&xact_t::payee),
                  make_setter(&xact_t::payee))

    .def("add_post", &xact_t::add_post, with_custodian_and_ward<1, 2>())

    .def("magnitude", &xact_t::magnitude)

    .def("lookup", &xact_t::lookup)

    .def("has_xdata", &xact_t::has_xdata)
    .def("clear_xdata", &xact_t::clear_xdata)

    .def("valid", &xact_t::valid)
    ;

  class_< auto_xact_t, bases<xact_base_t> > ("AutomatedTransaction")
    .def(init<predicate_t>())

    .add_property("predicate",
                  make_getter(&auto_xact_t::predicate),
                  make_setter(&auto_xact_t::predicate))

    .def("extend_xact", &auto_xact_t::extend_xact)
    ;

  class_< period_xact_t, bases<xact_base_t> > ("PeriodicTransaction")
    .def(init<string>())

    .add_property("period",
                  make_getter(&period_xact_t::period),
                  make_setter(&period_xact_t::period))
    .add_property("period_string",
                  make_getter(&period_xact_t::period_string),
                  make_setter(&period_xact_t::period_string))
    ;

  register_optional_to_python<std::string>();
}

} // namespace ledger
