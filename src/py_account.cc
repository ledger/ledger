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
#include "account.h"
#include "post.h"
#include "expr.h"

namespace ledger {

using namespace boost::python;

namespace {

  long accounts_len(account_t& account)
  {
    return static_cast<long>(account.accounts.size());
  }

  account_t& accounts_getitem(account_t& account, long i)
  {
    static long last_index = 0;
    static account_t * last_account = NULL;
    static accounts_map::iterator elem;

    long len = static_cast<long>(account.accounts.size());

    if (labs(i) >= len) {
      PyErr_SetString(PyExc_IndexError, _("Index out of range"));
      throw_error_already_set();
    }

    if (&account == last_account && i == last_index + 1) {
      last_index = i;
      return *(*++elem).second;
    }

    long x = i < 0 ? len + i : i;
    elem = account.accounts.begin();
    while (--x >= 0)
      elem++;

    last_account = &account;
    last_index   = i;

    return *(*elem).second;
  }

#if 0
  account_t * py_find_account_1(journal_t& journal, const string& name)
  {
    return journal.find_account(name);
  }

  account_t * py_find_account_2(journal_t& journal, const string& name,
                                const bool auto_create)
  {
    return journal.find_account(name, auto_create);
  }
#endif

  account_t::xdata_t& py_xdata(account_t& account) {
    return account.xdata();
  }

  PyObject * py_account_unicode(account_t& account) {
    return str_to_py_unicode(account.fullname());
  }

  value_t py_amount_0(const account_t& account)
  {
      return account.amount();
  }

  value_t py_amount_1(const account_t& account, const boost::optional<expr_t&>& expr)
  {
      return account.amount(expr);
  }

  value_t py_total_0(const account_t& account)
  {
      return account.total();
  }

  value_t py_total_1(const account_t& account, const boost::optional<expr_t&>& expr)
  {
      return account.total(expr);
  }

} // unnamed namespace

void export_account()
{
  scope().attr("ACCOUNT_EXT_SORT_CALC")        = ACCOUNT_EXT_SORT_CALC;
  scope().attr("ACCOUNT_EXT_HAS_NON_VIRTUALS") = ACCOUNT_EXT_HAS_NON_VIRTUALS;
  scope().attr("ACCOUNT_EXT_HAS_UNB_VIRTUALS") = ACCOUNT_EXT_HAS_UNB_VIRTUALS;
  scope().attr("ACCOUNT_EXT_AUTO_VIRTUALIZE")  = ACCOUNT_EXT_AUTO_VIRTUALIZE;
  scope().attr("ACCOUNT_EXT_VISITED")          = ACCOUNT_EXT_VISITED;
  scope().attr("ACCOUNT_EXT_MATCHING")         = ACCOUNT_EXT_MATCHING;
  scope().attr("ACCOUNT_EXT_TO_DISPLAY")       = ACCOUNT_EXT_TO_DISPLAY;
  scope().attr("ACCOUNT_EXT_DISPLAYED")        = ACCOUNT_EXT_DISPLAYED;

  class_< account_t::xdata_t::details_t > ("AccountXDataDetails")
    .def_readonly("total", &account_t::xdata_t::details_t::total)
    .def_readonly("calculated", &account_t::xdata_t::details_t::calculated)
    .def_readonly("gathered", &account_t::xdata_t::details_t::gathered)

    .def_readonly("posts_count", &account_t::xdata_t::details_t::posts_count)
    .def_readonly("posts_virtuals_count",
                  &account_t::xdata_t::details_t::posts_virtuals_count)
    .def_readonly("posts_cleared_count",
                  &account_t::xdata_t::details_t::posts_cleared_count)
    .def_readonly("posts_last_7_count",
                  &account_t::xdata_t::details_t::posts_last_7_count)
    .def_readonly("posts_last_30_count",
                  &account_t::xdata_t::details_t::posts_last_30_count)
    .def_readonly("posts_this_month_count",
                  &account_t::xdata_t::details_t::posts_this_month_count)

    .def_readonly("earliest_post",
                  &account_t::xdata_t::details_t::earliest_post)
    .def_readonly("earliest_cleared_post",
                  &account_t::xdata_t::details_t::earliest_cleared_post)
    .def_readonly("latest_post",
                  &account_t::xdata_t::details_t::latest_post)
    .def_readonly("latest_cleared_post",
                  &account_t::xdata_t::details_t::latest_cleared_post)

    .def_readonly("filenames", &account_t::xdata_t::details_t::filenames)
    .def_readonly("accounts_referenced",
                  &account_t::xdata_t::details_t::accounts_referenced)
    .def_readonly("payees_referenced",
                  &account_t::xdata_t::details_t::payees_referenced)

    .def(self += self)

    .def("update", &account_t::xdata_t::details_t::update)
    ;

  class_< account_t::xdata_t > ("AccountXData")
#if 1
    .add_property("flags",
                  &supports_flags<uint_least16_t>::flags,
                  &supports_flags<uint_least16_t>::set_flags)
    .def("has_flags", &supports_flags<uint_least16_t>::has_flags)
    .def("clear_flags", &supports_flags<uint_least16_t>::clear_flags)
    .def("add_flags", &supports_flags<uint_least16_t>::add_flags)
    .def("drop_flags", &supports_flags<uint_least16_t>::drop_flags)
#endif

    .def_readonly("self_details", &account_t::xdata_t::self_details)
    .def_readonly("family_details", &account_t::xdata_t::family_details)
    .def_readonly("reported_posts", &account_t::xdata_t::reported_posts)
    .def_readonly("sort_values", &account_t::xdata_t::sort_values)
    ;

  scope().attr("ACCOUNT_NORMAL") = ACCOUNT_NORMAL;
  scope().attr("ACCOUNT_KNOWN")  = ACCOUNT_KNOWN;
  scope().attr("ACCOUNT_TEMP")   = ACCOUNT_TEMP;

  class_< account_t > ("Account")
#if 1
    .add_property("flags",
                  &supports_flags<>::flags,
                  &supports_flags<>::set_flags)
    .def("has_flags", &supports_flags<>::has_flags)
    .def("clear_flags", &supports_flags<>::clear_flags)
    .def("add_flags", &supports_flags<>::add_flags)
    .def("drop_flags", &supports_flags<>::drop_flags)
#endif

    .add_property("parent",
                  make_getter(&account_t::parent,
                              return_internal_reference<>()))

    .def_readwrite("name", &account_t::name)
    .def_readwrite("note", &account_t::note)
    .def_readonly("depth", &account_t::depth)

    .def("__str__", &account_t::fullname)
    .def("__unicode__", py_account_unicode)

    .def("fullname", &account_t::fullname)
    .def("partial_name", &account_t::partial_name)

    .def("add_account", &account_t::add_account)
    .def("remove_account", &account_t::remove_account)

    .def("find_account", &account_t::find_account,
         return_internal_reference<>())
    .def("find_account_re", &account_t::find_account,
         return_internal_reference<>())

    .def("add_post", &account_t::add_post)
    .def("remove_post", &account_t::remove_post)

    .def("valid", &account_t::valid)

    .def("__len__", accounts_len)
    .def("__getitem__", accounts_getitem, return_internal_reference<>())

    .def("__iter__", python::range<return_internal_reference<> >
         (&account_t::accounts_begin, &account_t::accounts_end))
    .def("accounts", python::range<return_internal_reference<> >
         (&account_t::accounts_begin, &account_t::accounts_end))
    .def("posts", python::range<return_internal_reference<> >
         (&account_t::posts_begin, &account_t::posts_end))

    .def("has_xdata", &account_t::has_xdata)
    .def("clear_xdata", &account_t::clear_xdata)
    .def("xdata", py_xdata,
         return_internal_reference<>())

    .def("amount", py_amount_0)
    .def("amount", py_amount_1, args("expr"))
    .def("total", py_total_0)
    .def("total", py_total_1, args("expr"))

    .def("self_details", &account_t::self_details,
         return_internal_reference<>())
    .def("family_details", &account_t::family_details,
         return_internal_reference<>())

    .def("has_xflags", &account_t::has_xflags)
    .def("children_with_flags", &account_t::children_with_flags)
    ;
}

} // namespace ledger
