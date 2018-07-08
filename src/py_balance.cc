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
#include "pyfstream.h"
#include "commodity.h"
#include "annotate.h"
#include "balance.h"

namespace ledger {

using namespace boost::python;

namespace {

  boost::optional<balance_t> py_value_0(const balance_t& balance) {
    return balance.value(CURRENT_TIME());
  }
  boost::optional<balance_t> py_value_1(const balance_t& balance,
                                        const commodity_t * in_terms_of) {
    return balance.value(CURRENT_TIME(), in_terms_of);
  }
  boost::optional<balance_t> py_value_2(const balance_t& balance,
                                        const commodity_t * in_terms_of,
                                        const datetime_t& moment) {
    return balance.value(moment, in_terms_of);
  }
  boost::optional<balance_t> py_value_2d(const balance_t& balance,
                                         const commodity_t * in_terms_of,
                                         const date_t& moment) {
    return balance.value(datetime_t(moment), in_terms_of);
  }

  boost::optional<amount_t>
  py_commodity_amount_0(const balance_t& balance) {
    return balance.commodity_amount();
  }

  boost::optional<amount_t>
  py_commodity_amount_1(const balance_t& balance,
                        const commodity_t& commodity) {
    return balance.commodity_amount(commodity);
  }

#if 0
  void py_print(balance_t& balance, object out) {
    if (PyFile_Check(out.ptr())) {
      pyofstream outstr(reinterpret_cast<PyFileObject *>(out.ptr()));
      balance.print(outstr);
    } else {
      PyErr_SetString(PyExc_IOError,
                      _("Argument to balance.print_(file) is not a file object"));
    }
  }
#endif

  long balance_len(balance_t& bal) {
    return static_cast<long>(bal.amounts.size());
  }

  amount_t balance_getitem(balance_t& bal, long i) {
    long len = static_cast<long>(bal.amounts.size());

    if (labs(i) >= len) {
      PyErr_SetString(PyExc_IndexError, _("Index out of range"));
      throw_error_already_set();
    }

    long x = i < 0 ? len + i : i;
    balance_t::amounts_map::iterator elem = bal.amounts.begin();
    while (--x >= 0)
      elem++;

    return (*elem).second;
  }

  balance_t py_strip_annotations_0(balance_t& balance) {
    return balance.strip_annotations(keep_details_t());
  }
  balance_t py_strip_annotations_1(balance_t& balance, const keep_details_t& keep) {
    return balance.strip_annotations(keep);
  }

  PyObject * py_balance_unicode(balance_t& balance) {
    return str_to_py_unicode(balance.to_string());
  }

} // unnamed namespace

#define EXC_TRANSLATOR(type)                            \
  void exc_translate_ ## type(const type& err) {        \
    PyErr_SetString(PyExc_ArithmeticError, err.what()); \
  }

EXC_TRANSLATOR(balance_error)

void export_balance()
{
  class_< balance_t > ("Balance")
    .def(init<balance_t>())
    .def(init<amount_t>())
    .def(init<long>())
    .def(init<string>())

    .def(self += self)
    .def(self += other<amount_t>())
    .def(self += long())
    .def(self +  self)
    .def(self +  other<amount_t>())
    .def(self +  long())
    .def(self -= self)
    .def(self -= other<amount_t>())
    .def(self -= long())
    .def(self -  self)
    .def(self -  other<amount_t>())
    .def(self -  long())
    .def(self *= other<amount_t>())
    .def(self *= long())
    .def(self *  other<amount_t>())
    .def(self *  long())
    .def(self /= other<amount_t>())
    .def(self /= long())
    .def(self /  other<amount_t>())
    .def(self /  long())
    .def(- self)

    .def(self == self)
    .def(self == other<amount_t>())
    .def(self == long())
    .def(self != self)
    .def(self != other<amount_t>())
    .def(self != long())
    .def(! self)

    .def("__str__", &balance_t::to_string)
    .def("to_string", &balance_t::to_string)
    .def("__unicode__", py_balance_unicode)

    .def("negated", &balance_t::negated)
    .def("in_place_negate", &balance_t::in_place_negate,
         return_internal_reference<>())
    .def(- self)

    .def("abs", &balance_t::abs)
    .def("__abs__", &balance_t::abs)

    .def("__len__", balance_len)
    .def("__getitem__", balance_getitem)

    .def("rounded", &balance_t::rounded)
    .def("in_place_round", &balance_t::in_place_round,
         return_internal_reference<>())

    .def("truncated", &balance_t::truncated)
    .def("in_place_truncate", &balance_t::in_place_truncate,
         return_internal_reference<>())

    .def("floored", &balance_t::floored)
    .def("in_place_floor", &balance_t::in_place_floor,
         return_internal_reference<>())

    .def("unrounded", &balance_t::unrounded)
    .def("in_place_unround", &balance_t::in_place_unround,
         return_internal_reference<>())

    .def("reduced", &balance_t::reduced)
    .def("in_place_reduce", &balance_t::in_place_reduce,
         return_internal_reference<>())

    .def("unreduced", &balance_t::unreduced)
    .def("in_place_unreduce", &balance_t::in_place_unreduce,
         return_internal_reference<>())

    .def("value", py_value_0)
    .def("value", py_value_1, args("in_terms_of"))
    .def("value", py_value_2, args("in_terms_of", "moment"))
    .def("value", py_value_2d, args("in_terms_of", "moment"))

    .def("__nonzero__", &balance_t::is_nonzero)
    .def("is_nonzero", &balance_t::is_nonzero)
    .def("is_zero", &balance_t::is_zero)
    .def("is_realzero", &balance_t::is_realzero)

    .def("is_empty", &balance_t::is_empty)
    .def("single_amount", &balance_t::single_amount)

    .def("to_amount", &balance_t::to_amount)

    .def("commodity_count", &balance_t::commodity_count)
    .def("commodity_amount", py_commodity_amount_0)
    .def("commodity_amount", py_commodity_amount_1)

    .def("number", &balance_t::number)

    .def("strip_annotations", py_strip_annotations_0)
    .def("strip_annotations", py_strip_annotations_1)

    .def("valid",  &balance_t::valid)
    ;

  register_optional_to_python<balance_t>();

  implicitly_convertible<long, balance_t>();
  implicitly_convertible<string, balance_t>();
  implicitly_convertible<amount_t, balance_t>();

#define EXC_TRANSLATE(type) \
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(balance_error);
}

} // namespace ledger
