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
#include "pool.h"

namespace ledger {

using namespace boost::python;

namespace {

  boost::optional<amount_t> py_value_0(const amount_t& amount) {
    return amount.value(CURRENT_TIME());
  }
  boost::optional<amount_t> py_value_1(const amount_t& amount,
                                       const commodity_t * in_terms_of) {
    return amount.value(CURRENT_TIME(), in_terms_of);
  }
  boost::optional<amount_t> py_value_2(const amount_t& amount,
                                       const commodity_t * in_terms_of,
                                       const datetime_t& moment) {
    return amount.value(moment, in_terms_of);
  }
  boost::optional<amount_t> py_value_2d(const amount_t& amount,
                                        const commodity_t * in_terms_of,
                                        const date_t& moment) {
    return amount.value(datetime_t(moment), in_terms_of);
  }

  void py_parse_2(amount_t& amount, object in, unsigned char flags) {
    if (PyFile_Check(in.ptr())) {
      pyifstream instr(reinterpret_cast<PyFileObject *>(in.ptr()));
      amount.parse(instr, flags);
    } else {
      PyErr_SetString(PyExc_IOError,
                      _("Argument to amount.parse(file) is not a file object"));
    }
  }
  void py_parse_1(amount_t& amount, object in) {
    py_parse_2(amount, in, 0);
  }

  void py_parse_str_1(amount_t& amount, const string& str) {
    amount.parse(str);
  }
  void py_parse_str_2(amount_t& amount, const string& str, unsigned char flags) {
    amount.parse(str, flags);
  }

#if 0
  void py_print(amount_t& amount, object out) {
    if (PyFile_Check(out.ptr())) {
      pyofstream outstr(reinterpret_cast<PyFileObject *>(out.ptr()));
      amount.print(outstr);
    } else {
      PyErr_SetString(PyExc_IOError,
                      _("Argument to amount.print_(file) is not a file object"));
    }
  }
#endif

  annotation_t& py_amount_annotation(amount_t& amount) {
    return amount.annotation();
  }

  amount_t py_strip_annotations_0(amount_t& amount) {
    return amount.strip_annotations(keep_details_t());
  }
  amount_t py_strip_annotations_1(amount_t& amount, const keep_details_t& keep) {
    return amount.strip_annotations(keep);
  }

  PyObject * py_amount_unicode(amount_t& amount) {
    return str_to_py_unicode(amount.to_string());
  }

} // unnamed namespace

#define EXC_TRANSLATOR(type)                            \
  void exc_translate_ ## type(const type& err) {        \
    PyErr_SetString(PyExc_ArithmeticError, err.what()); \
  }

EXC_TRANSLATOR(amount_error)

void export_amount()
{
  class_< amount_t > ("Amount")
    .def("initialize", &amount_t::initialize) // only for the PyUnitTests
    .staticmethod("initialize")
    .def("shutdown", &amount_t::shutdown)
    .staticmethod("shutdown")

    .add_static_property("is_initialized",
                         make_getter(&amount_t::is_initialized),
                         make_setter(&amount_t::is_initialized))
    .add_static_property("stream_fullstrings",
                         make_getter(&amount_t::stream_fullstrings),
                         make_setter(&amount_t::stream_fullstrings))

    .def(init<long>())
    .def(init<std::string>())

    .def("exact", &amount_t::exact, args("value"),
         _("Construct an amount object whose display precision is always equal to its\n\
internal precision."))
    .staticmethod("exact")

    .def(init<amount_t>())

    .def("compare", &amount_t::compare, args("amount"),
         _("Compare two amounts for equality, returning <0, 0 or >0."))

    .def(self == self)
    .def(self == long())
    .def(long() == self)

    .def(self != self)
    .def(self != long())
    .def(long() != self)

    .def(! self)

    .def(self <  self)
    .def(self <  long())
    .def(long() < self)

    .def(self <= self)
    .def(self <= long())
    .def(long() <= self)

    .def(self >  self)
    .def(self >  long())
    .def(long() > self)

    .def(self >= self)
    .def(self >= long())
    .def(long() >= self)

    .def(self += self)
    .def(self += long())

    .def(self     + self)
    .def(self     + long())
    .def(long()   + self)

    .def(self -= self)
    .def(self -= long())

    .def(self     - self)
    .def(self     - long())
    .def(long()   - self)

    .def(self *= self)
    .def(self *= long())

    .def(self     * self)
    .def(self     * long())
    .def(long()   * self)

    .def(self /= self)
    .def(self /= long())

    .def(self     /  self)
    .def(self     /  long())
    .def(long()   / self)

    .add_property("precision", &amount_t::precision)
    .add_property("display_precision", &amount_t::display_precision)
    .add_property("keep_precision",
                  &amount_t::keep_precision,
                  &amount_t::set_keep_precision)

    .def("negated", &amount_t::negated)
    .def("in_place_negate", &amount_t::in_place_negate,
         return_internal_reference<>())
    .def(- self)

    .def("abs", &amount_t::abs)
    .def("__abs__", &amount_t::abs)

    .def("inverted", &amount_t::inverted)

    .def("rounded", &amount_t::rounded)
    .def("in_place_round", &amount_t::in_place_round,
         return_internal_reference<>())

    .def("truncated", &amount_t::truncated)
    .def("in_place_truncate", &amount_t::in_place_truncate,
         return_internal_reference<>())

    .def("floored", &amount_t::floored)
    .def("in_place_floor", &amount_t::in_place_floor,
         return_internal_reference<>())

    .def("unrounded", &amount_t::unrounded)
    .def("in_place_unround", &amount_t::in_place_unround,
         return_internal_reference<>())

    .def("reduced", &amount_t::reduced)
    .def("in_place_reduce", &amount_t::in_place_reduce,
         return_internal_reference<>())

    .def("unreduced", &amount_t::unreduced)
    .def("in_place_unreduce", &amount_t::in_place_unreduce,
         return_internal_reference<>())

    .def("value", py_value_0)
    .def("value", py_value_1, args("in_terms_of"))
    .def("value", py_value_2, args("in_terms_of", "moment"))
    .def("value", py_value_2d, args("in_terms_of", "moment"))

    .def("price", &amount_t::price)

    .def("sign", &amount_t::sign)
    .def("__nonzero__", &amount_t::is_nonzero)
    .def("is_nonzero", &amount_t::is_nonzero)
    .def("is_zero", &amount_t::is_zero)
    .def("is_realzero", &amount_t::is_realzero)
    .def("is_null", &amount_t::is_null)

    .def("to_double", &amount_t::to_double)
    .def("__float__", &amount_t::to_double)
    .def("to_long", &amount_t::to_long)
    .def("__int__", &amount_t::to_long)
    .def("fits_in_long", &amount_t::fits_in_long)

    .def("__str__", &amount_t::to_string)
    .def("to_string", &amount_t::to_string)
    .def("__unicode__", py_amount_unicode)
    .def("to_fullstring", &amount_t::to_fullstring)
    .def("__repr__", &amount_t::to_fullstring)
    .def("quantity_string", &amount_t::quantity_string)

    .add_property("commodity",
                  make_function(&amount_t::commodity,
                                return_internal_reference<>()),
                  make_function(&amount_t::set_commodity,
                                with_custodian_and_ward<1, 2>()))
    .def("has_commodity", &amount_t::has_commodity)
    .def("with_commodity", &amount_t::with_commodity)
    .def("clear_commodity", &amount_t::clear_commodity)

    .def("number", &amount_t::number)

    .def("annotate", &amount_t::annotate)
    .def("has_annotation", &amount_t::has_annotation)
    .add_property("annotation",
                  make_function(py_amount_annotation,
                                return_internal_reference<>()))
    .def("strip_annotations", py_strip_annotations_0)
    .def("strip_annotations", py_strip_annotations_1)

    .def("parse", py_parse_1)
    .def("parse", py_parse_2)
    .def("parse", py_parse_str_1)
    .def("parse", py_parse_str_2)

    .def("parse_conversion", &amount_t::parse_conversion)
    .staticmethod("parse_conversion")

    .def("valid", &amount_t::valid)
    ;

  enum_< parse_flags_enum_t >("ParseFlags")
    .value("Default",   PARSE_DEFAULT)
    .value("Partial",   PARSE_PARTIAL)
    .value("Single",    PARSE_SINGLE)
    .value("NoMigrate", PARSE_NO_MIGRATE)
    .value("NoReduce",  PARSE_NO_REDUCE)
    .value("NoAssign",  PARSE_NO_ASSIGN)
    .value("OpContext", PARSE_OP_CONTEXT)
    .value("SoftFail",  PARSE_SOFT_FAIL)
    ;

  register_optional_to_python<amount_t>();

  implicitly_convertible<long, amount_t>();
  implicitly_convertible<string, amount_t>();

#define EXC_TRANSLATE(type) \
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(amount_error);
}

} // namespace ledger
