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

namespace ledger {

using namespace boost::python;

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(value_overloads, value, 0, 2)
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(exchange_commodities_overloads,
                                       exchange_commodities, 1, 2)
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(set_string_overloads, set_string, 0, 2)

namespace {

  boost::optional<value_t> py_value_0(const value_t& value) {
    return value.value(CURRENT_TIME());
  }
  boost::optional<value_t> py_value_1(const value_t& value,
                                      const commodity_t * in_terms_of) {
    return value.value(CURRENT_TIME(), in_terms_of);
  }
  boost::optional<value_t> py_value_2(const value_t& value,
                                      const commodity_t * in_terms_of,
                                      const datetime_t& moment) {
    return value.value(moment, in_terms_of);
  }
  boost::optional<value_t> py_value_2d(const value_t& value,
                                       const commodity_t * in_terms_of,
                                       const date_t& moment) {
    return value.value(datetime_t(moment), in_terms_of);
  }

  PyObject * py_base_type(value_t& value)
  {
    if (value.is_boolean()) {
      return (PyObject *)&PyBool_Type;
    }
    else if (value.is_long()) {
      return (PyObject *)&PyInt_Type;
    }
    else if (value.is_string()) {
      return (PyObject *)&PyUnicode_Type;
    }
    else {
      object typeobj(object(value).attr("__class__"));
      return typeobj.ptr();
    }
  }

  string py_dump(const value_t& value) {
    std::ostringstream buf;
    value.dump(buf);
    return buf.str();
  }

  string py_dump_relaxed(const value_t& value) {
    std::ostringstream buf;
    value.dump(buf, true);
    return buf.str();
  }

  void py_set_string(value_t& value, const string& str) {
    return value.set_string(str);
  }

  annotation_t& py_value_annotation(value_t& value) {
    return value.annotation();
  }

  value_t py_strip_annotations_0(value_t& value) {
    return value.strip_annotations(keep_details_t());
  }
  value_t py_strip_annotations_1(value_t& value, const keep_details_t& keep) {
    return value.strip_annotations(keep);
  }

  PyObject * py_value_unicode(value_t& value) {
    return str_to_py_unicode(value.to_string());
  }

} // unnamed namespace

#define EXC_TRANSLATOR(type)                            \
  void exc_translate_ ## type(const type& err) {        \
    PyErr_SetString(PyExc_ArithmeticError, err.what()); \
  }

EXC_TRANSLATOR(value_error)

void export_value()
{
  enum_< value_t::type_t >("ValueType")
    .value("Void",         value_t::VOID)
    .value("Boolean",      value_t::BOOLEAN)
    .value("DateTime",     value_t::DATETIME)
    .value("Date",         value_t::DATE)
    .value("Integer",      value_t::INTEGER)
    .value("Amount",       value_t::AMOUNT)
    .value("Balance",      value_t::BALANCE)
    .value("String",       value_t::STRING)
    .value("Sequence",     value_t::SEQUENCE)
    .value("Scope",        value_t::SCOPE)
    ;

  class_< value_t > ("Value")
    .def("initialize", &value_t::initialize)
    .staticmethod("initialize")
    .def("shutdown", &value_t::shutdown)
    .staticmethod("shutdown")

    .def(init<bool>())
    .def(init<datetime_t>())
    .def(init<date_t>())
    .def(init<long>())
    .def(init<double>())
    .def(init<amount_t>())
    .def(init<balance_t>())
    .def(init<mask_t>())
    .def(init<std::string>())
    // jww (2009-11-02): Need to support conversion of value_t::sequence_t
    //.def(init<value_t::sequence_t>())
    .def(init<value_t>())

    .def("is_equal_to", &value_t::is_equal_to)
    .def("is_less_than", &value_t::is_less_than)
    .def("is_greater_than", &value_t::is_greater_than)

    .def(self == self)
    .def(self == long())
    .def(long() == self)
    .def(self == other<amount_t>())
    .def(other<amount_t>() == self)
    .def(self == other<balance_t>())
    .def(other<balance_t>() == self)

    .def(self != self)
    .def(self != long())
    .def(long() != self)
    .def(self != other<amount_t>())
    .def(other<amount_t>() != self)
    .def(self != other<balance_t>())
    .def(other<balance_t>() != self)

    .def(! self)

    .def(self <  self)
    .def(self <  long())
    .def(long() < self)
    .def(self < other<amount_t>())
    .def(other<amount_t>() < self)

    .def(self <= self)
    .def(self <= long())
    .def(long() <= self)
    .def(self <= other<amount_t>())
    .def(other<amount_t>() <= self)

    .def(self >  self)
    .def(self >  long())
    .def(long() > self)
    .def(self > other<amount_t>())
    .def(other<amount_t>() > self)

    .def(self >= self)
    .def(self >= long())
    .def(long() >= self)
    .def(self >= other<amount_t>())
    .def(other<amount_t>() >= self)

    .def(self += self)
    .def(self += long())
    .def(self += other<amount_t>())
    .def(self += other<balance_t>())

    .def(self   + self)
    .def(self   + long())
    .def(long() + self)
    .def(self + other<amount_t>())
    .def(other<amount_t>() + self)
    .def(self + other<balance_t>())

    .def(self -= self)
    .def(self -= long())
    .def(self -= other<amount_t>())
    .def(self -= other<balance_t>())

    .def(self   - self)
    .def(self   - long())
    .def(long() - self)
    .def(self - other<amount_t>())
    .def(other<amount_t>() - self)
    .def(self - other<balance_t>())

    .def(self *= self)
    .def(self *= long())
    .def(self *= other<amount_t>())

    .def(self   * self)
    .def(self   * long())
    .def(long() * self)
    .def(self * other<amount_t>())
    .def(other<amount_t>() * self)

    .def(self /= self)
    .def(self /= long())
    .def(self /= other<amount_t>())

    .def(self   /  self)
    .def(self   /  long())
    .def(long() / self)
    .def(self / other<amount_t>())
    .def(other<amount_t>() / self)

    .def("negated", &value_t::negated)
    .def("in_place_negate", &value_t::in_place_negate)
    .def("in_place_not", &value_t::in_place_not)
    .def(- self)

    .def("abs", &value_t::abs)
    .def("__abs__", &value_t::abs)

    .def("rounded", &value_t::rounded)
    .def("in_place_round", &value_t::in_place_round)
    .def("truncated", &value_t::truncated)
    .def("in_place_truncate", &value_t::in_place_truncate)
    .def("floored", &value_t::floored)
    .def("in_place_floor", &value_t::in_place_floor)
    .def("unrounded", &value_t::unrounded)
    .def("in_place_unround", &value_t::in_place_unround)
    .def("reduced", &value_t::reduced)
    .def("in_place_reduce", &value_t::in_place_reduce)
    .def("unreduced", &value_t::unreduced)
    .def("in_place_unreduce", &value_t::in_place_unreduce)

    .def("value", py_value_0)
    .def("value", py_value_1, args("in_terms_of"))
    .def("value", py_value_2, args("in_terms_of", "moment"))
    .def("value", py_value_2d, args("in_terms_of", "moment"))

    //.def("value", &value_t::value, value_overloads())
    .def("exchange_commodities", &value_t::exchange_commodities,
         exchange_commodities_overloads())

    .def("__nonzero__", &value_t::is_nonzero)
    .def("is_nonzero", &value_t::is_nonzero)
    .def("is_realzero", &value_t::is_realzero)
    .def("is_zero", &value_t::is_zero)
    .def("is_null", &value_t::is_null)

    .def("type", &value_t::type)
    .def("is_type", &value_t::is_type)

    .def("is_boolean", &value_t::is_boolean)
    .def("set_boolean", &value_t::set_boolean)

    .def("is_datetime", &value_t::is_datetime)
    .def("set_datetime", &value_t::set_datetime)

    .def("is_date", &value_t::is_date)
    .def("set_date", &value_t::set_date)

    .def("is_long", &value_t::is_long)
    .def("set_long", &value_t::set_long)

    .def("is_amount", &value_t::is_amount)
    .def("is_amount", &value_t::is_amount)

    .def("is_balance", &value_t::is_balance)
    .def("is_balance", &value_t::is_balance)

    .def("is_string", &value_t::is_string)
    .def("set_string", py_set_string)

    .def("is_mask", &value_t::is_mask)
    .def("is_mask", &value_t::is_mask)

    .def("is_sequence", &value_t::is_sequence)
    .def("set_sequence", &value_t::set_sequence)

    .def("to_boolean", &value_t::to_boolean)
    .def("to_long", &value_t::to_long)
    .def("__int__", &value_t::to_long)
    .def("to_datetime", &value_t::to_datetime)
    .def("to_date", &value_t::to_date)
    .def("to_amount", &value_t::to_amount)
    .def("to_balance", &value_t::to_balance)
    .def("__str__", &value_t::to_string)
    .def("__unicode__", py_value_unicode)
    .def("to_string", &value_t::to_string)
    .def("to_mask", &value_t::to_mask)
    .def("to_sequence", &value_t::to_sequence)

    .def("__repr__", py_dump)

    .def("casted", &value_t::casted)
    .def("in_place_cast", &value_t::in_place_cast)
    .def("simplified", &value_t::simplified)
    .def("in_place_simplify", &value_t::in_place_simplify)

    .def("number", &value_t::number)

    .def("annotate", &value_t::annotate)
    .def("has_annotation", &value_t::has_annotation)
    .add_property("annotation",
                  make_function(py_value_annotation,
                                return_internal_reference<>()))
    .def("strip_annotations", py_strip_annotations_0)
    .def("strip_annotations", py_strip_annotations_1)

#if 0
    .def("__getitem__", &value_t::operator[])
#endif
    .def("push_back", &value_t::push_back)
    .def("pop_back", &value_t::pop_back)
    .def("size", &value_t::size)

    .def("label", &value_t::label)

    .def("valid", &value_t::valid)

    .def("basetype", py_base_type)
    ;

#if 0
  // jww (2010-06-10): This is not working since I switched sequence_t to
  // ptr_deque<value_t>.
  class_< value_t::sequence_t > ("ValueSequence")
    .def(vector_indexing_suite< value_t::sequence_t, true >());
    ;
#endif

  scope().attr("NULL_VALUE")    = NULL_VALUE;
  scope().attr("string_value")  = &string_value;
  scope().attr("mask_value")    = &mask_value;
  scope().attr("value_context") = &value_context;

  register_optional_to_python<value_t>();

  implicitly_convertible<bool, value_t>();
  implicitly_convertible<long, value_t>();
  implicitly_convertible<string, value_t>();
  implicitly_convertible<amount_t, value_t>();
  implicitly_convertible<balance_t, value_t>();
  implicitly_convertible<mask_t, value_t>();
  implicitly_convertible<date_t, value_t>();
  implicitly_convertible<datetime_t, value_t>();

#define EXC_TRANSLATE(type) \
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(value_error);
}

} // namespace ledger
