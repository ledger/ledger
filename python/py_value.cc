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

#include "pyinterp.h"
#include "pyutils.h"
#include "value.h"

#include <boost/python/exception_translator.hpp>
#include <boost/python/implicit.hpp>
#include <boost/python/args.hpp>

namespace ledger {

using namespace boost::python;

boost::optional<value_t> py_value_0(const value_t& amount) {
  return amount.value();
}
boost::optional<value_t> py_value_1(const value_t& amount,
				    const boost::optional<datetime_t>& moment) {
  return amount.value(moment);
}
boost::optional<value_t> py_value_2(const value_t& amount,
				    const boost::optional<datetime_t>& moment,
				    const boost::optional<commodity_t&>& in_terms_of) {
  return amount.value(moment, in_terms_of);
}

void py_set_string(value_t& amount, const string& str) {
  return amount.set_string(str);
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_ArithmeticError, err.what());	\
  }

EXC_TRANSLATOR(value_error)

void export_value()
{
  class_< value_t > ("Value")
#if 0
    .def("initialize", &value_t::initialize)
    .staticmethod("initialize")
    .def("shutdown", &value_t::shutdown)
    .staticmethod("shutdown")
#endif

    .def(init<double>())
    .def(init<long>())
    .def(init<std::string>())
    .def(init<date_t>())
    .def(init<datetime_t>())
    .def(init<amount_t>())
#if 0
    .def(init<balance_t>())
    .def(init<balance_pair_t>())
#endif

    .def(init<value_t>())

    .def("is_equal_to", &value_t::is_equal_to)
    .def("is_less_than", &value_t::is_less_than)
    .def("is_greater_than", &value_t::is_greater_than)

    .def(self == self)
    .def(self == long())
    .def(long() == self)
    .def(self == double())
    .def(double() == self)

    .def(self != self)
    .def(self != long())
    .def(long() != self)
    .def(self != double())
    .def(double() != self)

    .def(! self)

    .def(self <  self)
    .def(self <  long())
    .def(long() < self)
    .def(self <  double())
    .def(double() < self)

    .def(self <= self)
    .def(self <= long())
    .def(long() <= self)
    .def(self <= double())
    .def(double() <= self)

    .def(self >  self)
    .def(self >  long())
    .def(long() > self)
    .def(self >  double())
    .def(double() > self)

    .def(self >= self)
    .def(self >= long())
    .def(long() >= self)
    .def(self >= double())
    .def(double() >= self)

    .def(self += self)
    .def(self += long())
    .def(self += double())

    .def(self	  + self)
    .def(self	  + long())
    .def(long()	  + self)
    .def(self	  + double())
    .def(double() + self)

    .def(self -= self)
    .def(self -= long())
    .def(self -= double())

    .def(self	  - self)
    .def(self	  - long())
    .def(long()	  - self)
    .def(self	  - double())
    .def(double() - self)

    .def(self *= self)
    .def(self *= long())
    .def(self *= double())

    .def(self	  * self)
    .def(self	  * long())
    .def(long()	  * self)
    .def(self	  * double())
    .def(double() * self)

    .def(self /= self)
    .def(self /= long())
    .def(self /= double())

    .def(self	  /  self)
    .def(self	  /  long())
    .def(long()	  / self)
    .def(self	  /  double())
    .def(double() / self)

    .def("add", &value_t::add,
	 return_value_policy<reference_existing_object>())

    .def("negate", &value_t::negate)
    .def("in_place_negate", &value_t::in_place_negate)
    .def("in_place_not", &value_t::in_place_not)
    .def(- self)

    .def("abs", &value_t::abs)
    .def("__abs__", &value_t::abs)

    .def("rounded", &value_t::rounded)
    .def("unrounded", &value_t::unrounded)

    .def("reduce", &value_t::reduce)
    .def("in_place_reduce", &value_t::in_place_reduce)

    .def("value", py_value_0)
    .def("value", py_value_1)
    .def("value", py_value_2)

    .def("cost", &value_t::cost)

    .def("__nonzero__", &value_t::is_nonzero)
    .def("is_nonzero", &value_t::is_nonzero)
    .def("is_realzero", &value_t::is_realzero)
    .def("is_zero", &value_t::is_zero)
    .def("is_null", &value_t::is_null)

    .def("type", &value_t::type)
    .def("is_type", &value_t::is_type)

    .def("is_boolean", &value_t::is_boolean)
    .def("set_boolean", &value_t::set_boolean)

    .def("is_boolean", &value_t::is_boolean)
    .def("set_boolean", &value_t::set_boolean)

    .def("is_datetime", &value_t::is_datetime)
    .def("set_datetime", &value_t::set_datetime)

    .def("is_date", &value_t::is_date)
    .def("set_date", &value_t::set_date)

    .def("is_long", &value_t::is_long)
    .def("set_long", &value_t::set_long)

    .def("is_amount", &value_t::is_amount)
    .def("set_amount", &value_t::set_amount)

    .def("is_balance", &value_t::is_balance)
    .def("set_balance", &value_t::set_balance)

    .def("is_balance_pair", &value_t::is_balance_pair)
    .def("set_balance_pair", &value_t::set_balance_pair)

    .def("is_string", &value_t::is_string)
    .def("set_string", py_set_string)

    .def("is_sequence", &value_t::is_sequence)
    .def("set_sequence", &value_t::set_sequence)

    .def("to_boolean", &value_t::to_boolean)
    .def("to_long", &value_t::to_long)
    .def("to_datetime", &value_t::to_datetime)
    .def("to_date", &value_t::to_date)
    .def("to_amount", &value_t::to_amount)
    .def("to_balance", &value_t::to_balance)
    .def("to_balance_pair", &value_t::to_balance_pair)
    .def("to_string", &value_t::to_string)
    .def("to_sequence", &value_t::to_sequence)

    .def("cast", &value_t::cast)
    .def("in_place_cast", &value_t::in_place_cast)

    .def("simplify", &value_t::simplify)
    .def("in_place_simplify", &value_t::in_place_simplify)

    .def("strip_annotations", &value_t::strip_annotations)

    // jww (2009-01-28): Allow for transparent exchanging with sequence
    // protocol objects in Python too; and conversion to a list.
#if 0
    .def("__getitem__", &value_t::operator[])
#endif
    .def("push_back", &value_t::push_back)
    .def("pop_back", &value_t::pop_back)
    .def("size", &value_t::size)

    .def("label", &value_t::label)

    .def("dump", &value_t::dump)
    .def("print", &value_t::print)

    .def("valid", &value_t::valid)
    ;

  enum_< value_t::type_t >("ValueType")
    .value("VOID",	   value_t::VOID)
    .value("BOOLEAN",	   value_t::BOOLEAN)
    .value("DATETIME",	   value_t::DATETIME)
    .value("DATE",	   value_t::DATE)
    .value("INTEGER",	   value_t::INTEGER)
    .value("AMOUNT",	   value_t::AMOUNT)
    .value("BALANCE",	   value_t::BALANCE)
    .value("BALANCE_PAIR", value_t::BALANCE_PAIR)
    .value("STRING",	   value_t::STRING)
    .value("SEQUENCE",	   value_t::SEQUENCE)
    .value("POINTER",	   value_t::POINTER)
    ;

  scope().attr("NULL_VALUE")    = NULL_VALUE;
  scope().attr("string_value")  = &string_value;
  scope().attr("value_context") = &value_context;

  register_optional_to_python<value_t>();

  implicitly_convertible<double, value_t>();
  implicitly_convertible<long, value_t>();
  implicitly_convertible<string, value_t>();
  implicitly_convertible<date_t, value_t>();
  implicitly_convertible<datetime_t, value_t>();
  implicitly_convertible<amount_t, value_t>();

#define EXC_TRANSLATE(type) \
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(value_error);
}

} // namespace ledger
