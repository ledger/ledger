/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

using namespace boost::python;
using namespace ledger;

value_t py_calc_1(xpath_t::op_t& xpath_t, const details_t& item)
{
  value_t result;
  xpath_t.calc(result, item);
  return result;
}

template <typename T>
value_t py_calc(xpath_t::op_t& xpath_t, const T& item)
{
  value_t result;
  xpath_t.calc(result, details_t(item));
  return result;
}

xpath_t::op_t * py_parse_xpath_t_1(const string& str)
{
  return parse_xpath_t(str);
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_RuntimeError, err.what());	\
  }

EXC_TRANSLATOR(xpath_t_error)
EXC_TRANSLATOR(calc_error)
#if 0
EXC_TRANSLATOR(mask_error)
#endif

void export_xpath()
{
  class_< details_t > ("Details", init<const entry_t&>())
    .def(init<const transaction_t&>())
    .def(init<const account_t&>())
    .add_property("entry",
		  make_getter(&details_t::entry,
			      return_value_policy<reference_existing_object>()))
    .add_property("xact",
		  make_getter(&details_t::xact,
			      return_value_policy<reference_existing_object>()))
    .add_property("account",
		  make_getter(&details_t::account,
			      return_value_policy<reference_existing_object>()))
    ;

  class_< xpath_t::op_t > ("ValueExpr", init<xpath_t::op_t::kind_t>())
    .def("calc", py_calc_1)
    .def("calc", py_calc<account_t>)
    .def("calc", py_calc<entry_t>)
    .def("calc", py_calc<transaction_t>)
    ;

  def("parse_xpath_t", py_parse_xpath_t_1,
      return_value_policy<manage_new_object>());

  class_< item_predicate<transaction_t> >
    ("TransactionPredicate", init<string>())
    .def("__call__", &item_predicate<transaction_t>::operator())
    ;

  class_< item_predicate<account_t> >
    ("AccountPredicate", init<string>())
    .def("__call__", &item_predicate<account_t>::operator())
    ;

#define EXC_TRANSLATE(type)					\
  register_error_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(xpath_t_error);
  EXC_TRANSLATE(calc_error);
#if 0
  EXC_TRANSLATE(mask_error);
#endif
}
