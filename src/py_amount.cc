#include "pyinterp.h"
#include "amount.h"

#include <boost/python/exception_translator.hpp>

namespace ledger {

using namespace boost::python;

void py_parse_1(amount_t& amount, const string& str, unsigned char flags) {
  amount.parse(str, flags);
}
void py_parse_2(amount_t& amount, const string& str) {
  amount.parse(str);
}

amount_t py_round_1(const amount_t& amount, amount_t::precision_t prec) {
  return amount.round(prec);
}
amount_t py_round_2(const amount_t& amount) {
  return amount.round();
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_ArithmeticError, err.what());	\
  }

EXC_TRANSLATOR(amount_error)

void export_amount()
{
  scope().attr("AMOUNT_PARSE_NO_MIGRATE") = AMOUNT_PARSE_NO_MIGRATE;
  scope().attr("AMOUNT_PARSE_NO_REDUCE")  = AMOUNT_PARSE_NO_REDUCE;

  class_< amount_t > ("amount")
#if 0
    .def("initialize", &amount_t::initialize)
    .staticmethod("initialize")
    .def("shutdown", &amount_t::shutdown)
    .staticmethod("shutdown")
#endif

#if 0
    .add_static_property("keep_base", &amount_t::keep_base)

    .add_static_property("keep_price", &amount_t::keep_price)
    .add_static_property("keep_date", &amount_t::keep_date)
    .add_static_property("keep_tag", &amount_t::keep_tag)

    .add_static_property("full_strings", &amount_t::full_strings)
#endif

    .def(init<double>())
    .def(init<long>())
    .def(init<std::string>())
    .def(init<char *>())

    .def("exact", &amount_t::exact)
    .staticmethod("exact")

    .def(init<amount_t>())

    .def("compare", &amount_t::compare)

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

    .def("negate", &amount_t::negate)
    .def("in_place_negate", &amount_t::in_place_negate,
	 return_value_policy<reference_existing_object>())
    .def(- self)

    .def("abs", &amount_t::abs)
    .def("__abs__", &amount_t::abs)

    .def("round", py_round_1)
    .def("round", py_round_2)
    .def("unround", &amount_t::unround)

    .def("reduce", &amount_t::reduce)
    .def("in_place_reduce", &amount_t::in_place_reduce,
	 return_value_policy<reference_existing_object>())

    .def("unreduce", &amount_t::unreduce)
    .def("in_place_unreduce", &amount_t::in_place_unreduce,
	 return_value_policy<reference_existing_object>())

    .def("value", &amount_t::value)

    .def("sign", &amount_t::sign)
    .def("__nonzero__", &amount_t::nonzero)
    .def("nonzero", &amount_t::nonzero)
    .def("zero", &amount_t::zero)
    .def("realzero", &amount_t::realzero)
    .def("is_null", &amount_t::is_null)

    .def("to_double", &amount_t::to_double)
    .def("__float__", &amount_t::to_double)
    .def("to_long", &amount_t::to_long)
    .def("__int__", &amount_t::to_long)
    .def("to_string", &amount_t::to_string)
    .def("__str__", &amount_t::to_string)
    .def("to_fullstring", &amount_t::to_fullstring)
    .def("__repr__", &amount_t::to_fullstring)

    .def("quantity_string", &amount_t::quantity_string)

    .def("has_commodity", &amount_t::has_commodity)

    .add_property("commodity",
		  make_function(&amount_t::commodity,
				return_value_policy<reference_existing_object>()),
		  make_function(&amount_t::set_commodity,
				with_custodian_and_ward<1, 2>()))

    .def("clear_commodity", &amount_t::clear_commodity)
    .def("number", &amount_t::number)

    .def("annotate_commodity", &amount_t::annotate_commodity)
    .def("strip_annotations", &amount_t::strip_annotations)

#if 0
    // jww (2007-05-03): This method depends on annotation_t
    .def("annotation_details", &amount_t::annotation_details)
#endif

    // jww (2007-05-03): There are four versions of this method now
    .def("parse", py_parse_1)
    .def("parse", py_parse_2)

#if 0
    // jww (2007-05-03): This method has two forms
    .def("parse_conversion", &amount_t::parse_conversion)
    .staticmethod("parse_conversion")
#endif

    .def("valid", &amount_t::valid)
    ;

#define EXC_TRANSLATE(type) \
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(amount_error);
}

} // namespace ledger
