#include "pyinterp.h"
#include "amount.h"

using namespace boost::python;

namespace ledger {

int py_amount_quantity(amount_t& amount)
{
  std::ostringstream quant;
  amount.print_quantity(quant);
  return std::atol(quant.str().c_str());
}

void py_parse_1(amount_t& amount, const string& str,
		unsigned char flags) {
  amount.parse(str, flags);
}
void py_parse_2(amount_t& amount, const string& str) {
  amount.parse(str);
}

amount_t py_round_1(amount_t& amount, unsigned int prec) {
  return amount.round(prec);
}
amount_t py_round_2(amount_t& amount) {
  return amount.round();
}

struct commodity_updater_wrap : public commodity_base_t::updater_t
{
  PyObject * self;
  commodity_updater_wrap(PyObject * self_) : self(self_) {}

  virtual void operator()(commodity_base_t& commodity,
			  const moment_t& moment,
			  const moment_t& date,
			  const moment_t& last,
			  amount_t&         price) {
    call_method<void>(self, "__call__", commodity, moment, date, last, price);
  }
};

commodity_t * py_find_commodity(const string& symbol)
{
  return commodity_t::find(symbol);
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_ArithmeticError, err.what());	\
  }

EXC_TRANSLATOR(amount_exception)

void export_amount()
{
  scope().attr("AMOUNT_PARSE_NO_MIGRATE") = AMOUNT_PARSE_NO_MIGRATE;
  scope().attr("AMOUNT_PARSE_NO_REDUCE")  = AMOUNT_PARSE_NO_REDUCE;

  class_< amount_t > ("amount")
    .def(init<amount_t>())
    .def(init<std::string>())
    .def(init<char *>())
    .def(init<long>())
    .def(init<double>())

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

    .def(- self)

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

    .def(self == self)
    .def(self == long())
    .def(long() == self)

    .def(self != self)
    .def(self != long())
    .def(long() != self)

    .def(! self)

    .def(self_ns::int_(self))
    .def(self_ns::float_(self))

    .def("__str__", &amount_t::to_string)
    .def("__repr__", &amount_t::to_fullstring)

    .def("has_commodity", &amount_t::has_commodity)

    .add_property("commodity",
		  make_function(&amount_t::commodity,
				return_value_policy<reference_existing_object>()),
		  make_function(&amount_t::set_commodity,
				with_custodian_and_ward<1, 2>()))

    .def("annotate_commodity", &amount_t::annotate_commodity)
    .def("strip_annotations", &amount_t::strip_annotations)
    .def("clear_commodity", &amount_t::clear_commodity)

    //.add_static_property("full_strings", &amount_t::full_strings)

    .def("to_string", &amount_t::to_string)
    .def("to_fullstring", &amount_t::to_fullstring)
    .def("quantity_string", &amount_t::quantity_string)

    .def("exact", &amount_t::exact)
    .staticmethod("exact")

    .def("__abs__", &amount_t::abs)
    .def("compare", &amount_t::compare)
    .def("date", &amount_t::date)
    .def("negate", &amount_t::negate)
    .def("null", &amount_t::null)
    .def("parse", py_parse_1)
    .def("parse", py_parse_2)
    .def("price", &amount_t::price)
    .def("realzero", &amount_t::realzero)
    .def("reduce", &amount_t::reduce)
    .def("round", py_round_1)
    .def("round", py_round_2)
    .def("sign", &amount_t::sign)
    .def("unround", &amount_t::unround)
    .def("value", &amount_t::value)
    .def("zero", &amount_t::zero)

    .def("valid", &amount_t::valid)

    .def("initialize", &amount_t::initialize)
    .staticmethod("initialize")
    .def("shutdown", &amount_t::shutdown)
    .staticmethod("shutdown")
    ;

  class_< commodity_base_t::updater_t, commodity_updater_wrap,
	  boost::noncopyable >
    ("updater")
    ;

  scope().attr("COMMODITY_STYLE_DEFAULTS")  = COMMODITY_STYLE_DEFAULTS;
  scope().attr("COMMODITY_STYLE_SUFFIXED")  = COMMODITY_STYLE_SUFFIXED;
  scope().attr("COMMODITY_STYLE_SEPARATED") = COMMODITY_STYLE_SEPARATED;
  scope().attr("COMMODITY_STYLE_EUROPEAN")  = COMMODITY_STYLE_EUROPEAN;
  scope().attr("COMMODITY_STYLE_THOUSANDS") = COMMODITY_STYLE_THOUSANDS;
  scope().attr("COMMODITY_STYLE_NOMARKET")  = COMMODITY_STYLE_NOMARKET;
  scope().attr("COMMODITY_STYLE_BUILTIN")   = COMMODITY_STYLE_BUILTIN;

  class_< commodity_t > ("commodity")
#if 0
    .add_property("symbol", &commodity_t::symbol)

    .add_property("name", &commodity_t::name, &commodity_t::set_name)
    .add_property("note", &commodity_t::note, &commodity_t::set_note)
    .add_property("precision", &commodity_t::precision,
		  &commodity_t::set_precision)
    .add_property("flags", &commodity_t::flags, &commodity_t::set_flags)
    .add_property("add_flags", &commodity_t::add_flags)
    .add_property("drop_flags", &commodity_t::drop_flags)
    //.add_property("updater", &commodity_t::updater)

    .add_property("smaller",
		  make_getter(&commodity_t::smaller,
			      return_value_policy<reference_existing_object>()),
		  make_setter(&commodity_t::smaller,
			      return_value_policy<reference_existing_object>()))
    .add_property("larger",
		  make_getter(&commodity_t::larger,
			      return_value_policy<reference_existing_object>()),
		  make_setter(&commodity_t::larger,
			      return_value_policy<reference_existing_object>()))

    .def(self_ns::str(self))

    .def("find", py_find_commodity,
	 return_value_policy<reference_existing_object>())
    .staticmethod("find")

    .def("add_price", &commodity_t::add_price)
    .def("remove_price", &commodity_t::remove_price)
    .def("value", &commodity_t::value)

    .def("valid", &commodity_t::valid)
#endif
    ;

#define EXC_TRANSLATE(type)					\
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(amount_exception);
}

} // namespace ledger
