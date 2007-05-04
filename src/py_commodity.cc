#include "pyinterp.h"
#include "amount.h"

#include <boost/python/exception_translator.hpp>

namespace ledger {

using namespace boost::python;

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

EXC_TRANSLATOR(commodity_error)

void export_commodity()
{
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
    ;

#define EXC_TRANSLATE(type) \
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(commodity_error);
}

} // namespace ledger
