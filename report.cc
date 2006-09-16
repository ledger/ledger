#ifdef USE_PCH
#include "pch.h"
#else
#include "report.h"
#include "repitem.h"
#include "transform.h"
#endif

namespace ledger {

report_t::~report_t()
{
  for (std::list<transform_t *>::const_iterator i = transforms.begin();
       i != transforms.end();
       i++)
    delete *i;
}

void report_t::apply_transforms(repitem_t * items)
{
  for (std::list<transform_t *>::const_iterator i = transforms.begin();
       i != transforms.end();
       i++)
    (*i)->walk_items(items);
}

valexpr_t::node_t * report_t::lookup(const std::string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'o':
    if (std::strncmp(p, "option_", 7) == 0) {
      p = p + 7;
      switch (*p) {
      case 'b':
	if (std::strcmp(p, "bar") == 0)
	  return MAKE_FUNCTOR(report_t, option_bar);
	break;
      case 'f':
	if (std::strcmp(p, "foo") == 0)
	  return MAKE_FUNCTOR(report_t, option_foo);
	break;
      }
    }
    break;
  }
  return valexpr_t::scope_t::lookup(name);
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#ifndef USE_PCH
#include <boost/python.hpp>
#endif

using namespace boost::python;
using namespace ledger;

void export_report()
{
  class_< report_t > ("Report")
    .add_property("session",
		  make_getter(&report_t::session,
			      return_value_policy<reference_existing_object>()))

    .def("apply_transforms", &report_t::apply_transforms)
    ;
}

#endif // USE_BOOST_PYTHON
