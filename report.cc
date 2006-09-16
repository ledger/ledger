#ifdef USE_PCH
#include "pch.h"
#else
#include "report.h"
#include "repitem.h"
#include "transform.h"
#include "util.h"
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

void report_t::abbrev(value_t& result, valexpr_t::scope_t * locals)
{
  if (locals->args.size() < 2)
    throw new error("usage: abbrev(STRING, WIDTH [, STYLE, ABBREV_LEN])");

  std::string str = locals->args[0].to_string();
  long	      wid = locals->args[1];

  elision_style_t style = session->elision_style;
  if (locals->args.size() == 3)
    style = (elision_style_t)locals->args[2].to_integer();

  long abbrev_len = session->abbrev_length;
  if (locals->args.size() == 4)
    abbrev_len = locals->args[3].to_integer();

  result.set_string(abbreviate(str, wid, style, true, (int)abbrev_len));
}

void report_t::ftime(value_t& result, valexpr_t::scope_t * locals)
{
  if (locals->args.size() < 1)
    throw new error("usage: ftime(DATE [, DATE_FORMAT])");

  datetime_t date = locals->args[0].to_datetime();

  std::string date_format;
  if (locals->args.size() == 2)
    date_format = locals->args[1].to_string();
  else
    date_format = session->date_format;

  result.set_string(date.to_string(date_format));
}

valexpr_t::node_t * report_t::lookup(const std::string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'a':
    if (name == "abbrev")
      return MAKE_FUNCTOR(report_t, abbrev);
    break;

  case 'f':
    if (name == "ftime")
      return MAKE_FUNCTOR(report_t, ftime);
    break;

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
