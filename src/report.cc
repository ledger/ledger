#include "report.h"

namespace ledger {

report_t::~report_t()
{
  TRACE_DTOR(report_t);
}

void report_t::apply_transforms(xml::document_t * document)
{
  for (ptr_list<transform_t>::iterator i = transforms.begin();
       i != transforms.end();
       i++)
    i->execute(document);
}

void report_t::abbrev(value_t& result, xml::xpath_t::scope_t * locals)
{
  if (locals->args.size() < 2)
    throw_(std::logic_error, "usage: abbrev(STRING, WIDTH [, STYLE, ABBREV_LEN])");

  string str = locals->args[0].string_value();
  long	      wid = locals->args[1];

  elision_style_t style = session->elision_style;
  if (locals->args.size() == 3)
    style = (elision_style_t)locals->args[2].integer();

  long abbrev_len = session->abbrev_length;
  if (locals->args.size() == 4)
    abbrev_len = locals->args[3].integer();

  result.set_string(abbreviate(str, wid, style, true, (int)abbrev_len));
}

void report_t::ftime(value_t&, xml::xpath_t::scope_t * locals)
{
  if (locals->args.size() < 1)
    throw_(std::logic_error, "usage: ftime(DATE [, DATE_FORMAT])");

  moment_t date = locals->args[0].datetime();

  string date_format;
  if (locals->args.size() == 2)
    date_format = locals->args[1].string_value();
#if 0
  // jww (2007-04-18): Need to setup an output facet here
  else
    date_format = moment_t::output_format;

  result.set_string(date.to_string(date_format));
#endif
}

bool report_t::resolve(const string& name, value_t& result,
		       xml::xpath_t::scope_t * locals)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'a':
    if (name == "abbrev") {
      abbrev(result, locals);
      return true;
    }
    break;

  case 'f':
    if (name == "ftime") {
      ftime(result, locals);
      return true;
    }
    break;
  }

  return xml::xpath_t::scope_t::resolve(name, result, locals);
}

xml::xpath_t::op_t * report_t::lookup(const string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'o':
    if (std::strncmp(p, "option_", 7) == 0) {
      p = p + 7;
      switch (*p) {
      case 'a':
#if 0
	if (std::strcmp(p, "accounts") == 0)
	  return MAKE_FUNCTOR(report_t, option_accounts);
	else
#endif
	  if (std::strcmp(p, "amount") == 0)
	  return MAKE_FUNCTOR(report_t, option_amount);
	break;

      case 'b':
	if (std::strcmp(p, "bar") == 0)
	  return MAKE_FUNCTOR(report_t, option_bar);
	break;

#if 0
      case 'c':
	if (std::strcmp(p, "clean") == 0)
	  return MAKE_FUNCTOR(report_t, option_clean);
	else if (std::strcmp(p, "compact") == 0)
	  return MAKE_FUNCTOR(report_t, option_compact);
	break;
#endif

      case 'e':
#if 0
	if (std::strcmp(p, "entries") == 0)
	  return MAKE_FUNCTOR(report_t, option_entries);
	else if (std::strcmp(p, "eval") == 0)
	  return MAKE_FUNCTOR(report_t, option_eval);
	else if (std::strcmp(p, "exclude") == 0)
	  return MAKE_FUNCTOR(report_t, option_remove);
#endif
	break;

      case 'f':
	if (std::strcmp(p, "foo") == 0)
	  return MAKE_FUNCTOR(report_t, option_foo);
	else if (std::strcmp(p, "format") == 0)
	  return MAKE_FUNCTOR(report_t, option_format);
	break;

      case 'i':
#if 0
	if (std::strcmp(p, "include") == 0)
	  return MAKE_FUNCTOR(report_t, option_select);
#endif
	break;

      case 'l':
#if 0
	if (! *(p + 1) || std::strcmp(p, "limit") == 0)
	  return MAKE_FUNCTOR(report_t, option_limit);
#endif
	break;

#if 0
      case 'm':
	if (std::strcmp(p, "merge") == 0)
	  return MAKE_FUNCTOR(report_t, option_merge);
	break;
#endif

      case 'r':
#if 0
	if (std::strcmp(p, "remove") == 0)
	  return MAKE_FUNCTOR(report_t, option_remove);
#endif
	break;

#if 0
      case 's':
	if (std::strcmp(p, "select") == 0)
	  return MAKE_FUNCTOR(report_t, option_select);
	else if (std::strcmp(p, "split") == 0)
	  return MAKE_FUNCTOR(report_t, option_split);
	break;
#endif

      case 't':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t, option_amount);
	else if (std::strcmp(p, "total") == 0)
	  return MAKE_FUNCTOR(report_t, option_total);
	break;

      case 'T':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t, option_total);
	break;
      }
    }
    break;
  }

  return xml::xpath_t::scope_t::lookup(name);
}

} // namespace ledger
