#include "option.h"
#include "report.h"
#include "config.h"
#include "debug.h"
#include "error.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif

#include <iostream>
#include <cstdarg>

#include "util.h"

#ifdef USE_BOOST_PYTHON
static ledger::option_t * find_option(const std::string& name);
#endif

namespace ledger {

void process_option(option_t * opt, option_t::option_source_t source,
		    report_t * report, const char * arg)
{
  if (opt->check(source)) {
    try {
      opt->select(report, arg);
    }
    catch (error * err) {
      err->context.push_back
	(new error_context
	 (std::string("While parsing option '--") + opt->long_opt +
	  "'" + (opt->short_opt != '\0' ?
		 (std::string(" (-") + opt->short_opt + "):") : ":")));
      throw err;
    }
  }
}

option_t * search_options(static_option_t * array, const char * name)
{
  int first = 0;
  int last  = OPTIONS_SIZE;
  while (first <= last) {
    int mid = (first + last) / 2; // compute mid point.

    int result;
    if ((result = (int)name[0] - (int)array[mid].long_opt[0]) == 0)
      result = std::strcmp(name, array[mid].long_opt);

    if (result > 0)
      first = mid + 1;		// repeat search in top half.
    else if (result < 0)
      last = mid - 1;		// repeat search in bottom half.
    else
      return array[mid].handler;
  }

#ifdef USE_BOOST_PYTHON
  return find_option(name);
#else
  return NULL;
#endif
}

inline option_t * search_options(static_option_t * array, const char letter)
{
  for (int i = 0; i < OPTIONS_SIZE; i++)
    if (letter == array[i].short_opt)
      return array[i].handler;
  return NULL;
}

void process_environment(static_option_t * static_options, const char ** envp,
			 const std::string& tag, report_t * report)
{
  const char * tag_p   = tag.c_str();
  unsigned int tag_len = tag.length();

  for (const char ** p = envp; *p; p++)
    if (! tag_p || std::strncmp(*p, tag_p, tag_len) == 0) {
      char   buf[128];
      char * r = buf;
      const char * q;
      for (q = *p + tag_len;
	   *q && *q != '=' && r - buf < 128;
	   q++)
	if (*q == '_')
	  *r++ = '-';
	else
	  *r++ = std::tolower(*q);
      *r = '\0';

      if (*q == '=') {
	try {
	  process_option(static_options, option_t::ENVIRONMENT,
			 buf, report, q + 1);
	}
	catch (error * err) {
	  err->context.pop_back();
	  err->context.push_back
	    (new error_context
	     (std::string("While parsing environment variable option '") +
	      *p + "':"));
	  throw err;
	}
      }
    }
}

void process_arguments(static_option_t * static_options,
		       int argc, char ** argv, const bool anywhere,
		       report_t * report, std::list<std::string>& args)
{
  int index = 0;
  for (char ** i = argv; *i; i++) {
    if ((*i)[0] != '-') {
      if (anywhere) {
	args.push_back(*i);
	continue;
      } else {
	for (; *i; i++)
	  args.push_back(*i);
	break;
      }
    }

    // --long-option or -s
   again:
    if ((*i)[1] == '-') {
      if ((*i)[2] == '\0')
	break;

      char * name  = *i + 2;
      char * value = NULL;
      if (char * p = std::strchr(name, '=')) {
	*p++ = '\0';
	value = p;
      }

      option_t * opt = search_options(static_options, name);
      if (! opt)
	throw new option_error(std::string("illegal option --") + name);

      if (opt->wants_arg && value == NULL) {
	value = *++i;
	if (value == NULL)
	  throw new option_error(std::string("missing option argument for --") +
				 name);
      }
      process_option(opt, option_t::COMMAND_LINE, report, value);
    }
    else if ((*i)[1] == '\0') {
      throw new option_error(std::string("illegal option -"));
    }
    else {
      std::list<option_t *> opt_queue;

      int x = 1;
      for (char c = (*i)[x]; c != '\0'; x++, c = (*i)[x]) {
	option_t * opt = search_options(static_options, c);
	if (! opt)
	  throw new option_error(std::string("illegal option -") + c);
	opt_queue.push_back(opt);
      }

      for (std::list<option_t *>::iterator o = opt_queue.begin();
	   o != opt_queue.end();
	   o++) {
	char * value = NULL;
	if ((*o)->wants_arg) {
	  value = *++i;
	  if (value == NULL)
	    throw new option_error(std::string("missing option argument for -") +
				   (*o)->short_opt);
	}
	process_option(*o, option_t::COMMAND_LINE, report, value);
      }
    }

   next:
    ;
  }
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <boost/python/detail/api_placeholder.hpp>
#include <boost/python/suite/indexing/map_indexing_suite.hpp>

using namespace boost::python;
using namespace ledger;

struct py_option_t : public option_t
{
  PyObject * self;

  py_option_t(PyObject * self_,
		      const std::string& long_opt,
		      const bool wants_arg)
    : self(self_), option_t(long_opt, wants_arg) {}

  virtual ~py_option_t() {}

  virtual bool check(option_source_t source) {
    return call_method<bool>(self, "check", source);
  }

  virtual void select(report_t * report, const char * optarg = NULL) {
    if (optarg)
      return call_method<void>(self, "select", report, optarg);
    else
      return call_method<void>(self, "select", report);
  }
};

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(option_select_overloads,
				       py_option_t::select, 1, 2)

typedef std::map<const std::string, object>  options_map;
typedef std::pair<const std::string, object> options_pair;

options_map options;

static option_t * find_option(const std::string& name)
{
  options_map::const_iterator i = options.find(name);
  if (i != options.end())
    return extract<py_option_t *>((*i).second.ptr());

  return NULL;
}

void shutdown_option()
{
  options.clear();
}

void export_option()
{
  class_< option_t, py_option_t, boost::noncopyable >
    ("Option", init<const std::string&, bool>())
    .def_readonly("long_opt", &py_option_t::long_opt)
    .def_readonly("short_opt", &py_option_t::short_opt)
    .def_readonly("wants_arg", &py_option_t::wants_arg)
    .def_readwrite("handled", &py_option_t::handled)
    .def("check", &py_option_t::check)
    .def("select", &py_option_t::select, option_select_overloads())
    ;

  enum_< option_t::option_source_t > ("OptionSource")
    .value("InitFile",    option_t::INIT_FILE)
    .value("Environment", option_t::ENVIRONMENT)
    .value("DataFile",    option_t::DATA_FILE)
    .value("CommandLine", option_t::COMMAND_LINE)
    ;

  class_< options_map > ("OptionsMap")
    .def(map_indexing_suite<options_map>())
    ;

  scope().attr("options") = ptr(&options);
}

#endif // USE_BOOST_PYTHON
