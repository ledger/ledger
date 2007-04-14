#include "option.h"
#include "report.h"
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

namespace {
  xml::xpath_t::functor_t * find_option(xml::xpath_t::scope_t * scope,
				     const std::string& name)
  {
    char buf[128];
    std::strcpy(buf, "option_");
    char * p = &buf[7];
    for (const char * q = name.c_str(); *q; q++) {
      if (*q == '-')
	*p++ = '_';
      else
	*p++ = *q;
    }
    *p = '\0';

    if (xml::xpath_t::op_t * def = scope->lookup(buf))
      return def->functor_obj();
    else
      return NULL;
  }

  xml::xpath_t::functor_t * find_option(xml::xpath_t::scope_t * scope,
				     const char letter)
  {
    char buf[9];
    std::strcpy(buf, "option_");
    buf[7] = letter;
    buf[8] = '\0';

    if (xml::xpath_t::op_t * def = scope->lookup(buf))
      return def->functor_obj();
    else
      return NULL;
  }

  void process_option(xml::xpath_t::functor_t * opt, xml::xpath_t::scope_t * scope,
		      const char * arg)
  {
    try {
      xml::xpath_t::scope_t * args = NULL;
      if (arg) {
	args = new xml::xpath_t::scope_t(scope, xml::xpath_t::scope_t::ARGUMENT);
	args->args.set_string(arg);
      }

      value_t temp;
      (*opt)(temp, args);
    }
    catch (error * err) {
#if 0
      err->context.push_back
	(new error_context
	 (std::string("While parsing option '--") + opt->long_opt +
	  "'" + (opt->short_opt != '\0' ?
		 (std::string(" (-") + opt->short_opt + "):") : ":")));
#endif
      throw err;
    }
  }
}

bool process_option(const std::string& name, xml::xpath_t::scope_t * scope,
		    const char * arg)
{
  if (xml::xpath_t::functor_t * opt = find_option(scope, name)) {
    process_option(opt, scope, arg);
    return true;
  }
  return false;
}

void process_environment(const char ** envp, const std::string& tag,
			 xml::xpath_t::scope_t * scope)
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
	  if (! process_option(buf, scope, q + 1))
	    /*throw new option_error("unknown option")*/;
	}
	catch (error * err) {
	  err->context.push_back
	    (new error_context
	     (std::string("While parsing environment variable option '") +
	      *p + "':"));
	  throw err;
	}
      }
    }
}

void process_arguments(int argc, char ** argv, const bool anywhere,
		       xml::xpath_t::scope_t * scope,
		       std::list<std::string>& args)
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

      xml::xpath_t::functor_t * opt = find_option(scope, name);
      if (! opt)
	throw new option_error(std::string("illegal option --") + name);

      if (opt->wants_args && value == NULL) {
	value = *++i;
	if (value == NULL)
	  throw new option_error(std::string("missing option argument for --") +
				 name);
      }
      process_option(opt, scope, value);
    }
    else if ((*i)[1] == '\0') {
      throw new option_error(std::string("illegal option -"));
    }
    else {
      std::list<xml::xpath_t::functor_t *> option_queue;

      int x = 1;
      for (char c = (*i)[x]; c != '\0'; x++, c = (*i)[x]) {
	xml::xpath_t::functor_t * opt = find_option(scope, c);
	if (! opt)
	  throw new option_error(std::string("illegal option -") + c);
	option_queue.push_back(opt);
      }

      for (std::list<xml::xpath_t::functor_t *>::iterator
	     o = option_queue.begin();
	   o != option_queue.end();
	   o++) {
	char * value = NULL;
	if ((*o)->wants_args) {
	  value = *++i;
	  if (value == NULL)
	    throw new option_error(std::string("missing option argument for -") +
#if 0
				   (*o)->short_opt);
#else
	  '?');
#endif
	}
	process_option(*o, scope, value);
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
