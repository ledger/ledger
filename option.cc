#include "option.h"
#include "debug.h"

#include <iostream>
#include <cstdarg>

#include "util.h"

void add_option_handler(std::list<option_t>& options,
			const std::string&   label,
			const std::string&   opt_chars,
			option_handler&	     option)
{
  option_t opt;

  char   buf[128];
  char * p = buf;
  for (const char * q = label.c_str();
       *q && p - buf < 128;
       q++)
    if (*q == '_')
      *p++ = '-';
    else
      *p++ = *q;
  *p = '\0';
  opt.long_opt = buf;

  if (! opt_chars.empty()) {
    if (opt_chars[0] != ':')
      opt.short_opt = opt_chars[0];

    if (opt_chars[opt_chars.length() - 1] == ':')
      opt.wants_arg = true;
  }

  opt.handler = &option;

  options.push_front(opt);
}

namespace {
  inline void process_option(const option_t& opt,
			     const char * arg = NULL) {
    if (! opt.handler->handled) {
      (*opt.handler)(arg);
      opt.handler->handled = true;
    }
  }
}

bool process_option(std::list<option_t>& options,
		    const std::string& opt, const char * arg)
{
  for (std::list<option_t>::iterator i = options.begin();
       i != options.end();
       i++)
    if ((*i).long_opt == opt) {
      if (! (*i).handler->handled) {
	(*(*i).handler)(arg);
	(*i).handler->handled = true;
	return true;
      }
      break;
    }

  return false;
}

void process_arguments(std::list<option_t>& options,
		       int argc, char ** argv, const bool anywhere,
		       std::list<std::string>& args)
{
  int index = 0;
  for (char ** i = argv; index < argc; i++, index++) {
    if ((*i)[0] != '-') {
      if (anywhere) {
	args.push_back(*i);
	continue;
      } else {
	for (; index < argc; i++, index++)
	  args.push_back(*i);
	break;
      }
    }

    // --long-option
   again:
    if ((*i)[1] == '-') {
      if ((*i)[2] == '\0')
	break;

      for (std::list<option_t>::iterator j = options.begin();
	   j != options.end();
	   j++)
	if ((*j).wants_arg) {
	  if (const char * p = std::strchr(*i + 2, '=')) {
	    if ((*j).long_opt == std::string(*i + 2, p - (*i + 2))) {
	      process_option(*j, p + 1);
	      goto next;
	    }
	  }
	  else if ((*j).long_opt == *i + 2) {
	    if (++index >= argc)
	      throw option_error(std::string("missing option argument for ") +
				 *i);
	    process_option(*j, argv[index]);
	    i++;
	    goto next;
	  }
	}
	else if ((*j).long_opt == *i + 2) {
	  process_option(*j);
	  goto next;
	}

      throw option_error(std::string("illegal option ") + *i);
    } else {
      for (std::list<option_t>::iterator j = options.begin();
	   j != options.end();
	   j++)
	if ((*i)[1] == (*j).short_opt) {
	  if ((*j).wants_arg) {
	    if (++index >= argc)
	      throw option_error(std::string("missing argument for option ") +
				 *i);
	    process_option(*j, argv[index]);
	    i++;
	    goto next;
	  } else {
	    process_option(*j);
	    if ((*i)[2]) {
	      std::strcpy(*i + 1, *i + 2);
	      goto again;
	    }
	    goto next;
	  }
	}

      throw option_error(std::string("illegal option -- ") + (*i)[1]);
    }

   next:
    ;
  }
}

void process_environment(std::list<option_t>& options,
			 char ** envp, const std::string& tag)
{
  const char * tag_p   = tag.c_str();
  int	       tag_len = tag.length();

  for (char ** p = envp; *p; p++)
    if (std::strncmp(*p, tag_p, tag_len) == 0) {
      char   buf[128];
      char * r = buf;
      char * q;
      for (q = *p + tag_len;
	   *q && *q != '=' && r - buf < 128;
	   q++)
	if (*q == '_')
	  *r++ = '-';
	else
	  *r++ = std::tolower(*q);
      *r = '\0';

      if (*q == '=')
	process_option(options, buf, q + 1);
    }
}

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <boost/python/detail/api_placeholder.hpp>
#include <vector>

using namespace boost::python;

struct func_option_wrapper : public option_handler
{
  object self;
  func_option_wrapper(object _self) : self(_self) {}

  virtual void operator()(const char * arg) {
    call<void>(self.ptr(), arg);
  }
};

namespace {
  std::list<func_option_wrapper> wrappers;
  std::list<option_t>            options;
}

void py_add_option_handler(const std::string& long_opt,
			   const std::string& short_opt, object func)
{
  wrappers.push_back(func_option_wrapper(func));
  add_option_handler(options, long_opt, short_opt, wrappers.back());
}

void add_other_option_handlers(const std::list<option_t>& other)
{
  options.insert(options.begin(), other.begin(), other.end());
}

bool py_process_option(const std::string& opt, const char * arg)
{
  return process_option(options, opt, arg);
}

list py_process_arguments(list args, bool anywhere = false)
{
  std::vector<char *> strs;

  int l = len(args);
  for (int i = 0; i < l; i++)
    strs.push_back(extract<char *>(args[i]));

  std::list<std::string> newargs;
  process_arguments(options, strs.size(), &strs.front(), anywhere, newargs);

  list py_newargs;
  for (std::list<std::string>::iterator i = newargs.begin();
       i != newargs.end();
       i++)
    py_newargs.append(*i);
  return py_newargs;
}

BOOST_PYTHON_FUNCTION_OVERLOADS(py_proc_args_overloads,
				py_process_arguments, 1, 2)

void py_process_environment(object env, const std::string& tag)
{
  std::vector<char *>      strs;
  std::vector<std::string> storage;

  list items = call_method<list>(env.ptr(), "items");
  int l = len(items);
  for (int i = 0; i < l; i++) {
    tuple pair = extract<tuple>(items[i]);
    std::string s = extract<std::string>(pair[0]);
    s += "=";
    s += extract<std::string>(pair[1]);
    storage.push_back(s);
    strs.push_back(const_cast<char *>(storage.back().c_str()));
  }

  process_environment(options, &strs.front(), tag);
}

void export_option()
{
  def("add_option_handler",  py_add_option_handler);
  def("process_option",      py_process_option);
  def("process_arguments",   py_process_arguments, py_proc_args_overloads());
  def("process_environment", py_process_environment);
}

#endif // USE_BOOST_PYTHON
