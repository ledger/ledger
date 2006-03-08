#include "option.h"
#include "config.h"
#include "debug.h"
#include "error.h"

#include <iostream>
#include <cstdarg>

#include "util.h"

namespace {
  inline void process_option(option_t * opt, const char * arg = NULL) {
    if (! opt->handled) {
      try {
	opt->handler(arg);
      }
      catch (error * err) {
	err->context.push_back
	  (new error_context
	   (std::string("While parsing option '--") + opt->long_opt +
	    "'" + (opt->short_opt != '\0' ?
		   (std::string(" (-") + opt->short_opt + "):") : ":")));
	throw err;
      }
      opt->handled = true;
    }
  }

  option_t * search_options(option_t * array, const char * name)
  {
    int first = 0;
    int last  = CONFIG_OPTIONS_SIZE;
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
	return &array[mid];
    }
    return NULL;
  }

  option_t * search_options(option_t * array, const char letter)
  {
    for (int i = 0; i < CONFIG_OPTIONS_SIZE; i++)
      if (letter == array[i].short_opt)
	return &array[i];
    return NULL;
  }
}

bool process_option(option_t * options, const std::string& name,
		    const char * arg)
{
  option_t * opt = search_options(options, name.c_str());
  if (opt) {
    process_option(opt, arg);
    return true;
  }
  return false;
}

void process_arguments(option_t * options, int argc, char ** argv,
		       const bool anywhere, std::list<std::string>& args)
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
    option_t * opt   = NULL;
    char *     value = NULL;

    if ((*i)[1] == '-') {
      if ((*i)[2] == '\0')
	break;

      char * name  = *i + 2;
      if (char * p = std::strchr(name, '=')) {
	*p++ = '\0';
	value = p;
      }

      opt = search_options(options, name);
      if (! opt)
	throw new option_error(std::string("illegal option --") + name);
      
      if (opt->wants_arg && ! value) {
	value = *++i;
	if (! value)
	  throw new option_error(std::string("missing option argument for --") +
				 name);
      }
      process_option(opt, value);
    } else {
      char c = (*i)[1];
      opt = search_options(options, c);
      if (! opt)
	throw new option_error(std::string("illegal option -") + c);

      if (opt->wants_arg) {
	value = *++i;
	if (! value)
	  throw new option_error(std::string("missing option argument for -") + c);
      }
    }

    assert(opt);
    assert(! value || opt->wants_arg);
    process_option(opt, value);

   next:
    ;
  }
}

void process_environment(option_t * options, char ** envp,
			 const std::string& tag)
{
  const char * tag_p   = tag.c_str();
  unsigned int tag_len = tag.length();

  for (char ** p = envp; *p; p++)
    if (! tag_p || std::strncmp(*p, tag_p, tag_len) == 0) {
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

      if (*q == '=') {
	try {
	  process_option(options, buf, q + 1);
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
