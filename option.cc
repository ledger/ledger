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

      if (*q == '=')
	process_option(options, buf, q + 1);
    }
}
