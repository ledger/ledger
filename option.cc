#include "option.h"

#include <iostream>
#include <cstdarg>

option_handler::option_handler(const std::string& label,
			       const std::string& opt_chars,
			       const bool         _multiple)
  : handled(false), multiple(_multiple)
{
  option_t opt;

  for (const char * q = label.c_str(); *q; q++)
    if (*q == '_')
      opt.long_opt += '-';
    else
      opt.long_opt += *q;

  handlers.insert(option_handler_pair(opt.long_opt, this));

  if (! opt_chars.empty()) {
    if (opt_chars[0] != ':')
      opt.short_opt = opt_chars[0];

    if (opt_chars[opt_chars.length() - 1] == ':')
      opt.wants_arg = true;
  }

  opt.handler = this;

  options.push_back(opt);
}

static inline void process_option(const option_t& opt,
				  const char * arg = NULL) {
  if (! opt.handler->handled || opt.handler->multiple) {
    opt.handler->handle_option(arg);
    opt.handler->handled = true;
  }
}

bool process_option(const std::string& opt, const char * arg)
{
  option_handler_map::iterator handler = option_handler::handlers.find(opt);
  if (handler != option_handler::handlers.end()) {
    if (! (*handler).second->handled || (*handler).second->multiple) {
      (*handler).second->handle_option(arg);
      (*handler).second->handled = true;
    }
    return true;
  }
  return false;
}

void process_arguments(int argc, char ** argv, const bool anywhere,
		       std::list<std::string>& args)
{
  int index = 1;
  for (char ** i = argv + 1; index < argc; i++, index++) {
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

      for (std::vector<option_t>::iterator j
	     = option_handler::options.begin();
	   j != option_handler::options.end();
	   j++)
	if ((*j).wants_arg) {
	  if (const char * p = std::strchr(*i + 2, '=')) {
	    if ((*j).long_opt == std::string(*i + 2, p - (*i + 2))) {
	      process_option(*j, p + 1);
	      goto next;
	    }
	  }
	  else if ((*j).long_opt == *i + 2) {
	    process_option(*j, argv[++index]);
	    goto next;
	  }
	}
	else if ((*j).long_opt == *i + 2) {
	  process_option(*j);
	  goto next;
	}

      std::cerr << "Error: illegal option " << *i << std::endl;
      std::exit(1);
    } else {
      for (std::vector<option_t>::iterator j
	     = option_handler::options.begin();
	   j != option_handler::options.end();
	   j++)
	if ((*i)[1] == (*j).short_opt) {
	  if ((*j).wants_arg) {
	    process_option(*j, argv[++index]);
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

      std::cerr << "Error: illegal option -- " << (*i)[1] << std::endl;
      std::exit(1);
    }

   next:
    ;
  }
}

void process_environment(char ** envp, const std::string& tag)
{
  for (char ** p = envp; *p; p++)
    if (std::strncmp(*p, tag.c_str(), 7) == 0) {
      std::string opt;
      char * q;
      for (q = *p + 7; *q && *q != '='; q++)
	if (*q == '_')
	  opt += '-';
	else
	  opt += std::tolower(*q);

      if (*q == '=')
	process_option(opt, q + 1);
    }
}
