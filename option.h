#ifndef _OPTION_H
#define _OPTION_H

#include <list>
#include <map>
#include <string>
#include <exception>

#include "error.h"

namespace ledger {

class report_t;

struct option_t
{
  const std::string long_opt;
  const char	    short_opt;
  const bool	    wants_arg;

  enum option_source_t {
    INIT_FILE    = 0x1,
    ENVIRONMENT  = 0x2,
    DATA_FILE    = 0x4,
    COMMAND_LINE = 0x8
  };

  unsigned short handled;

  option_t(const std::string& _long_opt,
	   const char	      _short_opt,
	   const bool	      _wants_arg = false)
    : long_opt(_long_opt), short_opt(_short_opt),
      wants_arg(_wants_arg) {}

  option_t(const std::string& _long_opt,
	   const bool	      _wants_arg = false)
    : long_opt(_long_opt), short_opt('\0'),
      wants_arg(_wants_arg) {}

  virtual ~option_t() {}

  virtual bool check(option_source_t source) {
    if (! handled) {
      handled |= (unsigned short)source;
      return true;
    }
    return false;
  }

  virtual void select(report_t * report, const char * arg = NULL) = 0;
};

struct static_option_t {
  const char * long_opt;
  const char   short_opt;

  option_t * handler;
};

class option_error : public error {
 public:
  option_error(const std::string& reason) throw() : error(reason) {}
  virtual ~option_error() throw() {}
};

void process_option(option_t * opt, option_t::option_source_t source,
		    report_t * report, const char * arg = NULL);

option_t * search_options(static_option_t * array, const char * name);

inline bool process_option(static_option_t * static_options,
			   option_t::option_source_t source,
			   const std::string& name, report_t * report,
			   const char * arg = NULL)
{
  if (option_t * opt = search_options(static_options, name.c_str())) {
    process_option(opt, source, report, arg);
    return true;
  }
  return false;
}

void process_environment(static_option_t * static_options,
			 const char ** envp, const std::string& tag,
			 report_t * report);

void process_arguments(static_option_t * static_options,
		       int argc, char ** argv, const bool anywhere,
		       report_t * report, std::list<std::string>& args);

} // namespace ledger

#endif // _OPTION_H
