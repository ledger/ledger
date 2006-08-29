#ifndef _OPTION_H
#define _OPTION_H

#include <list>
#include <map>
#include <string>
#include <exception>

#include "error.h"

struct option_handler_t
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

  option_handler_t(const std::string& _long_opt,
		   const char	      _short_opt,
		   const bool	      _wants_arg = false)
    : long_opt(_long_opt), short_opt(_short_opt),
      wants_arg(_wants_arg) {}

  option_handler_t(const std::string& _long_opt,
		   const bool	      _wants_arg = false)
    : long_opt(_long_opt), short_opt('\0'),
      wants_arg(_wants_arg) {}

  virtual ~option_handler_t() {}

  virtual bool check(option_source_t source);
  virtual void run(const char * arg = NULL) = 0;
};

struct static_option_t {
  const char * long_opt;
  const char   short_opt;

  option_handler_t * handler;
};

class option_error : public error {
 public:
  option_error(const std::string& reason) throw() : error(reason) {}
  virtual ~option_error() throw() {}
};

bool process_option(static_option_t * options,
		    option_handler_t::option_source_t source,
		    const std::string& opt, const char * arg = NULL);
void process_arguments(static_option_t * options, int argc, char ** argv,
		       const bool anywhere, std::list<std::string>& args);
void process_environment(static_option_t * options, const char ** envp,
			 const std::string& tag);

namespace ledger {

class config_t;
class report_t;

extern config_t * config;
extern report_t * report;

#define OPTIONS_SIZE 99
extern static_option_t options[OPTIONS_SIZE];

void help(std::ostream& out);


#define DEF_OPT(tag, option)					\
  struct option_ ## tag : public option_handler_t {		\
    option_ ## tag() : option_handler_t(option) {}

#define END_DEF()						\
  };

#define DEFR_OPT(tag, option)					\
  struct option_ ## tag : public option_handler_t {		\
    option_ ## tag() : option_handler_t(option) {}		\
								\
  virtual void run(const char * optarg) {

#define END_DEFR()						\
    }								\
  };

#define DEF_OPT_(tag, option)					\
  struct option_ ## tag : public option_handler_t {		\
    option_ ## tag() : option_handler_t(option, true) {}

#define DEFR_OPT_(tag, option)					\
  struct option_ ## tag : public option_handler_t {		\
    option_ ## tag() : option_handler_t(option, true) {}	\
								\
  virtual void run(const char * optarg) {


#define OPT_BEGIN(tag, chars)					\
  struct option_ ## tag : public option_handler_t {		\
    option_ ## tag(const std::string& long_opt,			\
		   const char	      short_opt,		\
		   const bool	      wants_arg = false)	\
      : option_handler_t(long_opt, short_opt, wants_arg) {}	\
								\
    option_ ## tag(const std::string& long_opt,			\
		   const bool	      wants_arg = false)	\
      : option_handler_t(long_opt, wants_arg) {}		\
								\
    virtual void run(const char * optarg)

#define OPT_END(tag)						\
    }

} // namespace ledger

#endif // _OPTION_H
