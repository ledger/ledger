#ifndef _OPTION_H
#define _OPTION_H

#include "utils.h"

namespace ledger {

typedef void (*handler_t)(const char * arg);

struct option_t {
  const char * long_opt;
  char	       short_opt;
  bool	       wants_arg;
  handler_t    handler;
  bool         handled;
};

DECLARE_EXCEPTION(error, option_error);

bool process_option(option_t * options, const string& opt,
		    const char * arg = NULL);
void process_arguments(option_t * options, int argc, char ** argv,
		       const bool anywhere, std::list<string>& args);
void process_environment(option_t * options, const char ** envp,
			 const string& tag);

class config_t;
class report_t;

extern config_t * config;
extern report_t * report;

#define CONFIG_OPTIONS_SIZE 97
extern option_t config_options[CONFIG_OPTIONS_SIZE];

void option_help(std::ostream& out);

#define OPT_BEGIN(tag, chars)			\
    void opt_ ## tag(const char * optarg)

#define OPT_END(tag)

} // namespace ledger

#endif // _OPTION_H
