#ifndef _OPTION_H
#define _OPTION_H

#include <list>
#include <map>
#include <string>
#include <exception>

#include "valexpr.h"
#include "error.h"

namespace ledger {

bool process_option(const std::string& name, valexpr_t::scope_t * scope,
		    const char * arg = NULL);

void process_environment(const char ** envp, const std::string& tag,
			 valexpr_t::scope_t * scope);

void process_arguments(int argc, char ** argv, const bool anywhere,
		       valexpr_t::scope_t * scope,
		       std::list<std::string>& args);

class option_error : public error {
 public:
  option_error(const std::string& reason) throw() : error(reason) {}
  virtual ~option_error() throw() {}
};

} // namespace ledger

#endif // _OPTION_H
