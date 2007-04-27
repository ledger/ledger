#ifndef _OPTION_H
#define _OPTION_H

#include "xpath.h"

namespace ledger {

bool process_option(const string& name, xml::xpath_t::scope_t * scope,
		    const char * arg = NULL);

void process_environment(const char ** envp, const string& tag,
			 xml::xpath_t::scope_t * scope);

void process_arguments(int argc, char ** argv, const bool anywhere,
		       xml::xpath_t::scope_t * scope,
		       std::list<string>& args);

DECLARE_EXCEPTION(option_exception);

} // namespace ledger

#endif // _OPTION_H
