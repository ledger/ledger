#ifndef _TEXTUAL_H
#define _TEXTUAL_H

#include "parser.h"

namespace ledger {

bool finalize_entry(entry_t * entry);

class textual_parser_t : public parser_t
{
 public:
  typedef bool (*finalize_hook_t)(entry_t * entry);

  std::list<finalize_hook_t> finalize_hooks;

  textual_parser_t() {
    add_finalize_hook(finalize_entry);
  }

  virtual bool test(std::istream& in) const {
    return true;
  }

  void add_finalize_hook(finalize_hook_t func, bool prepend = false) {
    if (prepend)
      finalize_hooks.push_front(func);
    else
      finalize_hooks.push_back(func);
  }
  void remove_finalize_hook(finalize_hook_t func) {
    finalize_hooks.remove(func);
  }
  bool run_finalize_hooks(entry_t * entry) {
    for (std::list<finalize_hook_t>::const_iterator i
	   = finalize_hooks.begin();
	 i != finalize_hooks.end();
	 i++)
      if (! (*i)(entry))
	return false;
    return true;
  }

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL);
};

} // namespace ledger

#endif // _TEXTUAL_H
