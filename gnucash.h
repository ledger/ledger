#ifndef _GNUCASH_H
#define _GNUCASH_H

#include "journal.h"

namespace ledger {

class gnucash_parser_t : public journal_t::parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream& in,
			     session_t&     session,
			     journal_t&   journal,
			     account_t *   master	 = NULL,
			     const path *  original_file = NULL);
};

} // namespace ledger

#endif // _GNUCASH_H
