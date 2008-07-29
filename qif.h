#ifndef _QIF_H
#define _QIF_H

#include "journal.h"

namespace ledger {

class qif_parser_t : public journal_t::parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream& in,
			     session_t&     session,
			     journal_t&   journal,
			     account_t *   master        = NULL,
			     const path *  original_file = NULL);
};

} // namespace ledger

#endif // _QIF_H
