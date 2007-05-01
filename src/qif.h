#ifndef _QIF_H
#define _QIF_H

#include "parser.h"

namespace ledger {

class qif_parser_t : public parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream&	   in,
			     journal_t *	   journal,
			     account_t *	   master   = NULL,
			     const optional<path>& original = optional<path>());
};

} // namespace ledger

#endif // _QIF_H
