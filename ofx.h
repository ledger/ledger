#ifndef _OFX_H
#define _OFX_H

#include "parser.h"

namespace ledger {

class ofx_parser_t : public parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream&	 in,
			     config_t&           config,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL);
};

} // namespace ledger

#endif // _OFX_H
