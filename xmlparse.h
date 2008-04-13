#ifndef _XMLPARSE_H
#define _XMLPARSE_H

#include "parser.h"

namespace ledger {

#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

class xml_parser_t : public parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL);
};

#endif

} // namespace ledger

#endif // _XMLPARSE_H
