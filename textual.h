#ifndef _TEXTUAL_H
#define _TEXTUAL_H

#include "parser.h"
#include "walk.h"

namespace ledger {

class textual_parser_t : public parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL);
};

transaction_t * parse_transaction_text(char * line, account_t * account);
transaction_t * parse_transaction(std::istream& in, account_t * account);

void write_textual_journal(journal_t& journal, std::string path,
			   item_handler<transaction_t>& formatter,
			   std::ostream& out);

} // namespace ledger

#endif // _TEXTUAL_H
