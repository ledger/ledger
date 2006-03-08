#ifndef _TEXTUAL_H
#define _TEXTUAL_H

#include "parser.h"
#include "format.h"
#include "walk.h"

namespace ledger {

class textual_parser_t : public parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream&	 in,
			     config_t&           config,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL);
};

transaction_t * parse_transaction_text(char * line, account_t * account);
transaction_t * parse_transaction(std::istream& in, account_t * account);

void write_textual_journal(journal_t& journal, std::string path,
			   item_handler<transaction_t>& formatter,
			   const std::string& write_hdr_format,
			   std::ostream& out);

class include_context : public file_context {
 public:
  include_context(const std::string& file, unsigned long line,
		  const std::string& desc = "") throw()
    : file_context(file, line, desc) {}
  virtual ~include_context() throw() {}

  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << ": ";
    out << "\"" << file << "\", line " << line << ":" << std::endl;
  }
};

} // namespace ledger

#endif // _TEXTUAL_H
