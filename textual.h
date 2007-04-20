#ifndef _TEXTUAL_H
#define _TEXTUAL_H

#include "parser.h"

namespace ledger {

class textual_parser_t : public parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const string * original_file = NULL);
};

#if 0
void write_textual_journal(journal_t& journal, string path,
			   item_handler<transaction_t>& formatter,
			   const string& write_hdr_format,
			   std::ostream& out);
#endif

class include_context : public file_context {
 public:
  include_context(const string& file, unsigned long line,
		  const string& desc = "") throw()
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
