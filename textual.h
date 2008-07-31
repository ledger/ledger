#ifndef _TEXTUAL_H
#define _TEXTUAL_H

#include "journal.h"
#include "format.h"
#include "walk.h"

namespace ledger {

class textual_parser_t : public journal_t::parser_t
{
public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream& in,
			     session_t&    session,
			     journal_t&    journal,
			     account_t *   master        = NULL,
			     const path *  original_file = NULL);
};

xact_t * parse_xact_text(char * line, account_t * account);
xact_t * parse_xact(std::istream& in, account_t * account);

void write_textual_journal(journal_t&	     journal,
			   const path&	     pathname,
			   xact_handler_ptr& formatter,
			   const string&     write_hdr_format,
			   std::ostream&     out);

#if 0
class include_context : public file_context
{
 public:
  include_context(const path& file, unsigned long line,
		  const string& desc = "") throw()
    : file_context(file, line, desc) {}
  virtual ~include_context() throw() {}

  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << ": ";
    out << "\"" << file.string() << "\", line " << line << ":"
	<< std::endl;
  }
};
#endif

} // namespace ledger

#endif // _TEXTUAL_H
