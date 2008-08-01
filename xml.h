#ifndef _XML_H
#define _XML_H

#include "journal.h"
#include "report.h"

namespace ledger {

#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

class xml_parser_t : public journal_t::parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream& in,
			     session_t&     session,
			     journal_t&   journal,
			     account_t *   master        = NULL,
			     const path *  original_file = NULL);
};

#endif

class format_xml_entries : public format_entries
{
  bool show_totals;

  format_xml_entries();

public:
  format_xml_entries(std::ostream& output_stream,
		     const bool _show_totals = false)
    : format_entries(output_stream, ""), show_totals(_show_totals) {
    TRACE_CTOR(format_xml_entries, "std::ostream&, const bool");
    output_stream << "<?xml version=\"1.0\"?>\n"
		  << "<ledger version=\"2.5\">\n";
  }
  virtual ~format_xml_entries() throw() {
    TRACE_DTOR(format_xml_entries);
  }

  virtual void flush() {
    format_entries::flush();
    output_stream << "</ledger>" << std::endl;
  }

  virtual void format_last_entry();
};

} // namespace ledger

#endif // _XML_H
