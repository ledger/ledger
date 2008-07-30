#ifndef _EMACS_H
#define _EMACS_H

#include "journal.h"
#include "format.h"

namespace ledger {

class format_emacs_xacts : public item_handler<xact_t>
{
  format_emacs_xacts();

protected:
  std::ostream& out;
  entry_t *     last_entry;

public:
  format_emacs_xacts(std::ostream& _out)
    : out(_out), last_entry(NULL) {
    TRACE_CTOR(format_emacs_xacts, "std::ostream&");
  }
  ~format_emacs_xacts() {
    TRACE_DTOR(format_emacs_xacts);
  }

  virtual void write_entry(entry_t& entry);
  virtual void flush() {
    if (last_entry)
      out << "))\n";
    out.flush();
  }
  virtual void operator()(xact_t& xact);
};

} // namespace ledger

#endif // _REPORT_H
