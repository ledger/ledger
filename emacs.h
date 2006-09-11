#ifndef _EMACS_H
#define _EMACS_H

#include "journal.h"
#include "format.h"

namespace ledger {

#if 0
class format_emacs_transactions : public item_handler<transaction_t>
{
 protected:
  std::ostream& out;
  entry_t *     last_entry;

 public:
  format_emacs_transactions(std::ostream& _out)
    : out(_out), last_entry(NULL) {}

  virtual void write_entry(entry_t& entry);
  virtual void flush() {
    if (last_entry)
      out << "))\n";
    out.flush();
  }
  virtual void operator()(transaction_t& xact);
};
#endif

} // namespace ledger

#endif // _REPORT_H
