#ifndef _CSV_H
#define _CSV_H

#include "journal.h"
#include "format.h"

namespace ledger {

class format_csv_transactions : public item_handler<transaction_t>
{
 protected:
  std::ostream&	out;

 public:
  format_csv_transactions(std::ostream& _out) : out(_out) {}
  virtual void flush() {
    out.flush();
  }
  virtual void operator()(transaction_t& xact);
};

} // namespace ledger

#endif // _REPORT_H
