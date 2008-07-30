#ifndef _CSV_H
#define _CSV_H

#include "journal.h"
#include "format.h"

namespace ledger {

class format_csv_xacts : public item_handler<xact_t>
{
  format_csv_xacts();

protected:
  std::ostream&	out;

public:
  format_csv_xacts(std::ostream& _out) : out(_out) {
    TRACE_CTOR(format_csv_xacts, "std::ostream&");
  }
  ~format_csv_xacts() {
    TRACE_DTOR(format_csv_xacts);
  }

  virtual void flush() {
    out.flush();
  }
  virtual void operator()(xact_t& xact);
};

} // namespace ledger

#endif // _REPORT_H
