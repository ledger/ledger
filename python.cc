#include <boost/python.hpp>

using namespace boost::python;

#include "ledger.h"
#include "acconf.h"

static struct cleanup_ledger_t {
  ~cleanup_ledger_t() {
    ledger::shutdown();
  }
} _cleanup_ledger;

void export_amount();
void export_balance();
void export_value();
void export_journal();
void export_parser();
void export_textual();
void export_binary();
void export_qif();
#ifdef READ_GNUCASH
void export_gnucash();
#endif
void export_option();

BOOST_PYTHON_MODULE(ledger) {
  export_amount();
  export_balance();
  export_value();
  export_journal();
  export_parser();
  export_textual();
  export_binary();
  export_qif();
#ifdef READ_GNUCASH
  export_gnucash();
#endif
  export_option();
  ledger::initialize();
}
