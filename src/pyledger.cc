#include <pyledger.h>

using namespace boost::python;

namespace ledger {

void export_amount();
#if 0
void export_balance();
void export_value();

void export_journal();
void export_parser();
void export_option();
void export_walk();
void export_report();
void export_format();
void export_valexpr();

void shutdown_option();
#endif

void initialize_for_python()
{
  export_amount();
#if 0
  export_balance();
  export_value();

  export_journal();
  export_parser();
  export_option();
  export_walk();
  export_format();
  export_report();
  export_valexpr();
#endif
}

void shutdown_for_python()
{
#if 0
  shutdown_option();
#endif
}

}

BOOST_PYTHON_MODULE(ledger)
{
  ledger::initialize();
  ledger::initialize_for_python();
}
