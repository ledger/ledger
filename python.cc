#include "python.h"
#include "journal.h"
#include "acconf.h"

#include <boost/python.hpp>

using namespace boost::python;

namespace {
  bool python_initialized = false;
  bool module_initialized = false;
}

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
void export_config();
void export_walk();
void export_format();
void export_valexpr();
void export_datetime();
void export_autoxact();

void initialize_ledger_for_python()
{
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
  export_config();
  export_walk();
  export_format();
  export_valexpr();
  export_datetime();
  export_autoxact();

  module_initialized = true;
}

namespace ledger {

python_support * python_interpretor = NULL;

static struct cleanup_python {
  ~cleanup_python() {
    if (python_initialized)
      Py_Finalize();
    if (python_interpretor)
      delete python_interpretor;
  }
} _cleanup;

void init_python()
{
  if (! module_initialized) {
    Py_Initialize();
    python_initialized = true;
    detail::init_module("ledger", &initialize_ledger_for_python);
  }

  python_interpretor = new python_support;
}

} // namespace ledger
