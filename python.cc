#include "python.h"
#include "ledger.h"
#include "acconf.h"

#include <boost/python.hpp>

using namespace boost::python;

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
void export_walk();
void export_format();
void export_valexpr();
void export_datetime();

namespace ledger {

python_support * python_interpretor = NULL;

#ifndef PYTHON_MODULE

static struct cleanup_python {
  ~cleanup_python() {
    if (python_interpretor) {
      Py_Finalize();
      delete python_interpretor;
    }
  }
} _cleanup;

void init_module()
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
  export_walk();
  export_format();
  export_valexpr();
  export_datetime();
}

#endif // PYTHON_MODULE

void init_python()
{
  assert(! python_interpretor);

#ifndef PYTHON_MODULE
  Py_Initialize();
  detail::init_module("ledger", &init_module);
#endif

  python_interpretor = new python_support;
}

} // namespace ledger
