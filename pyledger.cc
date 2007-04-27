#include "py_eval.h"
#include "session.h"

using namespace boost::python;

BOOST_PYTHON_MODULE(ledger)
{
  ledger::initialize();
  ledger::initialize_for_python();
}
