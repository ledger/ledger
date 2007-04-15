#include <boost/python.hpp>

#include "py_eval.h"

using namespace boost::python;

BOOST_PYTHON_MODULE(ledger)
{
  ledger::initialize_ledger_for_python();
}
