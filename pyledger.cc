#include <boost/python.hpp>

using namespace boost::python;

void initialize_ledger_for_python();

BOOST_PYTHON_MODULE(ledger)
{
  initialize_ledger_for_python();
}
