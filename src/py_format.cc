using namespace boost::python;
using namespace ledger;

void export_format()
{
  class_< format_t > ("Format")
    .def(init<string>())
    .def("parse", &format_t::parse)
    .def("format", &format_t::format)
    ;
}
