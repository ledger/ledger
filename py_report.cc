using namespace boost::python;
using namespace ledger;

void export_report()
{
  class_< report_t > ("Report")
    .add_property("session",
		  make_getter(&report_t::session,
			      return_value_policy<reference_existing_object>()))

    .def("apply_transforms", &report_t::apply_transforms)
    ;
}
