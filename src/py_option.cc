using namespace boost::python;
using namespace ledger;

struct py_option_t : public option_t
{
  PyObject * self;

  py_option_t(PyObject * self_,
		      const string& long_opt,
		      const bool wants_arg)
    : self(self_), option_t(long_opt, wants_arg) {}

  virtual ~py_option_t() {}

  virtual bool check(option_source_t source) {
    return call_method<bool>(self, "check", source);
  }

  virtual void select(report_t * report, const char * optarg = NULL) {
    if (optarg)
      return call_method<void>(self, "select", report, optarg);
    else
      return call_method<void>(self, "select", report);
  }
};

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(option_select_overloads,
				       py_option_t::select, 1, 2)

typedef std::map<const string, object>  options_map;
typedef std::pair<const string, object> options_pair;

options_map options;

static option_t * find_option(const string& name)
{
  options_map::const_iterator i = options.find(name);
  if (i != options.end())
    return extract<py_option_t *>((*i).second.ptr());

  return NULL;
}

void shutdown_option()
{
  options.clear();
}

void export_option()
{
  class_< option_t, py_option_t, boost::noncopyable >
    ("Option", init<const string&, bool>())
    .def_readonly("long_opt", &py_option_t::long_opt)
    .def_readonly("short_opt", &py_option_t::short_opt)
    .def_readonly("wants_arg", &py_option_t::wants_arg)
    .def_readwrite("handled", &py_option_t::handled)
    .def("check", &py_option_t::check)
    .def("select", &py_option_t::select, option_select_overloads())
    ;

  enum_< option_t::option_source_t > ("OptionSource")
    .value("InitFile",    option_t::INIT_FILE)
    .value("Environment", option_t::ENVIRONMENT)
    .value("DataFile",    option_t::DATA_FILE)
    .value("CommandLine", option_t::COMMAND_LINE)
    ;

  class_< options_map > ("OptionsMap")
    .def(map_indexing_suite<options_map>())
    ;

  scope().attr("options") = ptr(&options);
}
