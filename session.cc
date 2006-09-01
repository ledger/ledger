#include "session.h"

namespace ledger {

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

void export_session()
{
  class_< session_t > ("Session")
    .def_readwrite("init_file", &session_t::init_file)
    .def_readwrite("data_file", &session_t::data_file)
    .def_readwrite("cache_file", &session_t::cache_file)
    .def_readwrite("price_db", &session_t::price_db)

    .def_readwrite("balance_format", &session_t::balance_format)
    .def_readwrite("register_format", &session_t::register_format)
    .def_readwrite("wide_register_format", &session_t::wide_register_format)
    .def_readwrite("plot_amount_format", &session_t::plot_amount_format)
    .def_readwrite("plot_total_format", &session_t::plot_total_format)
    .def_readwrite("print_format", &session_t::print_format)
    .def_readwrite("write_hdr_format", &session_t::write_hdr_format)
    .def_readwrite("write_xact_format", &session_t::write_xact_format)
    .def_readwrite("equity_format", &session_t::equity_format)
    .def_readwrite("prices_format", &session_t::prices_format)
    .def_readwrite("pricesdb_format", &session_t::pricesdb_format)

    .def_readwrite("date_input_format", &session_t::date_input_format)

    .def_readwrite("pricing_leeway", &session_t::pricing_leeway)

    .def_readwrite("download_quotes", &session_t::download_quotes)
    .def_readwrite("use_cache", &session_t::use_cache)
    .def_readwrite("cache_dirty", &session_t::cache_dirty)
    .def_readwrite("debug_mode", &session_t::debug_mode)
    .def_readwrite("verbose_mode", &session_t::verbose_mode)
    .def_readwrite("trace_mode", &session_t::trace_mode)

#if 0
    .def_readwrite("journals", &session_t::journals)
#endif
    ;
}

#endif // USE_BOOST_PYTHON
