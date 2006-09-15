#include "session.h"
#include "repitem.h"

#include <fstream>

namespace ledger {

unsigned int session_t::read_journal(std::istream&       in,
				     journal_t *         journal,
				     account_t *	 master,
				     const std::string * original_file)
{
  if (! master)
    master = journal->master;

  for (std::list<parser_t *>::iterator i = parsers.begin();
       i != parsers.end();
       i++)
    if ((*i)->test(in))
      return (*i)->parse(in, journal, master, original_file);

  return 0;
}

unsigned int session_t::read_journal(const std::string&  path,
				     journal_t *         journal,
				     account_t *	 master,
				     const std::string * original_file)
{
  journal->sources.push_back(path);

  if (access(path.c_str(), R_OK) == -1)
    throw new error(std::string("Cannot read file '") + path + "'");

  if (! original_file)
    original_file = &path;

  std::ifstream stream(path.c_str());
  return read_journal(stream, journal, master, original_file);
}

valexpr_t::node_t * session_t::lookup(const std::string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'n':
    switch (*++p) {
    case 'o':
      return MAKE_FUNCTOR(session_t, now);
    }
    break;
  }
  return parent ? parent->lookup(name) : NULL;
}

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
