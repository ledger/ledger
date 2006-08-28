#include "config.h"
#include "acconf.h"
#include "option.h"
#include "datetime.h"
#include "quotes.h"
#include "valexpr.h"
#include "walk.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif

#include <fstream>
#include <cstdlib>
#ifdef WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

#ifdef HAVE_REALPATH
extern "C" char *realpath(const char *, char resolved_path[]);
#endif

#if defined(HAVE_GETPWUID) || defined(HAVE_GETPWNAM)
#include <pwd.h>
#endif

namespace ledger {

std::string expand_path(const std::string& path)
{
  if (path.length() == 0 || path[0] != '~')
    return path;

  const char * pfx = NULL;
  std::string::size_type pos = path.find_first_of('/');

  if (path.length() == 1 || pos == 1) {
    pfx = std::getenv("HOME");
#ifdef HAVE_GETPWUID
    if (! pfx) {
      // Punt. We're trying to expand ~/, but HOME isn't set
      struct passwd * pw = getpwuid(getuid());
      if (pw)
	pfx = pw->pw_dir;
    }
#endif
  }
#ifdef HAVE_GETPWNAM
  else {
    std::string user(path, 1, pos == std::string::npos ?
		     std::string::npos : pos - 1);
    struct passwd * pw = getpwnam(user.c_str());
    if (pw)
      pfx = pw->pw_dir;
  }
#endif

  // if we failed to find an expansion, return the path unchanged.

  if (! pfx)
    return path;

  std::string result(pfx);

  if (pos == std::string::npos)
    return result;

  if (result.length() == 0 || result[result.length() - 1] != '/')
    result += '/';

  result += path.substr(pos + 1);

  return result;
}

std::string resolve_path(const std::string& path)
{
  if (path[0] == '~')
    return expand_path(path);
  return path;
}

config_t::config_t()
{
  balance_format       = "%20T  %2_%-a\n";
  register_format      = ("%D %-.20P %-.22A %12.67t %!12.80T\n%/"
			  "%32|%-.22A %12.67t %!12.80T\n");
  wide_register_format = ("%D  %-.35P %-.38A %22.108t %!22.132T\n%/"
			  "%48|%-.38A %22.108t %!22.132T\n");
  plot_amount_format   = "%D %(@S(@t))\n";
  plot_total_format    = "%D %(@S(@T))\n";
  print_format	       = "\n%d %Y%C%P\n    %-34W  %12o%n\n%/    %-34W  %12o%n\n";
  write_hdr_format     = "%d %Y%C%P\n";
  write_xact_format    = "    %-34W  %12o%n\n";
  equity_format	       = "\n%D %Y%C%P\n%/    %-34W  %12t\n";
  prices_format	       = "%[%Y/%m/%d %H:%M:%S %Z]   %-10A %12t %12T\n";
  pricesdb_format      = "P %[%Y/%m/%d %H:%M:%S] %A %t\n";

  pricing_leeway       = 24 * 3600;

  download_quotes      = false;
  use_cache	       = false;
  cache_dirty	       = false;
  debug_mode	       = false;
  verbose_mode	       = false;
  trace_mode	       = false;
}

//////////////////////////////////////////////////////////////////////

void trace(const std::string& cat, const std::string& str)
{
  char buf[32];
  std::strftime(buf, 31, "%H:%M:%S", datetime_t::now.localtime());
  std::cerr << buf << " " << cat << ": " << str << std::endl;
}

void trace_push(const std::string& cat, const std::string& str,
		timing_t& timer)
{
  timer.start();
  trace(cat, str);
}

void trace_pop(const std::string& cat, const std::string& str,
	       timing_t& timer)
{
  timer.stop();
  std::ostringstream out;
  out << str << ": " << (double(timer.cumulative) / double(CLOCKS_PER_SEC)) << "s";
  trace(cat, out.str());
}

} // namespace ledger

#if 0
#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <boost/python/detail/api_placeholder.hpp>

using namespace boost::python;
using namespace ledger;

void py_process_options(config_t& config, const std::string& command,
			list args)
{
  strings_list strs;

  int l = len(args);
  for (int i = 0; i < l; i++)
    strs.push_back(std::string(extract<char *>(args[i])));

  config.process_options(command, strs.begin(), strs.end());
}

void add_other_option_handlers(const std::list<option_t>& other);

void py_add_config_option_handlers()
{
  add_other_option_handlers(config_options);
}

BOOST_PYTHON_FUNCTION_OVERLOADS(parse_ledger_data_overloads,
				parse_ledger_data, 1, 2)

void py_option_help()
{
  help(std::cout);
}

void export_config()
{
  class_< config_t > ("Config")
    .def_readwrite("init_file", &config_t::init_file)
    .def_readwrite("data_file", &config_t::data_file)
    .def_readwrite("cache_file", &config_t::cache_file)
    .def_readwrite("price_db", &config_t::price_db)
    .def_readwrite("output_file", &config_t::output_file)
    .def_readwrite("account", &config_t::account)
    .def_readwrite("predicate", &config_t::predicate)
    .def_readwrite("display_predicate", &config_t::display_predicate)
    .def_readwrite("report_period", &config_t::report_period)
    .def_readwrite("report_period_sort", &config_t::report_period_sort)
    .def_readwrite("format_string", &config_t::format_string)
    .def_readwrite("balance_format", &config_t::balance_format)
    .def_readwrite("register_format", &config_t::register_format)
    .def_readwrite("wide_register_format", &config_t::wide_register_format)
    .def_readwrite("plot_amount_format", &config_t::plot_amount_format)
    .def_readwrite("plot_total_format", &config_t::plot_total_format)
    .def_readwrite("print_format", &config_t::print_format)
    .def_readwrite("write_hdr_format", &config_t::write_hdr_format)
    .def_readwrite("write_xact_format", &config_t::write_xact_format)
    .def_readwrite("equity_format", &config_t::equity_format)
    .def_readwrite("prices_format", &config_t::prices_format)
    .def_readwrite("pricesdb_format", &config_t::pricesdb_format)
    .def_readwrite("date_format", &config_t::date_format)
    .def_readwrite("sort_string", &config_t::sort_string)
    .def_readwrite("amount_expr", &config_t::amount_expr)
    .def_readwrite("total_expr", &config_t::total_expr)
    .def_readwrite("total_expr_template", &config_t::total_expr_template)
    .def_readwrite("forecast_limit", &config_t::forecast_limit)
    .def_readwrite("reconcile_balance", &config_t::reconcile_balance)
    .def_readwrite("reconcile_date", &config_t::reconcile_date)
    .def_readwrite("budget_flags", &config_t::budget_flags)
    .def_readwrite("pricing_leeway", &config_t::pricing_leeway)
    .def_readwrite("show_collapsed", &config_t::show_collapsed)
    .def_readwrite("show_subtotal", &config_t::show_subtotal)
    .def_readwrite("show_totals", &config_t::show_totals)
    .def_readwrite("show_related", &config_t::show_related)
    .def_readwrite("show_all_related", &config_t::show_all_related)
    .def_readwrite("show_inverted", &config_t::show_inverted)
    .def_readwrite("show_empty", &config_t::show_empty)
    .def_readwrite("head_entries", &config_t::head_entries)
    .def_readwrite("tail_entries", &config_t::tail_entries)
    .def_readwrite("pager", &config_t::pager)
    .def_readwrite("days_of_the_week", &config_t::days_of_the_week)
    .def_readwrite("by_payee", &config_t::by_payee)
    .def_readwrite("comm_as_payee", &config_t::comm_as_payee)
    .def_readwrite("show_revalued", &config_t::show_revalued)
    .def_readwrite("show_revalued_only", &config_t::show_revalued_only)
    .def_readwrite("download_quotes", &config_t::download_quotes)
    .def_readwrite("use_cache", &config_t::use_cache)
    .def_readwrite("cache_dirty", &config_t::cache_dirty)

    .def("process_options", py_process_options)
    ;

  scope().attr("config") = ptr(&config);

  def("option_help", py_option_help);
  def("parse_ledger_data", parse_ledger_data, parse_ledger_data_overloads());
  def("add_config_option_handlers", py_add_config_option_handlers);
}

#endif // USE_BOOST_PYTHON
#endif
