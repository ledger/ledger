#ifndef _SESSION_H
#define _SESSION_H

#include "journal.h"

#include <list>

namespace ledger {

class session_t
{
 public:
  std::string init_file;
  std::string data_file;
  std::string cache_file;
  std::string price_db;

  std::string balance_format;
  std::string register_format;
  std::string wide_register_format;
  std::string plot_amount_format;
  std::string plot_total_format;
  std::string print_format;
  std::string write_hdr_format;
  std::string write_xact_format;
  std::string equity_format;
  std::string prices_format;
  std::string pricesdb_format;

  std::string date_input_format;

  unsigned long pricing_leeway;

  bool download_quotes;
  bool use_cache;
  bool cache_dirty;
  bool debug_mode;
  bool verbose_mode;
  bool trace_mode;

  std::list<journal_t *> journals;

  session_t() :
    balance_format("%20T  %2_%-a\n"),
    register_format("%D %-.20P %-.22A %12.67t %!12.80T\n%/"
		    "%32|%-.22A %12.67t %!12.80T\n"),
    wide_register_format("%D  %-.35P %-.38A %22.108t %!22.132T\n%/"
			 "%48|%-.38A %22.108t %!22.132T\n"),
    plot_amount_format("%D %(@S(@t))\n"),
    plot_total_format("%D %(@S(@T))\n"),
    print_format("\n%d %Y%C%P\n    %-34W  %12o%n\n%/    %-34W  %12o%n\n"),
    write_hdr_format("%d %Y%C%P\n"),
    write_xact_format("    %-34W  %12o%n\n"),
    equity_format("\n%D %Y%C%P\n%/    %-34W  %12t\n"),
    prices_format("%[%Y/%m/%d %H:%M:%S %Z]   %-10A %12t %12T\n"),
    pricesdb_format("P %[%Y/%m/%d %H:%M:%S] %A %t\n"),

    pricing_leeway(24 * 3600),

    download_quotes(false),
    use_cache(false),
    cache_dirty(false),
    debug_mode(false),
    verbose_mode(false),
    trace_mode(false) {}

  ~session_t() {
    for (std::list<journal_t *>::iterator i = journals.begin();
	 i != journals.end();
	 i++)
      delete *i;
  }
};

} // namespace ledger

#endif // _SESSION_H
