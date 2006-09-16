#ifndef _SESSION_H
#define _SESSION_H

#include "journal.h"
#include "parser.h"

#include <list>

namespace ledger {

class session_t : public valexpr_t::scope_t
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

  datetime_t now;

  std::list<journal_t *> journals;
  std::list<parser_t *>  parsers;

  session_t(valexpr_t::scope_t * parent = NULL) :
    valexpr_t::scope_t(parent),

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
    trace_mode(false),

    now(datetime_t::now) {}

  virtual ~session_t() {
    for (std::list<journal_t *>::iterator i = journals.begin();
	 i != journals.end();
	 i++)
      delete *i;

    for (std::list<parser_t *>::iterator i = parsers.begin();
	 i != parsers.end();
	 i++)
      delete *i;
  }

  journal_t * new_journal() {
    journal_t * journal = new journal_t(this);
    journals.push_back(journal);
    return journal;
  }
  void close_journal(journal_t * journal) {
    journals.remove(journal);
    delete journal;
  }

  unsigned int read_journal(std::istream&       in,
			    journal_t *         journal,
			    account_t *		master        = NULL,
			    const std::string * original_file = NULL);

  unsigned int read_journal(const std::string&  path,
			    journal_t *         journal,
			    account_t *		master        = NULL,
			    const std::string * original_file = NULL);

  void read_init();

  journal_t * read_data(const std::string& master_account = "");

  void register_parser(parser_t * parser) {
    parsers.push_back(parser);
  }
  bool unregister_parser(parser_t * parser) {
    std::list<parser_t *>::iterator i;
    for (i = parsers.begin(); i != parsers.end(); i++)
      if (*i == parser)
	break;
    if (i == parsers.end())
      return false;

    parsers.erase(i);

    return true;
  }

  //
  // Scope members
  //

  virtual valexpr_t::node_t * lookup(const std::string& name);

  //
  // Option handlers
  //

  void option_file(value_t&, valexpr_t::scope_t * locals) {
    data_file = locals->args[0].get_string();
  }

#ifdef USE_BOOST_PYTHON
  void option_import(value_t&) {
    python_import(optarg);
  }
  void option_import_stdin(value_t&) {
    python_eval(std::cin, PY_EVAL_MULTI);
  }
#endif
};

} // namespace ledger

#endif // _SESSION_H
