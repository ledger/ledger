#ifndef _SESSION_H
#define _SESSION_H

#include "journal.h"
#include "parser.h"
#include "register.h"

namespace ledger {

class session_t : public xml::xpath_t::scope_t
{
 public:
  string init_file;
  string data_file;
  string cache_file;
  string price_db;

  string register_format;
  string wide_register_format;
  string print_format;
  string balance_format;
  string equity_format;
  string plot_amount_format;
  string plot_total_format;
  string write_hdr_format;
  string write_xact_format;
  string prices_format;
  string pricesdb_format;

  unsigned long pricing_leeway;

  bool download_quotes;
  bool use_cache;
  bool cache_dirty;

  moment_t now;

  elision_style_t elision_style;

  int abbrev_length;

  bool ansi_codes;
  bool ansi_invert;

  std::list<journal_t *> journals;
  std::list<parser_t *>  parsers;

  session_t(xml::xpath_t::scope_t * _parent = NULL) :
    xml::xpath_t::scope_t(_parent),

    register_format
    ("%((//entry)%{date} %-.20{payee}"
     "%((./xact)%32|%-22{abbrev(account, 22)} %12.67t %12.80T\n))"),
    wide_register_format
    ("%D  %-.35P %-.38A %22.108t %!22.132T\n%/"
     "%48|%-.38A %22.108t %!22.132T\n"),
    print_format
#if 1
    ("%(/%(/%{date} %-.20{payee}\n%(:    %-34{account}  %12t\n)\n))"),
#else
    ("\n%d %Y%C%P\n    %-34W  %12o%n\n%/    %-34W  %12o%n\n"),
#endif
    balance_format
    ("%(/%(//%20t  %{\"  \" * rdepth}%{rname}\n))--------------------\n%20t\n"),
    equity_format

    ("%((/)%{ftime(now, date_format)} %-.20{\"Opening Balance\"}\n%((.//account[value != 0])    %-34{fullname}  %12{value}\n)\n)"),
    plot_amount_format
    ("%D %(@S(@t))\n"),
    plot_total_format
    ("%D %(@S(@T))\n"),
    write_hdr_format
    ("%d %Y%C%P\n"),
    write_xact_format
    ("    %-34W  %12o%n\n"),
    prices_format
    ("%[%Y/%m/%d %H:%M:%S %Z]   %-10A %12t %12T\n"),
    pricesdb_format
    ("P %[%Y/%m/%d %H:%M:%S] %A %t\n"),

    pricing_leeway(24 * 3600),

    download_quotes(false),
    use_cache(false),
    cache_dirty(false),

    now(now),

    elision_style(ABBREVIATE),
    abbrev_length(2),

    ansi_codes(false),
    ansi_invert(false) {
    TRACE_CTOR(session_t, "xml::xpath_t::scope_t *");
  }

  virtual ~session_t() {
    TRACE_DTOR(session_t);

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
			    const string * original_file = NULL);

  unsigned int read_journal(const string&  path,
			    journal_t *         journal,
			    account_t *		master        = NULL,
			    const string * original_file = NULL);

  void read_init();

  journal_t * read_data(const string& master_account = "");

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

  virtual bool resolve(const string& name, value_t& result,
		       xml::xpath_t::scope_t * locals = NULL);
  virtual xml::xpath_t::op_t * lookup(const string& name);

  //
  // Debug options
  //

  void option_verify(value_t&) {}
  void option_trace(value_t&, xml::xpath_t::scope_t * locals) {}
  void option_debug(value_t&, xml::xpath_t::scope_t * locals) {}

  void option_verbose(value_t&) {
    _log_level = LOG_INFO;
  }

  //
  // Option handlers
  //

  void option_file(value_t&, xml::xpath_t::scope_t * locals) {
    data_file = locals->args.to_string();
  }

#if 0
#if defined(USE_BOOST_PYTHON)
  void option_import(value_t&) {
    python_import(optarg);
  }
  void option_import_stdin(value_t&) {
    python_eval(std::cin, PY_EVAL_MULTI);
  }
#endif
#endif
};

void initialize();
void shutdown();

} // namespace ledger

#endif // _SESSION_H
