/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _SESSION_H
#define _SESSION_H

#include "xpath.h"
#include "journal.h"
#include "parser.h"
#include "abbrev.h"

namespace ledger {

class session_t : public xml::xpath_t::scope_t
{
 public:
  static session_t * current;

  path		 data_file;
  optional<path> init_file;
  optional<path> cache_file;
  optional<path> price_db;

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

  ptr_list<journal_t> journals;
  ptr_list<parser_t>  parsers;

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
  }

  journal_t * create_journal() {
    journal_t * journal = new journal_t(this);
    journals.push_back(journal);
    return journal;
  }
  void close_journal(journal_t * journal) {
    for (ptr_list<journal_t>::iterator i = journals.begin();
	 i != journals.end();
	 i++)
      if (&*i == journal) {
	journals.erase(i);
	return;
      }
    assert(false);
    checked_delete(journal);
  }

  std::size_t read_journal(std::istream&   in,
			   const path&	   pathname,
			   xml::builder_t& builder);
  std::size_t read_journal(const path&	   pathname,
			   xml::builder_t& builder);

  void read_init();

  std::size_t read_data(xml::builder_t& builder,
			journal_t *	journal,
			const string&	master_account = "");

  void register_parser(parser_t * parser) {
    parsers.push_back(parser);
  }
  void unregister_parser(parser_t * parser) {
    for (ptr_list<parser_t>::iterator i = parsers.begin();
	 i != parsers.end();
	 i++)
      if (&*i == parser) {
	parsers.erase(i);
	return;
      }
    assert(false);
    checked_delete(parser);
  }

  //
  // Scope members
  //

  virtual bool resolve(const string& name, value_t& result,
		       xml::xpath_t::scope_t * locals = NULL);
  virtual xml::xpath_t::ptr_op_t lookup(const string& name);

  //
  // Debug options
  //

  value_t option_trace_(xml::xpath_t::scope_t * locals) {}
  value_t option_debug_(xml::xpath_t::scope_t * locals) {}

  value_t option_verify(xml::xpath_t::scope_t *) {}
  value_t option_verbose(xml::xpath_t::scope_t *) {
#if defined(LOGGING_ON)
    if (_log_level < LOG_INFO)
      _log_level = LOG_INFO;
#endif
  }

  //
  // Option handlers
  //

  value_t option_file_(xml::xpath_t::scope_t * locals) {
    assert(locals->args.size() == 1);
    data_file = locals->args[0].as_string();
  }

#if 0
#if defined(USE_BOOST_PYTHON)
  value_t option_import_(xml::xpath_t::scope_t * locals) {
    python_import(optarg);
  }
  value_t option_import_stdin(xml::xpath_t::scope_t * locals) {
    python_eval(std::cin, PY_EVAL_MULTI);
  }
#endif
#endif
};

/**
 * This sets the current session context, transferring all static
 * globals to point at the data structures related to this session.
 * Although Ledger itself is not thread-safe, by locking, switching
 * session context, then unlocking after the operation is done,
 * multiple threads can sequentially make use of the library.  Thus, a
 * session_t maintains all of the information relating to a single
 * usage of the Ledger library.
 */
void set_session_context(session_t * session = NULL);

} // namespace ledger

#endif // _SESSION_H
