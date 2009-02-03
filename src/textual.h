/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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

/**
 * @defgroup parse Parsers
 */

/**
 * @file   textual.h
 * @author John Wiegley
 *
 * @ingroup parse
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _TEXTUAL_H
#define _TEXTUAL_H

#include "journal.h"
#include "handler.h"

namespace ledger {

#define TIMELOG_SUPPORT 1

#if defined(TIMELOG_SUPPORT)
class time_log_t;
#endif

/**
 * @brief Brief
 *
 * Long.
 */
class textual_parser_t : public journal_t::parser_t
{
public:
  virtual bool test(std::istream& in) const;

  virtual std::size_t parse(std::istream& in,
			    session_t&    session,
			    journal_t&    journal,
			    account_t *   master        = NULL,
			    const path *  original_file = NULL);

protected:
  class instance_t : public noncopyable, public scope_t
  {
    static const std::size_t MAX_LINE = 1024;

  public:
    std::list<account_t *>& account_stack;
#if defined(TIMELOG_SUPPORT)
    time_log_t&		    timelog;
#endif

    instance_t *      parent;
    std::istream&     in;
    session_t&	      session;
    journal_t&	      journal;
    account_t *	      master;
    const path *      original_file;
    accounts_map      account_aliases;

    path	      pathname;
    std::size_t       linenum;
    istream_pos_type  beg_pos;
    std::size_t       beg_line;
    istream_pos_type  end_pos;
    std::size_t       count;
    std::size_t       errors;

    char	      linebuf[MAX_LINE + 1];

    scoped_ptr<auto_entry_finalizer_t> auto_entry_finalizer;

    instance_t(std::list<account_t *>& _account_stack,
#if defined(TIMELOG_SUPPORT)
	       time_log_t&             _timelog,
#endif
	       std::istream&	       _in,
	       session_t&	       _session,
	       journal_t&	       _journal,
	       account_t *	       _master        = NULL,
	       const path *	       _original_file = NULL,
	       instance_t *            _parent        = NULL);

    ~instance_t();

    void parse();
    void read_next_directive(); 

#if defined(TIMELOG_SUPPORT)
    void clock_in_directive(char * line, bool capitalized);
    void clock_out_directive(char * line, bool capitalized);
#endif

    void default_commodity_directive(char * line);
    void default_account_directive(char * line);
    void price_conversion_directive(char * line);
    void price_entry_directive(char * line);
    void nomarket_directive(char * line);
    void year_directive(char * line);
    void option_directive(char * line);
    void automated_entry_directive(char * line);
    void period_entry_directive(char * line);
    void entry_directive(char * line);
    void include_directive(char * line);
    void account_directive(char * line);
    void end_directive(char * line);
    void alias_directive(char * line);
    void define_directive(char * line);
    void general_directive(char * line);

    xact_t * parse_xact(char *	    line,
			account_t * account,
			entry_t *   entry);

    bool parse_xacts(std::istream&    in,
		     account_t *      account,
		     entry_base_t&    entry,
		     const string&    kind,
		     istream_pos_type beg_pos);

    entry_t * parse_entry(std::istream&     in,
			  char *	    line,
			  account_t *	    master,
			  istream_pos_type& pos);

    virtual expr_t::ptr_op_t lookup(const string& name);
  };

  friend class instance_t;
};

void write_textual_journal(journal_t&	     journal,
			   const path&	     pathname,
			   xact_handler_ptr& formatter,
			   const string&     write_hdr_format,
			   std::ostream&     out);

} // namespace ledger

#endif // _TEXTUAL_H
