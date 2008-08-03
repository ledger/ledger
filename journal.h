/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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

#ifndef _JOURNAL_H
#define _JOURNAL_H

#include "utils.h"
#include "hooks.h"
#include "entry.h"

namespace ledger {

typedef std::list<path>	paths_list;

class session_t;
class account_t;

class journal_t : public noncopyable
{
public:
  session_t *	 owner;
  account_t *	 master;
  account_t *	 basket;
  entries_list	 entries;
  paths_list	 sources;
  optional<path> price_db;

  auto_entries_list    auto_entries;
  period_entries_list  period_entries;

  hooks_t<entry_finalizer_t, entry_t> entry_finalize_hooks;

  journal_t(session_t * _owner);
  ~journal_t();

  // These four methods are delegated to 'owner', since all accounts processed
  // are gathered together at the session level.
  void	      add_account(account_t * acct);
  bool	      remove_account(account_t * acct);
  account_t * find_account(const string& name, bool auto_create = true);
  account_t * find_account_re(const string& regexp);

  bool add_entry(entry_t * entry);
  bool remove_entry(entry_t * entry);

  void add_entry_finalizer(entry_finalizer_t * finalizer) {
    entry_finalize_hooks.add_hook(finalizer);
  }
  void remove_entry_finalizer(entry_finalizer_t * finalizer) {
    entry_finalize_hooks.remove_hook(finalizer);
  }

  /**
   * @class journal_t::parser_t
   *
   * @brief Provides an abstract interface for writing journal parsers.
   *
   * Any data format for Ledger data is possible, as long as it can be parsed
   * into a journal_t data tree.  This class provides the basic interface which
   * must be implemented by every such journal parser.
   */
  class parser_t : public noncopyable
  {
  public:
    parser_t() {
      TRACE_CTOR(journal_t::parser_t, "");
    }
    virtual ~parser_t() {
      TRACE_DTOR(journal_t::parser_t);
    }

    virtual bool test(std::istream& in) const = 0;

    virtual unsigned int parse(std::istream& in,
			       session_t&    session,
			       journal_t&    journal,
			       account_t *   master        = NULL,
			       const path *  original_file = NULL) = 0;
  };

  class binary_parser_t : public parser_t
  {
  public:
    virtual bool test(std::istream& in) const;

    virtual unsigned int parse(std::istream& in,
			       session_t&     session,
			       journal_t&   journal,
			       account_t *   master        = NULL,
			       const path *  original_file = NULL);
  };

  bool valid() const;
};

extern const string version;

} // namespace ledger

#endif // _JOURNAL_H
