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
 * @addtogroup parse
 */

/**
 * @file   cache.h
 * @author John Wiegley
 *
 * @ingroup parse
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef CACHE_H
#define CACHE_H

#include "utils.h"
#include "session.h"
#include "journal.h"
#include "account.h"

namespace ledger {

DECLARE_EXCEPTION(cache_error, std::runtime_error);

/**
 * @brief Brief
 *
 * Long.
 */
class binary_cache_t
{
  static const unsigned long binary_magic_number = 0xFFEED765;
#if defined(DEBUG_ON)
  static const unsigned long format_version      = 0x0002060d;
#else
  static const unsigned long format_version      = 0x0002060c;
#endif

  scoped_array<char>  item_pool;

  std::vector<account_t *> accounts;
  account_t::ident_t  account_ident;

  entry_t *           entry_pool; // points into item_pool
  std::size_t	      entry_count;
  std::size_t	      auto_entry_count;
  std::size_t	      period_entry_count;

  xact_t *            xact_pool;  // points into item_pool
  std::size_t	      xact_count;

#if 0
  commodity_base_t ** base_commodities; // allocated
  commodity_base_t ** base_commodities_next;
  uint_fast32_t	      base_commodity_index;
  std::size_t	      base_commodity_count;
#endif

  commodity_t **      commodities; // allocated
  commodity_t **      commodities_next;
  uint_fast32_t	      commodity_ident;
  std::size_t	      commodity_count;

  char *	      bigints;	// points into item_pool
  char *	      bigints_next;
  uint_fast32_t	      bigints_index;
  std::size_t	      bigints_count;

  void read_xact(const char *& data, xact_t * xact);
  void write_xact(std::ostream& out, xact_t * xact,
		  bool ignore_calculated);

  void read_entry_base(const char *& data, entry_base_t * entry,
		       xact_t *& xact_pool, bool& finalize);
  void write_entry_base(std::ostream& out, entry_base_t * entry);
  void read_entry(const char *& data, entry_t * entry,
		  xact_t *& xact_pool, bool& finalize);
  void write_entry(std::ostream& out, entry_t * entry);
  void read_auto_entry(const char *& data, auto_entry_t * entry,
		       xact_t *& xact_pool);
  void write_auto_entry(std::ostream& out, auto_entry_t * entry);
  void read_period_entry(const char *& data, period_entry_t * entry,
			 xact_t *& xact_pool, bool& finalize);
  void write_period_entry(std::ostream& out, period_entry_t * entry);

#if 0
  commodity_t::base_t * read_commodity_base(const char *& data);
  void write_commodity_base(std::ostream& out, commodity_t::base_t * commodity);
  void read_commodity_base_extra(const char *& data,
				 commodity_t::ident_t ident);
  void write_commodity_base_extra(std::ostream& out,
				  commodity_t::base_t * commodity);
#endif

  commodity_t * read_commodity(const char *& data);
  void          write_commodity(std::ostream& out, commodity_t * commodity);
  commodity_t * read_commodity_annotated(const char *& data);
  void          write_commodity_annotated(std::ostream& out,
					  commodity_t * commodity);

  account_t * read_account(const char *& data, account_t * master = NULL);
  void        write_account(std::ostream& out);

  std::size_t read_journal(std::istream& in,
			   const path&	 file,
			   journal_t&	 journal,
			   account_t *	 master);
  void        write_journal(std::ostream&    out,
			    const journal_t& journal);

public:
  binary_cache_t()
    : account_ident(0),
#if 0
      base_commodity_ident(0),
#endif
      commodity_ident(0)
  {
  }

  std::size_t read_session(std::istream& in, const path& file);
  void        write_session(std::ostream& out, session_t& session);

};

} // namespace ledger

#endif // CACHE_H
