/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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
 * @addtogroup util
 */

/**
 * @file   select.h
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief SQL-like SELECT statement parser for Ledger reports.
 *
 * Provides an alternative query syntax that lets users write queries
 * resembling SQL SELECT statements rather than Ledger's native command-line
 * options.  For example:
 *
 * @code
 *   ledger select date, payee, account, amount from posts where account =~ /Food/
 * @endcode
 *
 * The parser recognizes these clauses:
 *
 *   - **SELECT** columns  -- translated into a format string
 *   - **FROM** source     -- xacts | posts | accounts | commodities
 *   - **WHERE** expr      -- maps to `--limit`
 *   - **DISPLAY** expr    -- maps to `--display`
 *   - **COLLECT** expr    -- maps to `--amount`
 *   - **GROUP BY** expr   -- maps to `--group-by`
 *   - **STYLE** name      -- output style (csv, xml, json, emacs) (placeholder)
 *
 * Each clause is translated into the corresponding report option, then a
 * reporter functor is constructed and executed just as if the user had run
 * a traditional Ledger command with the equivalent flags.
 */
#pragma once

#include "utils.h"
#include "value.h"

namespace ledger {

class call_scope_t;

/**
 * @brief Execute a SQL-like SELECT query against the journal.
 *
 * Parses the SELECT statement from @p args, translates each clause into
 * report options and a format string, constructs the appropriate reporter
 * functor (for postings, accounts, or commodities), and runs it.
 *
 * @param args  Call scope whose arguments form the SELECT statement text.
 * @return      The result of executing the report functor.
 */
value_t select_command(call_scope_t& args);

} // namespace ledger
