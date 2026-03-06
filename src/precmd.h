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
 * @addtogroup report
 */

/**
 * @file   precmd.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Diagnostic pre-commands that do not require journal data.
 *
 * Pre-commands are special command verbs (registered as PRECOMMAND symbols)
 * that bypass journal loading entirely.  They are used for debugging and
 * exploring Ledger's internal representations:
 *
 *   - **parse**  : Show how a value expression is tokenized, parsed into
 *                  an AST, compiled, and evaluated against a sample posting.
 *   - **eval**   : Evaluate a value expression and print the result.
 *   - **format** : Show how a format string is parsed into format elements
 *                  and rendered against a sample posting.
 *   - **period** : Tokenize and parse a period expression, displaying its
 *                  date interval structure.
 *   - **query**  : Parse a query expression and display both the limit and
 *                  display predicates as compiled expression trees.
 *
 * Because these commands do not read the journal, they execute quickly
 * and are invaluable for understanding why a particular expression or
 * format string produces unexpected results.
 */
#pragma once

#include "value.h"

namespace ledger {

class call_scope_t;

/// Show expression parsing, compilation, and evaluation stages.
value_t parse_command(call_scope_t& args);

/// Evaluate a value expression and print the result.
value_t eval_command(call_scope_t& args);

/// Show format string parsing and rendering against a sample posting.
value_t format_command(call_scope_t& args);

/// Tokenize and display a period expression's date interval structure.
value_t period_command(call_scope_t& args);

/// Parse a query expression and display its limit and display predicates.
value_t query_command(call_scope_t& args);

} // namespace ledger
