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
 * @addtogroup data
 */

/**
 * @file   convert.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief CSV file import -- the `convert` command.
 *
 * The convert command reads a CSV file (typically a bank export), parses
 * each row into a transaction using csv_reader, and prints the resulting
 * Ledger-format transactions to stdout.  It supports duplicate detection
 * via UUID tags and SHA-1 checksums, optional account auto-matching based
 * on payee history, and amount inversion for credit-card statements.
 *
 * Usage: `ledger convert bank-export.csv --account Assets:Checking`
 */
#pragma once

#include "value.h"

namespace ledger {

class call_scope_t;

/**
 * @brief Implements the `ledger convert` command.
 *
 * Reads a CSV file whose path is the first positional argument, converts
 * each row into a journal transaction, deduplicates against existing
 * journal entries, and prints the new transactions in Ledger format.
 *
 * @param scope  Call scope containing the CSV file path argument and
 *               report options (--account, --invert, --rich-data, etc.).
 * @return       true on success.
 */
value_t convert_command(call_scope_t& scope);

} // namespace ledger
