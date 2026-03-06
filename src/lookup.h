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
 * @file   lookup.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Probabilistic payee-to-account matching for transaction drafting.
 *
 * This module provides a fuzzy-matching algorithm that, given a payee
 * name (or partial name), searches the journal history for the most
 * likely matching transaction and its associated accounts.  It is the
 * core intelligence behind Ledger's "xact" command, which drafts new
 * transactions based on historical patterns.
 *
 * The algorithm scores each historical payee against the search
 * identifier using a character-by-character similarity metric that
 * rewards:
 *   - Exact matches (score 100, terminates search)
 *   - In-sequence character runs (10 points, with chain bonuses)
 *   - In-order but non-adjacent matches (5 points)
 *   - Proximity bonuses for nearby matches
 *   - Position decay so that early characters weigh more heavily
 *     (useful for credit card payees that share trailing location info)
 *
 * After scoring, the top five matching transactions are examined to
 * find the most frequently used account (excluding any reference
 * account), with a recency bias applied to break ties.
 */
#pragma once

#include "iterators.h"

namespace ledger {

/**
 * @brief Find the most probable transaction and account for a given payee.
 *
 * Searches backward through the journal's transaction list, scoring each
 * payee against @p ident using a fuzzy character-matching algorithm.
 * Returns the best-matching transaction and the most commonly used
 * account among the top matches.
 *
 * @param ident        The payee name (or partial name) to search for.
 * @param iter         Reverse iterator to the start of the transaction list.
 * @param end          Reverse iterator past the end of the transaction list.
 * @param ref_account  If non-null, this account is excluded from the
 *                     account frequency count (typically the "from" account).
 * @return A pair of (best matching transaction, most probable account).
 *         Either or both may be null if no match scores high enough.
 */
std::pair<xact_t*, account_t*> lookup_probable_account(const string& ident,
                                                       xacts_list::reverse_iterator iter,
                                                       const xacts_list::reverse_iterator& end,
                                                       account_t* ref_account = nullptr);

} // namespace ledger
