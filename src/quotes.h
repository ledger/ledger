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
 * @addtogroup extra
 */

/**
 * @file   quotes.h
 * @author John Wiegley
 *
 * @ingroup extra
 */
#pragma once

namespace ledger {

std::optional<price_point_t> commodity_quote_from_script(commodity_t& commodity,
                                                         const commodity_t* exchange_commodity);

/**
 * @brief Fetch price quotes for multiple commodities in a single script
 *        invocation.
 *
 * Invokes the getquote script with --batch, passing all commodity symbols
 * on the command line grouped by their exchange commodity.  The script is
 * expected to output one price directive per line ("P DATE SYMBOL AMOUNT")
 * for each commodity it was able to price.
 *
 * If the command fails (non-zero exit) or produces no output, an empty
 * vector is returned so the caller can fall back to per-commodity fetching.
 *
 * @param commodities        The commodities to fetch quotes for.
 * @param exchange_commodity The target currency (may be nullptr).
 * @return A vector of (commodity, price_point) pairs for successful quotes.
 */
std::vector<std::pair<commodity_t*, price_point_t>>
commodity_batch_quote_from_script(std::vector<std::reference_wrapper<commodity_t>>& commodities,
                                  const commodity_t* exchange_commodity);

} // namespace ledger
