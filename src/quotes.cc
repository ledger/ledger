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

#include <system.hh>

#include "amount.h"
#include "commodity.h"
#include "pool.h"
#include "quotes.h"

namespace ledger {

#if !defined(_WIN32) && !defined(__CYGWIN__)
static std::string shell_escape(const std::string& str) {
  std::string result = "'";
  for (char c : str) {
    if (c == '\'')
      result += "'\\''";
    else
      result += c;
  }
  result += "'";
  return result;
}

/**
 * Run the getquote script with ARGS, which are already shell-escaped, and
 * record the prices it prints.  Nothing is parsed until the script has
 * exited 0, so a failed run records nothing.  Returns nullopt when the
 * script cannot be run or exits non-zero.
 */
static std::optional<std::vector<std::pair<commodity_t*, price_point_t>>>
run_getquote(const string& args) {
  commodity_pool_t& pool(*commodity_pool_t::current_pool);
  string getquote_cmd =
      (pool.getquote ? shell_escape(pool.getquote->string()) : string("getquote")) + " " + args;
  DEBUG("commodity.download", "invoking command: " << getquote_cmd);

  FILE* fp = popen(getquote_cmd.c_str(), "r");
  if (!fp)
    return std::nullopt;

  // Lines without a newline -- longer than the buffer, or a last line the
  // script did not end -- are skipped whole, as before.
  std::vector<string> lines;
  char buf[4096];
  bool truncated = false;
  while (std::fgets(buf, sizeof(buf), fp) != nullptr) {
    std::size_t len = std::strlen(buf);
    if (len == 0 || buf[len - 1] != '\n') {
      truncated = true;
      continue;
    }
    if (!truncated && len > 1)
      lines.emplace_back(buf, len - 1);
    truncated = false;
  }
  if (pclose(fp) != 0)
    return std::nullopt;

  std::vector<std::pair<commodity_t*, price_point_t>> quotes;
  for (string& line : lines) {
    DEBUG("commodity.download", "downloaded quote: " << line);
    if (std::optional<std::pair<commodity_t*, price_point_t>> point =
            pool.parse_price_directive(line.data())) {
      if (pool.price_db) {
        ofstream database(*pool.price_db, std::ios_base::out | std::ios_base::app);
        database << "P " << format_datetime(point->second.when, FMT_WRITTEN) << " "
                 << point->first->symbol() << " " << point->second.price << '\n';
      }
      quotes.push_back(*point);
    }
  }
  return quotes;
}
#endif

std::optional<price_point_t> commodity_quote_from_script(commodity_t& commodity,
                                                         const commodity_t* exchange_commodity) {
  DEBUG("commodity.download", "downloading quote for symbol " << commodity.symbol());
#if DEBUG_ON
  if (exchange_commodity)
    DEBUG("commodity.download", "  in terms of commodity " << exchange_commodity->symbol());
#endif

#if !defined(_WIN32) && !defined(__CYGWIN__)
  if (std::optional<std::vector<std::pair<commodity_t*, price_point_t>>> quotes = run_getquote(
          shell_escape(commodity.symbol()) + " " +
          shell_escape(exchange_commodity ? exchange_commodity->symbol() : string()))) {
    // The script may answer for other commodities too; their prices are
    // recorded under their own symbols and only this commodity's is returned.
    for (const std::pair<commodity_t*, price_point_t>& quote : *quotes)
      if (&quote.first->referent() == &commodity.referent())
        return quote.second;
    warning_(std::string("Price quote script returned no usable price for commodity '") +
             commodity.symbol() + "'");
  } else {
    warning_(std::string("Price quote script failed for commodity '") + commodity.symbol() + "'");
  }
  // The caller (check_for_updated_price) throttles retries via last_quote.
#endif
  return std::nullopt;
}

std::vector<std::pair<commodity_t*, price_point_t>>
commodity_batch_quote_from_script(std::vector<std::reference_wrapper<commodity_t>>& commodities,
                                  const commodity_t* exchange_commodity) {
  std::vector<std::pair<commodity_t*, price_point_t>> results;

  if (commodities.empty())
    return results;

  DEBUG("commodity.download",
        "batch downloading quotes for " << commodities.size() << " commodities");
#if DEBUG_ON
  if (exchange_commodity)
    DEBUG("commodity.download", "  in terms of commodity " << exchange_commodity->symbol());
#endif

#if !defined(_WIN32) && !defined(__CYGWIN__)
  string args =
      "--batch " + shell_escape(exchange_commodity ? exchange_commodity->symbol() : string());
  for (commodity_t& comm : commodities)
    args += " " + shell_escape(comm.symbol());

  if (std::optional<std::vector<std::pair<commodity_t*, price_point_t>>> quotes =
          run_getquote(args)) {
    results = std::move(*quotes);
    DEBUG("commodity.download", "batch quote returned " << results.size() << " prices");
  } else {
    DEBUG("commodity.download", "batch quote command failed");
  }
#endif
  return results;
}

} // namespace ledger
