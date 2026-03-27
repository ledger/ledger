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

std::optional<price_point_t> commodity_quote_from_script(commodity_t& commodity,
                                                         const commodity_t* exchange_commodity) {
  DEBUG("commodity.download", "downloading quote for symbol " << commodity.symbol());
#if DEBUG_ON
  if (exchange_commodity)
    DEBUG("commodity.download", "  in terms of commodity " << exchange_commodity->symbol());
#endif

  char buf[4096];
  buf[0] = '\0';

  string getquote_cmd;
  if (commodity_pool_t::current_pool->getquote)
    getquote_cmd = shell_escape(commodity_pool_t::current_pool->getquote->string());
  else
    getquote_cmd = "getquote";

  getquote_cmd += " ";
  getquote_cmd += shell_escape(commodity.symbol());
  getquote_cmd += " ";
  if (exchange_commodity)
    getquote_cmd += shell_escape(exchange_commodity->symbol());
  else
    getquote_cmd += "''";

  DEBUG("commodity.download", "invoking command: " << getquote_cmd);

  bool success = true;
  bool cmd_failed = false; // popen failed or command exited non-zero
#if !defined(_WIN32) && !defined(__CYGWIN__)
  if (FILE* fp = popen(getquote_cmd.c_str(), "r")) {
    if (std::feof(fp) || !std::fgets(buf, sizeof(buf), fp)) {
      success = false;
    } else {
      // Check for truncation (no newline means line was too long)
      std::size_t buflen = std::strlen(buf);
      if (buflen > 0 && buf[buflen - 1] != '\n') {
        // Consume rest of line
        char discard[4096];
        while (std::fgets(discard, sizeof(discard), fp) != nullptr) {
          std::size_t dlen = std::strlen(discard);
          if (dlen > 0 && discard[dlen - 1] == '\n')
            break;
        }
        success = false; // Don't process truncated output
      }
    }
    if (pclose(fp) != 0) {
      success = false;
      cmd_failed = true;
    }
  } else {
    success = false;
    cmd_failed = true;
  }

  if (success && buf[0]) {
    if (char* p = std::strchr(buf, '\n'))
      *p = '\0';
    DEBUG("commodity.download", "downloaded quote: " << buf);

    if (std::optional<std::pair<commodity_t*, price_point_t>> point =
            commodity_pool_t::current_pool->parse_price_directive(buf)) {
      if (commodity_pool_t::current_pool->price_db) {
        ofstream database(*commodity_pool_t::current_pool->price_db,
                          std::ios_base::out | std::ios_base::app);
        database << "P " << format_datetime(point->second.when, FMT_WRITTEN) << " "
                 << commodity.symbol() << " " << point->second.price << '\n';
      }
      return point->second;
    }
  } else {
    if (cmd_failed)
      warning_(std::string("Price quote script failed for commodity '") + commodity.symbol() + "'");
    else
      warning_(std::string("Price quote script returned no usable price for commodity '") +
               commodity.symbol() + "'");

    DEBUG("commodity.download", "Failed to download price for '"
                                    << commodity.symbol() << "' (command: \""
                                    << (commodity_pool_t::current_pool->getquote
                                            ? commodity_pool_t::current_pool->getquote->string()
                                            : "getquote")
                                    << " " << commodity.symbol() << " "
                                    << (exchange_commodity ? exchange_commodity->symbol() : "''")
                                    << "\")");

    // The caller (check_for_updated_price) throttles retries via last_quote.
  }
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
  string getquote_cmd;
  if (commodity_pool_t::current_pool->getquote)
    getquote_cmd = shell_escape(commodity_pool_t::current_pool->getquote->string());
  else
    getquote_cmd = "getquote";

  getquote_cmd += " --batch ";
  if (exchange_commodity)
    getquote_cmd += shell_escape(exchange_commodity->symbol());
  else
    getquote_cmd += "''";

  for (commodity_t& comm : commodities) {
    getquote_cmd += " ";
    getquote_cmd += shell_escape(comm.symbol());
  }

  DEBUG("commodity.download", "invoking batch command: " << getquote_cmd);

  bool cmd_failed = false;
  if (FILE* fp = popen(getquote_cmd.c_str(), "r")) {
    char buf[4096];
    while (std::fgets(buf, sizeof(buf), fp) != nullptr) {
      // Check for truncation (no newline means line was too long)
      std::size_t buflen = std::strlen(buf);
      if (buflen > 0 && buf[buflen - 1] != '\n') {
        // Consume rest of line and skip it
        char discard[4096];
        while (std::fgets(discard, sizeof(discard), fp) != nullptr) {
          std::size_t dlen = std::strlen(discard);
          if (dlen > 0 && discard[dlen - 1] == '\n')
            break;
        }
        continue;
      }

      // Strip trailing newline
      if (buflen > 0 && buf[buflen - 1] == '\n')
        buf[buflen - 1] = '\0';

      if (buf[0] == '\0')
        continue;

      DEBUG("commodity.download", "batch downloaded quote: " << buf);

      if (std::optional<std::pair<commodity_t*, price_point_t>> point =
              commodity_pool_t::current_pool->parse_price_directive(buf)) {
        if (commodity_pool_t::current_pool->price_db) {
          ofstream database(*commodity_pool_t::current_pool->price_db,
                            std::ios_base::out | std::ios_base::app);
          database << "P " << format_datetime(point->second.when, FMT_WRITTEN) << " "
                   << point->first->symbol() << " " << point->second.price << '\n';
        }
        results.push_back(*point);
      }
    }
    if (pclose(fp) != 0)
      cmd_failed = true;
  } else {
    cmd_failed = true;
  }

  if (cmd_failed) {
    DEBUG("commodity.download", "batch quote command failed");
    results.clear();
  } else if (results.empty()) {
    DEBUG("commodity.download", "batch quote command returned no usable prices");
  } else {
    DEBUG("commodity.download", "batch quote returned " << results.size() << " prices");
  }
#endif
  return results;
}

} // namespace ledger
