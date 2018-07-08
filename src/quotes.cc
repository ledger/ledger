/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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

optional<price_point_t>
commodity_quote_from_script(commodity_t& commodity,
                            const commodity_t * exchange_commodity)
{
  DEBUG("commodity.download", "downloading quote for symbol " << commodity.symbol());
#if DEBUG_ON
  if (exchange_commodity)
    DEBUG("commodity.download",
          "  in terms of commodity " << exchange_commodity->symbol());
#endif

  char buf[256];
  buf[0] = '\0';

  string getquote_cmd("getquote \"");
  getquote_cmd += commodity.symbol();
  getquote_cmd += "\" \"";
  if (exchange_commodity)
    getquote_cmd += exchange_commodity->symbol();
  getquote_cmd += "\"";

  DEBUG("commodity.download", "invoking command: " << getquote_cmd);

  bool success = true;
#if !defined(_WIN32) && !defined(__CYGWIN__)
  if (FILE * fp = popen(getquote_cmd.c_str(), "r")) {
    if (std::feof(fp) || ! std::fgets(buf, 255, fp))
      success = false;
    if (pclose(fp) != 0)
      success = false;
  } else {
    success = false;
  }

  if (success && buf[0]) {
    if (char * p = std::strchr(buf, '\n')) *p = '\0';
    DEBUG("commodity.download", "downloaded quote: " << buf);

    if (optional<std::pair<commodity_t *, price_point_t> > point =
        commodity_pool_t::current_pool->parse_price_directive(buf)) {
      if (commodity_pool_t::current_pool->price_db) {
#if defined(__GNUG__) && __GNUG__ < 3
        ofstream database(*commodity_pool_t::current_pool->price_db,
                          ios::out | ios::app);
#else
        ofstream database(*commodity_pool_t::current_pool->price_db,
                          std::ios_base::out | std::ios_base::app);
#endif
        database << "P "
                 << format_datetime(point->second.when, FMT_WRITTEN)
                 << " " << commodity.symbol()
                 << " " << point->second.price
                 << std::endl;
      }
      return point->second;
    }
  } else {
    DEBUG("commodity.download",
          "Failed to download price for '" << commodity.symbol() <<
          "' (command: \"getquote " << commodity.symbol() <<
          " " << (exchange_commodity ?
                  exchange_commodity->symbol() : "''") << "\")");

    // Don't try to download this commodity again.
    commodity.add_flags(COMMODITY_NOMARKET);
  }
#endif
  return none;
}

} // namespace ledger
