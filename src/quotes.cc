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

#include "quotes.h"
#include "utils.h"

namespace ledger {

#if 0
void quotes_by_script::operator()(commodity_base_t& commodity,
				  const datetime_t& moment,
				  const datetime_t& date,
				  const datetime_t& last,
				  amount_t&	    price)
{
  DEBUG_CLASS("ledger.quotes.download");

  DEBUG_("commodity: " << commodity.symbol);
  DEBUG_TIME_(current_moment);
  DEBUG_TIME_(moment);
  DEBUG_TIME_(date);
  DEBUG_TIME_(last);
  if (commodity.history)
    DEBUG_TIME_(commodity.history->last_lookup);
  DEBUG_("pricing_leeway is " << pricing_leeway);

  if ((commodity.history &&
       (current_moment - commodity.history->last_lookup) < pricing_leeway) ||
      (current_moment - last) < pricing_leeway ||
      (price && moment > date && (moment - date) <= pricing_leeway))
    return;

  using namespace std;

  DEBUG_("downloading quote for symbol " << commodity.symbol);

  char buf[256];
  buf[0] = '\0';

  bool success = true;

  if (FILE * fp = popen((string("getquote \"") +
			 commodity.symbol + "\"").c_str(), "r")) {
    if (feof(fp) || ! fgets(buf, 255, fp))
      success = false;
    if (pclose(fp) != 0)
      success = false;
  } else {
    success = false;
  }

  if (success && buf[0]) {
    char * p = strchr(buf, '\n');
    if (p) *p = '\0';

    DEBUG_("downloaded quote: " << buf);

    price.parse(buf);
    commodity.add_price(current_moment, price);

    commodity.history->last_lookup = current_moment;

    if (price && ! price_db.empty()) {
#if defined(__GNUG__) && __GNUG__ < 3
      ofstream database(price_db.c_str(), ios::out | ios::app);
#else
      ofstream database(price_db.c_str(), ios_base::out | ios_base::app);
#endif
      database << "P " << current_moment.to_string("%Y/%m/%d %H:%M:%S")
	       << " " << commodity.symbol << " " << price << endl;
    }
  } else {
    throw_(std::runtime_error,
	   _("Failed to download price for '%1' (command: \"getquote %1\")")
	   << commodity.symbol);
  }
}
#endif

} // namespace ledger
