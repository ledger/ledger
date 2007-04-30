#include "quotes.h"

namespace ledger {

void quotes_by_script::operator()(commodity_base_t& commodity,
				  const ptime&      moment,
				  const ptime&      date,
				  const ptime&      last,
				  amount_t&	    price)
{
  LOGGER("quotes.download");

  DEBUG_("commodity: " << commodity.symbol);
  DEBUG_("      now: " << now);
  DEBUG_("   moment: " << moment);
  DEBUG_("     date: " << date);
  DEBUG_("     last: " << last);

  if (SHOW_DEBUG_() && commodity.history)
    DEBUG_("last_lookup: " << commodity.history->last_lookup);
  DEBUG_("pricing_leeway is " << pricing_leeway);

  if ((commodity.history &&
       (time_now - commodity.history->last_lookup) < pricing_leeway) ||
      (time_now - last) < pricing_leeway ||
      (price && moment > date && (moment - date) <= pricing_leeway))
    return;

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
    commodity.add_price(now, price);

    commodity.history->last_lookup = time_now;
    cache_dirty = true;

    if (price) {
      assert(! price_db.empty());

#if defined(__GNUG__) && __GNUG__ < 3
      ofstream database(price_db, ios::out | ios::app);
#else
      ofstream database(price_db, std::ios_base::out | std::ios_base::app);
#endif
#if 0
      // jww (2007-04-18): Need to convert to local time and print
      // here, print with UTC timezone specifier
      database << "P " << now.to_string("%Y/%m/%d %H:%M:%S")
	       << " " << commodity.symbol << " " << price << endl;
#endif
    }
  } else {
    throw_(download_error,
	   "Failed to download price for '" << commodity.symbol <<
	   "' (command: \"getquote " << commodity.symbol << "\")");
  }
}

} // namespace ledger
