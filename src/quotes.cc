#include "quotes.h"

namespace ledger {

optional<amount_t>
quotes_by_script::operator()(commodity_t&	       commodity,
			     const optional<moment_t>& date,
			     const optional<moment_t>& moment,
			     const optional<moment_t>& last)
{
  LOGGER("quotes.download");

  IF_DEBUG_() {
    DEBUG_("commodity: " << commodity.symbol());
    DEBUG_("      now: " << now);
    if (date)
      DEBUG_("     date: " << date);
    if (moment)
      DEBUG_("   moment: " << moment);
    if (last)
      DEBUG_("     last: " << last);
    if (commodity.history())
      DEBUG_("last_lookup: " << commodity.history()->last_lookup);
  }
  DEBUG_("pricing_leeway is " << pricing_leeway);

  if ((commodity.history() &&
       (now - commodity.history()->last_lookup) < pricing_leeway) ||
      (last && (now - *last) < pricing_leeway) ||
      (moment && date && *moment > *date &&
       (*moment - *date) <= pricing_leeway))
    return optional<amount_t>();

  DEBUG_("downloading quote for symbol " << commodity.symbol());

  char buf[256];
  buf[0] = '\0';

  bool success = true;

  if (FILE * fp = popen((string("getquote \"") +
			 commodity.base_symbol() + "\"").c_str(), "r")) {
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

    amount_t price;
    price.parse(buf);
    commodity.add_price(now, price);

    commodity.history()->last_lookup = now;
    cache_dirty = true;

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
    return price;
  } else {
    throw_(download_error,
	   "Failed to download price for '" << commodity.symbol() <<
	   "' (command: \"getquote " << commodity.base_symbol() << "\")");
  }
}

} // namespace ledger
