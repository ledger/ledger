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
    cache_dirty = true;

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
    throw new error(string("Failed to download price for '") +
		    commodity.symbol + "' (command: \"getquote " +
		    commodity.symbol + "\")");
  }
}
#endif

} // namespace ledger
