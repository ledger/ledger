#include "quotes.h"
#include "datetime.h"
#include "error.h"
#include "debug.h"

#include <fstream>
#include <cstdlib>
#include <cstdio>

namespace ledger {

void quotes_by_script::operator()(commodity_t&      commodity,
				  const std::time_t moment,
				  const std::time_t date,
				  const std::time_t last,
				  amount_t&	    price)
{
  DEBUG_CLASS("ledger.quotes.download");

  DEBUG_PRINT_("commodity: " << commodity.symbol);
  DEBUG_PRINT_TIME_(now);
  DEBUG_PRINT_TIME_(moment);
  DEBUG_PRINT_TIME_(date);
  DEBUG_PRINT_TIME_(last);
  if (commodity.history())
    DEBUG_PRINT_TIME_(commodity.history()->last_lookup);
  DEBUG_PRINT_("pricing_leeway is " << pricing_leeway);

  if ((commodity.history() &&
       std::difftime(now, commodity.history()->last_lookup) < pricing_leeway) ||
      std::difftime(now, last) < pricing_leeway ||
      (price && std::difftime(moment, date) > 0 &&
       std::difftime(moment, date) <= pricing_leeway))
    return;

  using namespace std;

  DEBUG_PRINT_("downloading quote for symbol " << commodity.symbol);

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

    DEBUG_PRINT_("downloaded quote: " << buf);

    price.parse(buf);
    commodity.add_price(now, price);

    commodity.history()->last_lookup = now;
    cache_dirty = true;

    if (price && ! price_db.empty()) {
      strftime(buf, 127, "%Y/%m/%d %H:%M:%S", localtime(&now));
#if defined(__GNUG__) && __GNUG__ < 3
      ofstream database(price_db.c_str(), ios::out | ios::app);
#else
      ofstream database(price_db.c_str(), ios_base::out | ios_base::app);
#endif
      database << "P " << buf << " " << commodity.symbol
	       << " " << price << endl;
    }
  } else {
    throw error(std::string("Failed to download price for '") +
		commodity.symbol + "' (command: \"getquote " +
		commodity.symbol + "\")");
  }
}

} // namespace ledger
