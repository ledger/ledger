#include "quotes.h"
#include "datetime.h"
#include "debug.h"

#include <fstream>
#include <stdlib.h>

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
  DEBUG_PRINT_TIME_(commodity.last_lookup);
  DEBUG_PRINT_("pricing_leeway is " << pricing_leeway);

  if (std::difftime(now, commodity.last_lookup) < pricing_leeway ||
      std::difftime(now, last) < pricing_leeway ||
      (price && std::difftime(moment, date) <= pricing_leeway))
    return;

  using namespace std;

  DEBUG_PRINT_("downloading quote for symbol " << commodity.symbol);

  // Only consult the Internet once for any commodity
  commodity.last_lookup = now;
  cache_dirty = true;

  char buf[256];
  buf[0] = '\0';

  bool success = true;

  if (FILE * fp = popen((string("getquote ") +
			 commodity.symbol).c_str(), "r")) {
    if (feof(fp) || ! fgets(buf, 255, fp))
      success = false;
    fclose(fp);
  }

  if (success && buf[0]) {
    char * p = strchr(buf, '\n');
    if (p) *p = '\0';

    DEBUG_PRINT_("downloaded quote: " << buf);

    price.parse(buf);
    commodity.add_price(now, price);

    if (price && ! price_db.empty()) {
      char buf[128];
      strftime(buf, 127, "%Y/%m/%d %H:%M:%S", localtime(&now));
      ofstream database(price_db.c_str(), ios_base::out | ios_base::app);
      database << "P " << buf << " " << commodity.symbol
	       << " " << price << endl;
    }
  }
}

} // namespace ledger
