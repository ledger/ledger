#include "quotes.h"
#include "datetime.h"

#include <fstream>

namespace ledger {

void quotes_by_script::operator()(commodity_t *     commodity,
				  const std::time_t date,
				  const amount_t&   price,
				  const std::time_t moment)
{
  if (! (commodity->flags & COMMODITY_STYLE_CONSULTED) &&
      std::difftime(now, moment) < pricing_leeway &&
      (! price || std::difftime(moment, date) > pricing_leeway)) {
    using namespace std;

    // Only consult the Internet once for any commodity
    commodity->flags |= COMMODITY_STYLE_CONSULTED;
    cache_dirty = true;

    char buf[256];
    buf[0] = '\0';

    if (FILE * fp = popen((string("getquote ") +
			   commodity->symbol).c_str(), "r")) {
      if (feof(fp) || ! fgets(buf, 255, fp)) {
	fclose(fp);
	return;
      }
      fclose(fp);
    }

    if (buf[0]) {
      char * p = strchr(buf, '\n');
      if (p) *p = '\0';

      amount_t current;
      current.parse(buf);

      commodity->add_price(now, current);

      if (! price_db.empty()) {
	char buf[128];
	strftime(buf, 127, "%Y/%m/%d %H:%M:%S", localtime(&now));
	ofstream database(price_db.c_str(), ios_base::out | ios_base::app);
	database << "P " << buf << " " << commodity->symbol << " "
		 << current << endl;
      }
    }
  }
}

} // namespace ledger
