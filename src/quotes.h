#ifndef _QUOTES_H
#define _QUOTES_H

#include "amount.h"

namespace ledger {

class quotes_by_script
{
  path		price_db;
  time_duration pricing_leeway;
  bool&         cache_dirty;

 public:
  quotes_by_script(path		 _price_db,
		   time_duration _pricing_leeway,
		   bool&         _cache_dirty)
    : price_db(_price_db), pricing_leeway(_pricing_leeway),
      cache_dirty(_cache_dirty) {}

  virtual optional<amount_t>
    operator()(commodity_t&		 commodity,
	       const optional<moment_t>& date,
	       const optional<moment_t>& moment,
	       const optional<moment_t>& last);
};

DECLARE_EXCEPTION(download_error);

} // namespace ledger

#endif // _QUOTES_H
