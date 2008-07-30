#ifndef _QUOTES_H
#define _QUOTES_H

#include "amount.h"

namespace ledger {

#if 0
class quotes_by_script : public noncopyable, public commodity_t::base_t::updater_t
{
  string	price_db;
  unsigned long pricing_leeway;
  bool&         cache_dirty;

  quotes_by_script();

public:
  quotes_by_script(path		 _price_db,
		   unsigned long _pricing_leeway,
		   bool&         _cache_dirty)
    : price_db(_price_db), pricing_leeway(_pricing_leeway),
      cache_dirty(_cache_dirty) {
    TRACE_CTOR(quotes_by_script, "path, unsigned long, bool&");
  }
  ~quotes_by_script() throw() {
    TRACE_DTOR(quotes_by_script);
  }

  virtual void operator()(commodity_base_t& commodity,
			  const datetime_t& moment,
			  const datetime_t& date,
			  const datetime_t& last,
			  amount_t&	    price);
};
#endif

} // namespace ledger

#endif // _QUOTES_H
