#include "constraint.h"
#include "expr.h"

#include <pcre.h>

namespace ledger {

constraints_t::~constraints_t()
{
  if (predicate)  delete predicate;
  if (sort_order) delete sort_order;
}

mask_t::mask_t(const std::string& pat) : exclude(false)
{
  const char * p = pat.c_str();
  if (*p == '-') {
    exclude = true;
    p++;
    while (std::isspace(*p))
      p++;
  }
  else if (*p == '+') {
    p++;
    while (std::isspace(*p))
      p++;
  }
  pattern = p;

  const char *error;
  int erroffset;
  regexp = pcre_compile(pattern.c_str(), PCRE_CASELESS,
			&error, &erroffset, NULL);
  if (! regexp)
    std::cerr << "Warning: Failed to compile regexp: " << pattern
	      << std::endl;
}

mask_t::mask_t(const mask_t& m) : exclude(m.exclude), pattern(m.pattern)
{
  const char *error;
  int erroffset;
  regexp = pcre_compile(pattern.c_str(), PCRE_CASELESS,
			&error, &erroffset, NULL);
  assert(regexp);
}

bool mask_t::match(const std::string& str) const
{
  static int ovec[30];
  int result = pcre_exec((pcre *)regexp, NULL,
			 str.c_str(), str.length(), 0, 0, ovec, 30);
  return result >= 0 && ! exclude;
}

mask_t::~mask_t() {
  pcre_free((pcre *)regexp);
}

bool matches(const masks_list& regexps, const std::string& str,
	     bool * by_exclusion)
{
  if (regexps.empty())
    return false;

  bool match    = false;
  bool definite = false;

  for (masks_list::const_iterator r = regexps.begin();
       r != regexps.end();
       r++) {
    static int ovec[30];
    int result = pcre_exec((pcre *)(*r).regexp, NULL,
			   str.c_str(), str.length(), 0, 0, ovec, 30);
    if (result >= 0) {
      match     = ! (*r).exclude;
      definite  = true;
    }
    else if ((*r).exclude) {
      if (! match)
	match = ! definite;
    }
    else {
      definite = true;
    }
  }

  if (by_exclusion)
    *by_exclusion = match && ! definite && by_exclusion;

  return match;
}

bool constraints_t::matches_date_range(const std::time_t date) const
{
  if (begin_date != -1 && difftime(date, begin_date) < 0)
    return false;

  if (end_date != -1 && difftime(date, end_date) >= 0)
    return false;

  if (have_date_mask) {
    struct std::tm * then = std::gmtime(&date);

    if (date_mask.tm_mon != -1 &&
	date_mask.tm_mon != then->tm_mon)
      return false;

    if (date_mask.tm_mday != -1 &&
	date_mask.tm_mday != then->tm_mday)
      return false;

#if 0
    // jww (2003-10-10): This causes only certain days of the week to
    // print, even when it was not included in the mask.
    if (date_mask.tm_wday != -1 &&
	date_mask.tm_wday != then->tm_wday)
      return false;
#endif

    if (date_mask.tm_year != -1 &&
	date_mask.tm_year != then->tm_year)
      return false;
  }

  return true;
}

bool constraints_t::operator ()(const transaction_t * xact) const
{
  if ((cleared_only && xact->entry->state != entry_t::CLEARED) ||
      (uncleared_only && xact->entry->state == entry_t::CLEARED) ||
      ! matches_date_range(xact->entry->date))
    return false;

  if (! payee_masks.empty() &&
      (! (matches(payee_masks, xact->entry->payee)
	  //|| matches(payee_masks, xact->entry->code))
	  )))
    return false;

  if (real_only && xact->flags & TRANSACTION_VIRTUAL)
    return false;

  if (! account_masks.empty() &&
      ! (matches(account_masks, std::string(*(xact->account)))
	 //|| matches(account_masks, (*i)->note)
	 ))
    return false;

  return true;
}

bool constraints_t::operator ()(const entry_t * entry) const
{
  if ((cleared_only && entry->state != entry_t::CLEARED) ||
      (uncleared_only && entry->state == entry_t::CLEARED) ||
      ! matches_date_range(entry->date))
    return false;

  if (! payee_masks.empty() &&
      (! (matches(payee_masks, entry->payee)
	  //|| matches(payee_masks, entry->code)
	  )))
    return false;

  if (! account_masks.empty()) {
    bool match = false;

    for (transactions_list::const_iterator i = entry->transactions.begin();
	 i != entry->transactions.end();
	 i++) {
      if (real_only && (*i)->flags & TRANSACTION_VIRTUAL)
	continue;

      if (matches(account_masks, std::string(*((*i)->account)))
	  //|| matches(account_masks, (*i)->note)
	  ) {
	match = true;
	break;
      }
    }

    if (! match)
      return false;
  }

  return true;
}

bool constraints_t::operator ()(const item_t * item) const
{
  if (predicate && ! predicate->compute(item, begin(), end()))
    return false;

  if (! matches_date_range(item->date))
    return false;

  if (! payee_masks.empty() && ! matches(payee_masks, item->payee))
    return false;

#if 0
  // jww (2004-07-26): It shouldn't be necessary to check against the
  // account here, since this is always done during initial compiling
  // of the item_t tree.

  if (! account_masks.empty()) {
    bool match = false;

    for (amounts_map::const_iterator i = item->value.quantity.amounts.begin();
	 i != item->value.quantity.amounts.end();
	 i++) {
      if (matches(account_masks, std::string(*((*i)->account)))
	  //|| matches(account_masks, (*i)->note)
	  ) {
	match = true;
	break;
      }
    }

    if (! match)
      return false;
  }
#endif

  return true;
}

} // namespace ledger
