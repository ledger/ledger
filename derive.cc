#include "derive.h"
#include "datetime.h"
#include "error.h"
#include "mask.h"

namespace ledger {

entry_t * derive_new_entry(journal_t& journal,
			   strings_list::iterator i,
			   strings_list::iterator end)
{
  std::auto_ptr<entry_t> added(new entry_t);

  entry_t * matching = NULL;

  if (! parse_date((*i).c_str(), &added->date))
    throw error("Bad date passed to 'entry'");

  if (++i == end)
    throw error("Too few arguments to 'entry'");

  mask_t regexp(*i++);

  for (entries_list::reverse_iterator j = journal.entries.rbegin();
       j != journal.entries.rend();
       j++)
    if (regexp.match((*j)->payee)) {
      matching = *j;
      break;
    }

  added->payee = matching ? matching->payee : regexp.pattern;

  if (i == end)
    throw error("Too few arguments to 'entry'");

  if ((*i)[0] == '-' || std::isdigit((*i)[0])) {
    if (! matching)
      throw error("Could not determine the account to draw from");

    transaction_t * m_xact, * xact, * first;
    m_xact = matching->transactions.front();

    first = xact = new transaction_t(m_xact->account, amount_t(*i++));
    added->add_transaction(xact);

    if (xact->amount.commodity().symbol.empty())
      xact->amount.set_commodity(m_xact->amount.commodity());

    m_xact = matching->transactions.back();

    xact = new transaction_t(m_xact->account, - first->amount);
    added->add_transaction(xact);

    if (i != end) {
      account_t * acct = journal.find_account_re(*i);
      if (! acct)
	acct = journal.find_account(*i);
      if (acct)
	added->transactions.back()->account = acct;
    }
  } else {
    while (i != end) {
      std::string&  re_pat(*i++);
      account_t *   acct  = NULL;
      commodity_t * cmdty = NULL;

      if (matching) {
	mask_t acct_regex(re_pat);

	for (transactions_list::const_iterator x
	       = matching->transactions.begin();
	     x != matching->transactions.end();
	     x++) {
	  if (acct_regex.match((*x)->account->fullname())) {
	    acct  = (*x)->account;
	    cmdty = &(*x)->amount.commodity();
	    break;
	  }
	}
      }

      if (! acct)
	acct = journal.find_account_re(re_pat);
      if (! acct)
	acct = journal.find_account(re_pat);

      if (i == end) {
	added->add_transaction(new transaction_t(acct));
	goto done;
      }

      transaction_t * xact = new transaction_t(acct, amount_t(*i++));
      if (cmdty && ! xact->amount.commodity())
	xact->amount.set_commodity(*cmdty);

      added->add_transaction(xact);
    }

    if (! matching) {
      throw error("Could not determine the account to draw from");
    } else {
      transaction_t * xact
	= new transaction_t(matching->transactions.back()->account);
      added->add_transaction(xact);
    }
  }

 done:
  if (! added->finalize() ||
      ! run_hooks(journal.entry_finalize_hooks, *added))
    throw error("Failed to finalize derived entry (check commodities)");

  return added.release();
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <boost/python/detail/api_placeholder.hpp>

using namespace boost::python;
using namespace ledger;

entry_t * py_derive_new_entry(journal_t& journal, list args)
{
  strings_list strs;

  int l = len(args);
  for (int i = 0; i < l; i++)
    strs.push_back(extract<std::string>(args[i]));

  return derive_new_entry(journal, strs.begin(), strs.end());
}

void export_derive()
{
  def("derive_new_entry", py_derive_new_entry,
      return_value_policy<manage_new_object>());
}

#endif // USE_BOOST_PYTHON
