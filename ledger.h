#ifndef _LEDGER_H
#define _LEDGER_H "$Revision: 1.16 $"

//////////////////////////////////////////////////////////////////////
//
// ledger: Double-entry ledger accounting
//
//   by John Wiegley <johnw@newartisans.com>
//
// Copyright (c) 2003 New Artisans, Inc.  All Rights Reserved.

#include <iostream>
#include <string>
#include <vector>
#include <list>
#include <map>
#include <ctime>

#ifdef DEBUG
#include <cassert>
#else
#define assert(x)
#endif

#include <pcre.h>               // Perl regular expression library

namespace ledger {

// Format of a ledger entry (GNUcash account files are also supported):
//
// DATE [CLEARED] (CODE) DESCRIPTION
//   ACCOUNT AMOUNT [; NOTE]
//   ACCOUNT AMOUNT [; NOTE]
//   ...
//
// The DATE can be YYYY.MM.DD or YYYY/MM/DD or MM/DD.
// The CLEARED bit is a '*' if the account has been cleared.
// The CODE can be anything, but must be enclosed in parenthesis.
// The DESCRIPTION can be anything, up to a newline.
//
// The ACCOUNT is a colon-separated string naming the account.
// The AMOUNT follows the form:
//   [COMM][WS]QUANTITY[WS][COMM][[WS]@[WS][COMM]PRICE[COMM]]
// For example:
//   200 AAPL @ $40.00
//   $50.00
//   DM 12.54
//   DM 12.54 @ $1.20
// The NOTE can be anything.
//
// All entries must balance to 0.0, in every commodity.  This means
// that a transaction with mixed commodities must balance by
// converting one of those commodities to the other.  As a
// convenience, this is done automatically for you in the case where
// exactly two commodities are referred to, in which case the second
// commodity is converted into the first by computing which the price
// must have been in order to balance the transaction.  Example:
//
//   2004.06.18 * (BUY)  Apple Computer
//     Assets:Brokerage     $-200.00
//     Assets:Brokerage     100 AAPL
//
// What this transaction says is that $200 was paid from the
// brokerage account to buy 100 shares of Apple stock, and then place
// those same shares back in the brokerage account.  From this point
// forward, the account "Assets:Brokerage" will have two balance
// totals: The number of dollars in the account, and the number of
// apple shares.
//     In terms of the transaction, however, it must balance to zero,
// otherwise it would mean that something had been lost without
// accouting for it.  So in this case what ledger will do is divide
// 100 by $200, to arrive at a per-share price of $2 for the APPL
// stock, and it will read this transaction as if it had been
// written:
//
//   2004.06.18 * (BUY)  Apple Computer
//     Assets:Brokerage     $-200
//     Assets:Brokerage     100 AAPL @ $2
//
// If you then wanted to give some of the shares to someone, in
// exchange for services rendered, use the regular single-commodity
// form of transaction:
//
//   2004.07.11 *  A kick-back for the broker
//     Assets:Brokerage        -10 AAPL
//     Expenses:Broker's Fees   10 AAPL
//
// This transaction does not need to know the price of AAPL on the
// given day, because none of the shares are being converted to
// another commodity.  It simply directly affects the total number of
// AAPL shares held in "Assets:Brokerage".

struct commodity
{
  std::string name;
  std::string symbol;

  bool prefix;
  bool separate;
  bool thousands;
  bool european;

  int precision;

  commodity() : prefix(false), separate(true),
       thousands(false), european(false) {}
  commodity(const std::string& sym, bool pre = false, bool sep = true,
	    bool thou = true, bool euro = false, int prec = 2);
};

typedef std::map<const std::string, commodity *> commodities_t;
typedef commodities_t::iterator commodities_iterator;
typedef std::pair<const std::string, commodity *> commodities_entry;


class amount
{
 public:
  virtual ~amount() {}

  virtual commodity * comm() const = 0;
  virtual const std::string& comm_symbol() const = 0;
  virtual amount * copy() const = 0;
  virtual amount * value(amount * pr = NULL) const = 0;
  virtual amount * street() const = 0;

  virtual bool has_price() const = 0;
  virtual void set_value(const amount * pr) = 0;

  // Test if non-zero

  virtual operator bool() const = 0;

  // Assignment

  virtual void credit(const amount * other) = 0;
  virtual void negate() = 0;

  // String conversion routines

  virtual void parse(const char * num) = 0;
  virtual std::string as_str(bool full_prec = false) const = 0;
};

extern amount * create_amount(const char * value,
			      const amount * cost = NULL);

struct mask
{
  bool   exclude;
  pcre * regexp;

  mask(bool exc, pcre * re) : exclude(exc), regexp(re) {}
};

extern std::list<mask> regexps;

extern void record_regexp(char * pattern, std::list<mask>& regexps);
extern void read_regexps(const char * path, std::list<mask>& regexps);
extern bool matches(const std::list<mask>& regexps,
		    const std::string& str, bool * exclude = NULL);


struct account;
struct transaction
{
  account * acct;
  amount *  cost;

  std::string note;
#ifdef HUQUQULLAH
  bool exempt_or_necessary;
#endif

  transaction(account * _acct = NULL, amount * _cost = NULL)
    : acct(_acct), cost(_cost) {
#ifdef HUQUQULLAH
    exempt_or_necessary = false;
#endif
  }

#ifdef DO_CLEANUP
  ~transaction() {
    if (cost)
      delete cost;
  }
#endif
};

struct entry
{
  std::time_t date;
  std::string code;
  std::string desc;

  bool cleared;

  std::list<transaction *> xacts;

  entry() : cleared(false) {}

#ifdef DO_CLEANUP
  // If we're running as a command-line tool, it's cheaper to just
  // throw away the heap on exit, than spend time freeing things up
  // like a good citizen.

  ~entry() {
    for (std::list<transaction *>::iterator i = xacts.begin();
	 i != xacts.end();
	 i++) {
      delete *i;
    }
  }
#endif

  bool matches(const std::list<mask>& regexps) const;
  void print(std::ostream& out, bool shortcut = true) const;
  bool validate(bool show_unaccounted = false) const;
};

struct cmp_entry_date {
  bool operator()(const entry * left, const entry * right) {
    return std::difftime(left->date, right->date) < 0;
  }
};

typedef std::vector<entry *> entries_t;
typedef entries_t::iterator entries_iterator;


struct totals
{
  typedef std::map<const std::string, amount *> map;
  typedef map::iterator iterator;
  typedef map::const_iterator const_iterator;
  typedef std::pair<const std::string, amount *> pair;

  map amounts;

#ifdef DO_CLEANUP
  ~totals();
#endif

  void credit(const amount * val) {
    std::pair<iterator, bool> result =
      amounts.insert(pair(val->comm_symbol(), val->copy()));
    if (! result.second)
      amounts[val->comm_symbol()]->credit(val);
  }
  void credit(const totals& other);

  operator bool() const;

  void print(std::ostream& out, int width) const;

  // Returns an allocated entity
  amount * sum(const std::string& comm) {
    return amounts[comm];
  }
};

template<class Traits>
std::basic_ostream<char, Traits> &
operator<<(std::basic_ostream<char, Traits>& out, const totals& t) {
  t.print(out, 20);
  return out;
}


typedef std::map<const std::string, account *> accounts_t;
typedef accounts_t::iterator accounts_iterator;
typedef std::pair<const std::string, account *> accounts_entry;

struct account
{
  account * parent;

  std::string name;
  commodity * comm;             // default commodity for this account
  totals      balance;

  int  checked;
#ifdef HUQUQULLAH
  bool exempt_or_necessary;
#endif

  accounts_t children;

  account() : parent(NULL), checked(0) {
#ifdef HUQUQULLAH
    exempt_or_necessary = false;
#endif
  }
  account(const std::string& _name, struct account * _parent = NULL)
    : parent(_parent), name(_name), checked(0) {
#ifdef HUQUQULLAH
    exempt_or_necessary = false;
#endif
  }

  const std::string as_str() const {
    if (! parent)
      return name;
    else
      return parent->as_str() + ":" + name;
  }
};

template<class Traits>
std::basic_ostream<char, Traits> &
operator<<(std::basic_ostream<char, Traits>& out, const account& a) {
  return (out << a.as_str());
}


struct state
{
  commodities_t commodities;
  accounts_t    accounts;
  accounts_t    accounts_cache; // maps full names to accounts
  entries_t     entries;
  totals        prices;

#ifdef HUQUQULLAH
  bool            compute_huquq;
  std::list<mask> huquq_categories;
  amount *        huquq;
  commodity *     huquq_commodity;
  account *       huquq_account;
  account *       huquq_expenses_account;

  state() : compute_huquq(false) {}
#endif

#ifdef DO_CLEANUP
  ~state();
#endif

  void record_price(const char * setting);
  account * find_account(const char * name, bool create = true);
};

extern state main_ledger;
extern bool  use_warnings;

inline commodity::commodity(const std::string& sym, bool pre, bool sep,
			    bool thou, bool euro, int prec)
  : symbol(sym), prefix(pre), separate(sep),
    thousands(thou), european(euro), precision(prec) {
  std::pair<commodities_iterator, bool> result =
    main_ledger.commodities.insert(commodities_entry(sym, this));
  assert(result.second);
}

} // namespace ledger

#endif // _LEDGER_H
