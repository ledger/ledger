#ifndef _LEDGER_H
#define _LEDGER_H "$Revision: 1.3 $"

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
#include <cassert>

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

  int precision;

  commodity() : prefix(false), separate(true) {}
  commodity(const std::string& sym, bool pre, bool sep, int prec);
};

typedef std::map<const std::string, commodity *> commodities_t;
typedef commodities_t::iterator commodities_iterator;
typedef std::pair<const std::string, commodity *> commodities_entry;

extern commodities_t commodities;
extern commodity *   commodity_usd;

inline commodity::commodity(const std::string& sym,
			    bool pre, bool sep, int prec)
  : symbol(sym), prefix(pre), separate(sep), precision(prec) {
  std::pair<commodities_iterator, bool> result =
    commodities.insert(commodities_entry(sym, this));
  assert(result.second);
}


class amount
{
 public:
  virtual ~amount() {}

  virtual const std::string& comm_symbol() const = 0;
  virtual amount * copy() const = 0;
  virtual amount * value() const = 0;

  // Test if non-zero

  virtual operator bool() const = 0;

  // Assignment

  virtual void credit(const amount * other) = 0;
  virtual void operator+=(const amount& other) = 0;

  // String conversion routines

  virtual void parse(const char * num) = 0;
  virtual amount& operator=(const char * num) = 0;
  virtual operator std::string() const = 0;
};

template<class Traits>
std::basic_ostream<char, Traits> &
operator<<(std::basic_ostream<char, Traits>& out, const amount& a) {
  return (out << std::string(a));
}

extern amount * create_amount(const char * value, const amount * price = NULL);

struct account;
struct transaction
{
  account * acct;
  amount *  cost;

  std::string note;

  transaction() : acct(NULL), cost(NULL) {}

  ~transaction() {
    if (cost)
      delete cost;
  }
};

struct entry
{
  std::time_t date;
  std::string code;
  std::string desc;

  bool cleared;

  std::list<transaction *> xacts;

  entry() : cleared(false) {}
  ~entry() {
    for (std::list<transaction *>::iterator i = xacts.begin();
	 i != xacts.end();
	 i++) {
      delete *i;
    }
  }

  void print(std::ostream& out) const;
  bool validate() const;
};

struct cmp_entry_date {
  bool operator()(const entry * left, const entry * right) {
    return std::difftime(left->date, right->date) < 0;
  }
};

typedef std::vector<entry *> ledger_t;
typedef ledger_t::iterator ledger_iterator;

extern ledger_t ledger;


class totals
{
  typedef std::map<const std::string, amount *> map_t;
  typedef map_t::iterator iterator_t;
  typedef map_t::const_iterator const_iterator_t;
  typedef std::pair<const std::string, amount *> pair_t;

  map_t amounts;

 public:
  void credit(const amount * val) {
    std::pair<iterator_t, bool> result =
      amounts.insert(pair_t(val->comm_symbol(), val->copy()));
    if (! result.second)
      amounts[val->comm_symbol()]->credit(val);
  }
  void credit(const totals& other);

  operator bool() const;

  void print(std::ostream& out) const;

  // Returns an allocated entity
  amount * value(const std::string& comm);
  amount * sum(const std::string& comm) {
    return amounts[comm];
  }
};

template<class Traits>
std::basic_ostream<char, Traits> &
operator<<(std::basic_ostream<char, Traits>& out, const totals& t) {
  t.print(out);
  return out;
}


struct account
{
  std::string name;
  commodity * comm;             // default commodity for this account

  struct account * parent;

  typedef std::map<const std::string, struct account *> map;
  typedef map::iterator iterator;
  typedef map::const_iterator const_iterator;
  typedef std::pair<const std::string, struct account *> pair;

  map children;

  account(const std::string& _name, struct account * _parent = NULL)
    : name(_name), parent(_parent) {}

  operator std::string() const {
    if (! parent)
      return name;
    else
      return std::string(*parent) + ":" + name;
  }
};

template<class Traits>
std::basic_ostream<char, Traits> &
operator<<(std::basic_ostream<char, Traits>& out, const account& a) {
  return (out << std::string(a));
}


typedef std::map<const std::string, account *> accounts_t;
typedef accounts_t::iterator accounts_iterator;
typedef std::pair<const std::string, account *> accounts_entry;

extern accounts_t accounts;

} // namespace ledger

#endif // _LEDGER_H
