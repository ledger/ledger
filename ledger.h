#ifndef _LEDGER_H
#define _LEDGER_H "$Revision: 1.17 $"

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

  // Test if the quantity is zero

  virtual bool is_zero() const = 0;

  // Assignment

  virtual void credit(const amount * other) = 0;
  virtual void negate() = 0;

  // String conversion routines

  virtual void parse(const std::string& num) = 0;
  virtual const std::string as_str(bool full_prec = false) const = 0;
};

extern amount * create_amount(const std::string& value,
			      const amount * cost = NULL);

struct mask
{
  bool        exclude;
  std::string pattern;
  pcre *      regexp;

  mask(const std::string& pattern);
};

typedef std::list<mask> regexps_t;

void record_regexp(const std::string& pattern, regexps_t& regexps);
void read_regexps(const std::string& path, regexps_t& regexps);
bool matches(const regexps_t& regexps, const std::string& str,
	     bool * by_exclusion = NULL);


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

  bool is_zero() const;

  void print(std::ostream& out, int width) const;

  // Returns an allocated entity
  amount * sum(const std::string& comm) {
    return amounts[comm];
  }
};


typedef std::map<const std::string, account *> accounts_t;
typedef accounts_t::iterator accounts_iterator;
typedef std::pair<const std::string, account *> accounts_entry;

struct account
{
  account * parent;

  std::string name;
#ifdef READ_GNUCASH
  commodity * comm;             // default commodity for this account
#endif
  totals      balance;          // optional, parse-time computed balance

  mutable std::string full_name;

  int  checked;                 // 'balance' uses this for speed's sake
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
    else if (full_name.empty())
      full_name = parent->as_str() + ":" + name;

    return full_name;
  }
};


struct state
{
  commodities_t   commodities;
  accounts_t      accounts;
  accounts_t      accounts_cache; // maps full names to accounts
  entries_t       entries;
  totals          prices;

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

  void record_price(const std::string& setting);
  account * find_account(const std::string& name, bool create = true);
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
