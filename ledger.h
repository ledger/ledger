#ifndef _LEDGER_H
#define _LEDGER_H "$Revision: 1.34 $"

//////////////////////////////////////////////////////////////////////
//
// Ledger Accounting Tool
//
//   A command-line tool for general double-entry accounting.
//
// Copyright (c) 2003 John Wiegley <johnw@newartisans.com>
//

#include <iostream>
#include <string>
#include <vector>
#include <list>
#include <map>
#include <ctime>

#ifdef DEBUG
#include <cassert>
#else
#ifdef assert
#undef assert
#endif
#define assert(x)
#endif

#include <pcre.h>               // Perl regular expression library

namespace ledger {

class amount;
class commodity
{
  commodity(const commodity&);

  typedef std::map<const std::time_t, amount *>  price_map;
  typedef std::pair<const std::time_t, amount *> price_map_pair;

 public:
  std::string name;
  std::string symbol;

  mutable bool sought;

  bool prefix;
  bool separate;
  bool thousands;
  bool european;
  int  precision;

 protected:
  mutable price_map history;	// the price history
  mutable amount *  conversion;	// fixed conversion (ignore history)

 public:
  explicit commodity() : sought(false), prefix(false), separate(true),
    thousands(false), european(false), conversion(NULL) {}

  explicit commodity(const std::string& sym, bool pre = false,
		     bool sep = true, bool thou = true,
		     bool euro = false, int prec = 2);
  ~commodity();

  void     set_price(amount * price, std::time_t * when = NULL);
  amount * price(std::time_t * when = NULL, bool download = false) const;
};

typedef std::map<const std::string, commodity *>  commodities_map;
typedef commodities_map::iterator                 commodities_map_iterator;
typedef std::pair<const std::string, commodity *> commodities_map_pair;


class amount
{
 public:
  virtual ~amount() {}

  virtual commodity * commdty() const = 0;
  virtual void set_commdty(commodity *) = 0;

  virtual amount * copy() const = 0;
  virtual amount * value(const amount * pr = NULL) const = 0;
  virtual void set_value(const amount * pr) = 0;
  virtual amount * street(std::time_t * when = NULL,
			  bool get_quotes = false) const = 0;

  virtual bool has_price() const = 0;
  virtual amount * per_item_price() const = 0;

  // Comparison

  virtual bool is_zero() const = 0;
  virtual bool is_negative() const = 0;
  virtual int  compare(const amount * other) const = 0;

  // Assignment

  virtual void credit(const amount * other) = 0;
  virtual void negate() = 0;

  // String conversion routines

  virtual void parse(const std::string& num) = 0;
  virtual const std::string as_str(bool full_prec = false) const = 0;
};

extern amount * create_amount(const std::string& value,
			      const amount * cost = NULL);

class mask
{
 public:
  bool        exclude;
  std::string pattern;
  pcre *      regexp;

  explicit mask(const std::string& pattern);

  mask(const mask&);

  ~mask() {
    pcre_free(regexp);
  }

  bool match(const std::string& str) const;
};

typedef std::list<mask>                 regexps_list;
typedef std::list<mask>::iterator       regexps_list_iterator;
typedef std::list<mask>::const_iterator regexps_list_const_iterator;

class account;
class transaction
{
  transaction(const transaction&);

 public:
  account * acct;
  amount *  cost;

  std::string note;

  bool is_virtual;
  bool must_balance;
  bool specified;

  explicit transaction(account * _acct = NULL, amount * _cost = NULL)
    : acct(_acct), cost(_cost),
      is_virtual(false), must_balance(true), specified(false) {}

  ~transaction() {
    if (cost)
      delete cost;
  }

  const std::string acct_as_str() const;

  void print(std::ostream& out, bool display_quantity = true,
	     bool display_price = true) const;
};


class book;
class entry
{
  entry(const entry&);

 public:
  book * ledger;
  
  std::time_t date;
  std::string code;
  std::string desc;

  bool cleared;

  std::list<transaction *> xacts;

  explicit entry(book * l) : ledger(l), cleared(false) {}

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

  bool matches(const regexps_list& regexps) const;
  bool validate(bool show_unaccounted = false) const;
  bool finalize(bool do_compute = false);

  void print(std::ostream& out, bool shortcut = true) const;
};

struct cmp_entry_date {
  bool operator()(const entry * left, const entry * right) {
    return std::difftime(left->date, right->date) < 0;
  }
};

typedef std::vector<entry *>           entries_list;
typedef entries_list::iterator         entries_list_iterator;
typedef entries_list::reverse_iterator entries_list_reverse_iterator;
typedef entries_list::const_iterator   entries_list_const_iterator;


class totals
{
  totals(const totals&);

 public:
  typedef std::map<commodity *, amount *>  map;
  typedef map::iterator                    iterator;
  typedef map::const_iterator              const_iterator;
  typedef std::pair<commodity *, amount *> pair;

  map amounts;

  totals() {}
  ~totals();

  void credit(const amount * val);
  void credit(const totals& other);

  void negate();

  bool is_zero() const;
  bool is_negative() const;

  void print(std::ostream& out, int width) const;
};


typedef std::map<const std::string, account *>  accounts_map;
typedef accounts_map::iterator                  accounts_map_iterator;
typedef std::pair<const std::string, account *> accounts_map_pair;

class account
{
  account(const account&);

 public:
  account *    parent;

  std::string  name;
#ifdef READ_GNUCASH
  commodity *  comm;           // default commodity for this account
#endif
  totals       balance;        // optional, parse-time computed balance
  int          checked;        // 'balance' uses this for speed's sake
  accounts_map children;

  mutable std::string full_name;

  explicit account() : parent(NULL), checked(0) {}

  explicit account(const std::string& _name,
		   struct account * _parent = NULL)
    : parent(_parent), name(_name), checked(0) {}

  ~account();

  const std::string as_str(const account * stop = NULL) const;
};


class book
{
  book(const book&);

 public:
  typedef std::map<regexps_list *,
		   std::list<transaction *> *> virtual_map;

  typedef std::pair<regexps_list *,
		    std::list<transaction *> *> virtual_map_pair;

  typedef virtual_map::const_iterator virtual_map_iterator;

  commodities_map commodities;
  accounts_map    accounts;
  accounts_map    accounts_cache; // maps full names to accounts
  virtual_map     virtual_mapping;
  entries_list    entries;
  int             current_year;

  book() {}
  ~book();

  template<typename Compare>
  void sort(Compare comp) {
    std::sort(entries.begin(), entries.end(), comp);
  }
  void print(std::ostream& out, regexps_list& regexps,
	     bool shortcut) const;

  account * re_find_account(const std::string& regex);
  account * find_account(const std::string& name, bool create = true);
};

extern book * main_ledger;

inline commodity::commodity(const std::string& sym, bool pre, bool sep,
			    bool thou, bool euro, int prec)
  : symbol(sym), sought(false), prefix(pre), separate(sep),
    thousands(thou), european(euro), precision(prec), conversion(NULL) {
#ifdef DEBUG
  std::pair<commodities_map_iterator, bool> result =
#endif
    main_ledger->commodities.insert(commodities_map_pair(sym, this));
#ifdef DEBUG
  assert(result.second);
#endif
}

// Parsing routines

extern int parse_ledger(book * ledger, std::istream& in,
			regexps_list& regexps,
			bool compute_balances = false,
			const char * acct_prefix = NULL);
#ifdef READ_GNUCASH
extern book * parse_gnucash(std::istream& in, bool compute_balances = false);
#endif

extern int parse_ledger_file(book * ledger, const std::string& file,
			     regexps_list& regexps,
			     bool compute_balances = false,
			     const char * acct_prefix = NULL);

extern bool parse_date_mask(const char * date_str,
			    struct std::tm * result);
extern bool parse_date(const char * date_str, std::time_t * result,
		       const int year = -1);

extern void record_regexp(const std::string& pattern, regexps_list& regexps);
extern void read_regexps(const std::string& path, regexps_list& regexps);
extern bool matches(const regexps_list& regexps, const std::string& str,
		    bool * by_exclusion = NULL);

extern void read_prices(const std::string& path);
extern void parse_price_setting(const std::string& setting);

} // namespace ledger

#endif // _LEDGER_H
