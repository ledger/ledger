#ifndef _LEDGER_H
#define _LEDGER_H

//////////////////////////////////////////////////////////////////////
//
// Ledger Accounting Tool
//
//   A command-line tool for general double-entry accounting.
//
// Copyright (c) 2003,2004 John Wiegley <johnw@newartisans.com>
//

#include <map>
#include <list>
#include <string>
#include <ctime>
#include <cctype>
#include <iostream>
#include <sstream>

#ifdef DEBUG
#include <cassert>
#else
#ifdef assert
#undef assert
#endif
#define assert(x)
#endif

namespace ledger {

extern const std::string version;

class commodity_t;
class amount_t;
class transaction_t;
class entry_t;
class account_t;
class ledger_t;

class amount_t
{
  typedef void * base_type;

  void _init();
  void _copy(const amount_t& amt);
  void _clear();

 public:
  base_type	quantity;	// amount, to MAX_PRECISION
  commodity_t *	commodity;

  static commodity_t * null_commodity;

  bool valid() const {
    if (quantity)
      return commodity != NULL;
    else
      return commodity == NULL;
  }

  // constructors
  amount_t(commodity_t * _commodity = NULL)
    : quantity(NULL), commodity(_commodity) {}

  amount_t(const amount_t& amt) : quantity(NULL) {
    if (amt.quantity)
      _copy(amt);
    else
      commodity = amt.commodity;
  }
  amount_t(const std::string& value) {
    _init();
    std::istringstream str(value);
    str >> *this;
  }
  amount_t(const int value) : quantity(NULL), commodity(NULL) {
    if (value != 0) {
      std::string str;
      std::ostringstream strstr(str);
      strstr << value;
      parse(strstr.str());
    }
  }
  amount_t(const unsigned int value) : quantity(NULL), commodity(NULL) {
    if (value != 0) {
      std::string str;
      std::ostringstream strstr(str);
      strstr << value;
      parse(strstr.str());
    }
  }
  amount_t(const double value) : quantity(NULL), commodity(NULL) {
    if (value != 0.0) {
      std::string str;
      std::ostringstream strstr(str);
      strstr << value;
      parse(strstr.str());
    }
  }

  // destructor
  ~amount_t() {
    if (quantity)
      _clear();
  }

  // assignment operator
  amount_t& operator=(const amount_t& amt);
  amount_t& operator=(const std::string& value);
  amount_t& operator=(const int value);
  amount_t& operator=(const unsigned int value);
  amount_t& operator=(const double value);

  // general methods
  amount_t round(int precision = -1) const;

  // in-place arithmetic
  amount_t& operator*=(const amount_t& amt);
  amount_t& operator/=(const amount_t& amt);
  amount_t& operator%=(const amount_t& amt);
  amount_t& operator+=(const amount_t& amt);
  amount_t& operator-=(const amount_t& amt);

  // simple arithmetic
  amount_t operator*(const amount_t& amt) const {
    amount_t temp = *this;
    temp *= amt;
    return temp;
  }
  amount_t operator/(const amount_t& amt) const {
    amount_t temp = *this;
    temp /= amt;
    return temp;
  }
  amount_t operator%(const amount_t& amt) const {
    amount_t temp = *this;
    temp %= amt;
    return temp;
  }
  amount_t operator+(const amount_t& amt) const {
    amount_t temp = *this;
    temp += amt;
    return temp;
  }
  amount_t operator-(const amount_t& amt) const {
    amount_t temp = *this;
    temp -= amt;
    return temp;
  }

  // unary negation
  amount_t& negate();
  amount_t negated() const {
    amount_t temp = *this;
    temp.negate();
    return temp;
  }
  amount_t operator-() const {
    return negated();
  }

  // test for non-zero (use ! for zero)
  operator bool() const;

  // comparisons to zero
  bool operator<(const int num) const;
  bool operator<=(const int num) const;
  bool operator>(const int num) const;
  bool operator>=(const int num) const;

  // comparisons between amounts
  bool operator<(const amount_t& amt) const;
  bool operator<=(const amount_t& amt) const;
  bool operator>(const amount_t& amt) const;
  bool operator>=(const amount_t& amt) const;
  bool operator==(const amount_t& amt) const;
  bool operator!=(const amount_t& amt) const {
    if (commodity != amt.commodity)
      return true;
    return ! (*this == amt);
  }

  amount_t value(const std::time_t moment) const;

  operator std::string() const;

  void parse(std::istream& in, ledger_t * ledger = NULL);
  void parse(const std::string& str, ledger_t * ledger = NULL) {
    std::istringstream stream(str);
    parse(stream, ledger);
  }

  void write_quantity(std::ostream& out) const;
  void read_quantity(std::istream& in);

  friend std::istream& operator>>(std::istream& in, amount_t& amt);
};

void parse_quantity(std::istream& in, std::string& value);
void parse_commodity(std::istream& in, std::string& symbol);

inline amount_t abs(const amount_t& amt) {
  return amt < 0 ? amt.negated() : amt;
}

inline std::istream& operator>>(std::istream& in, amount_t& amt) {
  amt.parse(in);
  return in;
}

std::ostream& operator<<(std::ostream& out, const amount_t& amt);


#define COMMODITY_STYLE_DEFAULTS   0x00
#define COMMODITY_STYLE_SUFFIXED   0x01
#define COMMODITY_STYLE_SEPARATED  0x02
#define COMMODITY_STYLE_EUROPEAN   0x04
#define COMMODITY_STYLE_THOUSANDS  0x08
#define COMMODITY_STYLE_CONSULTED  0x10
#define COMMODITY_STYLE_NOMARKET   0x20

typedef std::map<const std::time_t, amount_t>  history_map;
typedef std::pair<const std::time_t, amount_t> history_pair;

class commodity_t
{
 public:
  std::string	symbol;
  std::string	name;
  std::string	note;
  unsigned int	precision;
  unsigned int	flags;
  history_map	history;
  amount_t	conversion;
  unsigned long	ident;

  static void (*updater)(commodity_t *	   commodity,
			 const std::time_t date,
			 const amount_t&   price,
			 const std::time_t moment);

  commodity_t(const std::string& _symbol    = "",
	      unsigned int	 _precision = 2,
	      unsigned int       _flags	    = COMMODITY_STYLE_DEFAULTS)
    : symbol(_symbol), precision(_precision), flags(_flags) {}

  void add_price(const std::time_t date, const amount_t& price) {
    history.insert(history_pair(date, price));
  }
  bool remove_price(const std::time_t date) {
    history_map::size_type n = history.erase(date);
    return n > 0;
  }

  void set_conversion(const amount_t& price) {
    conversion = price;
  }

  amount_t value(const std::time_t moment = std::time(NULL));
};


#define TRANSACTION_NORMAL   0x0
#define TRANSACTION_VIRTUAL  0x1
#define TRANSACTION_BALANCE  0x2

class transaction_t
{
 public:
  entry_t *	entry;
  account_t *	account;
  amount_t	amount;
  amount_t	cost;
  unsigned int	flags;
  std::string	note;

  transaction_t(entry_t * _entry, account_t * _account)
    : entry(_entry), account(_account), flags(TRANSACTION_NORMAL) {}

  transaction_t(entry_t *	   _entry,
		account_t *	   _account,
		const amount_t&    _amount,
		const amount_t&    _cost,
		unsigned int	   _flags = TRANSACTION_NORMAL,
		const std::string& _note  = "")
    : entry(_entry), account(_account), amount(_amount),
    cost(_cost), flags(_flags), note(_note) {}
};


typedef std::list<transaction_t *> transactions_list;

class entry_t
{
 public:
  enum entry_state_t {
    UNCLEARED, CLEARED, PENDING
  };

  std::time_t	     date;
  enum entry_state_t state;
  std::string	     code;
  std::string	     payee;
  transactions_list  transactions;

  ~entry_t() {
    for (transactions_list::iterator i = transactions.begin();
	 i != transactions.end();
	 i++)
      delete *i;
  }

  void add_transaction(transaction_t * xact) {
    transactions.push_back(xact);
  }
  bool remove_transaction(transaction_t * xact) {
    transactions.remove(xact);
    return true;
  }
};


typedef std::map<const std::string, account_t *> accounts_map;
typedef std::pair<const std::string, account_t *> accounts_pair;

inline std::ostream& operator<<(std::ostream& out, const account_t& acct);

class account_t
{
 public:
  const account_t *	parent;
  std::string		name;
  std::string		note;
  accounts_map		accounts;
  mutable accounts_map  accounts_cache;
  transactions_list	transactions;
  unsigned long	        ident;
  static unsigned long  next_ident;

  account_t(const account_t * _parent, const std::string& _name = "",
	    const std::string& _note = "")
    : parent(_parent), name(_name), note(_note) {}

  ~account_t();

  std::string fullname() const;

  void add_account(account_t * acct) {
    acct->ident = next_ident++;
    accounts.insert(accounts_pair(acct->name, acct));
  }
  bool remove_account(account_t * acct) {
    accounts_map::size_type n = accounts.erase(acct->name);
    return n > 0;
  }

  account_t * find_account(const std::string& name, bool auto_create = true);

  operator std::string() const {
    return fullname();
  }

  // These functions should only be called from ledger_t::add_entry
  // and ledger_t::remove_entry; or from the various parsers.
  void add_transaction(transaction_t * xact) {
    transactions.push_back(xact);
  }
  bool remove_transaction(transaction_t * xact);

  friend class ledger_t;
};

inline std::ostream& operator<<(std::ostream& out, const account_t& acct) {
  out << acct.fullname();
  return out;
}


typedef std::map<const std::string, commodity_t *>  commodities_map;
typedef std::pair<const std::string, commodity_t *> commodities_pair;

typedef std::list<entry_t *> entries_list;

class ledger_t
{
 public:
  account_t *		 master;
  commodities_map	 commodities;
  entries_list		 entries;
  std::list<std::string> sources;

  ledger_t() {
    master = new account_t(NULL, "");
    master->ident = 0;
    account_t::next_ident = 1;
  }

  ~ledger_t();

  void add_account(account_t * acct) {
    master->add_account(acct);
  }
  bool remove_account(account_t * acct) {
    return master->remove_account(acct);
  }

  account_t * find_account(const std::string& name, bool auto_create = true) {
    return master->find_account(name, auto_create);
  }

  void add_commodity(commodity_t * commodity, const std::string symbol = "") {
    commodities.insert(commodities_pair(symbol.empty() ?
					commodity->symbol : symbol, commodity));
  }
  bool remove_commodity(commodity_t * commodity) {
    commodities_map::size_type n = commodities.erase(commodity->symbol);
    return n > 0;
  }

  commodity_t * find_commodity(const std::string& symbol,
			       bool auto_create = false);

  bool add_entry(entry_t * entry);
  bool remove_entry(entry_t * entry);
};

int parse_ledger_file(char * p, ledger_t * book);

} // namespace ledger

#endif // _LEDGER_H
