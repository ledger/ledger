#ifndef _REPITEM_H
#define _REPITEM_H

#include "session.h"
#include "journal.h"
#include "valexpr.h"
#include "format.h"

#include <iostream>
#include <memory>
#include <list>

namespace ledger {

class report_t;

class repitem_t : public valexpr_t::scope_t
{
  repitem_t(const repitem_t&);

 public:
  repitem_t * parent;
  repitem_t * next;
  repitem_t * prev;

  union {
    account_t *	account_ptr;
    journal_t *	journal;
    session_t *	session;
  };

  enum kind_t { UNKNOWN, TRANSACTION, ENTRY, ACCOUNT,
		JOURNAL, SESSION } kind;

  bool istemp; // if this does not refer to a real journal item

  repitem_t * contents;
  repitem_t * last_content;
  repitem_t * children;
  repitem_t * last_child;

  value_t * c_value;		// the calculated value
  value_t * c_sort_value;	// and sort value

  unsigned short flags;
  unsigned int   parents_elided;
  datetime_t	 reported_date;

  repitem_t(kind_t _kind, repitem_t * owner = NULL)
    : valexpr_t::scope_t(owner, false, true), kind(_kind),
      parent(NULL), next(NULL), prev(NULL), istemp(false),
      contents(NULL), last_content(NULL),
      children(NULL), last_child(NULL),
      c_value(NULL), c_sort_value(NULL), flags(0),
      parents_elided(0) {
    TRACE_CTOR("repitem_t(kind_t, repitem_t *)");
    if (owner) {
      if (kind == TRANSACTION)
	owner->add_content(this);
      else
	owner->add_child(this);
    }
  }
  virtual ~repitem_t();

  virtual void clear();

  virtual void add_value(value_t& val);
  virtual void add_sort_value(value_t& val);
  virtual void add_total(value_t& val);

  virtual datetime_t date() const;
  virtual datetime_t effective_date() const;
  virtual datetime_t actual_date() const;

  void date(value_t& result) { result = date(); }
  void edate(value_t& result) { result = effective_date(); }
  void rdate(value_t& result) { result = actual_date(); }

  bool valid() const;

  static repitem_t * wrap(transaction_t * txact, repitem_t * owner = NULL);
  static repitem_t * wrap(entry_t * tentry, repitem_t * owner = NULL,
			  bool deep = true);
  static repitem_t * wrap(account_t * taccount, repitem_t * owner = NULL,
			  bool deep = true);
  static repitem_t * wrap(journal_t * tjournal, repitem_t * owner = NULL,
			  bool deep = true);
  static repitem_t * wrap(session_t * tsession,
			  valexpr_t::scope_t * parent = NULL,
			  bool deep = true);

  repitem_t * add_content(repitem_t * item);
  repitem_t * add_child(repitem_t * item);

  static repitem_t * fake_transaction(account_t * taccount);
  static repitem_t * fake_entry(const datetime_t& edate,
				const datetime_t& rdate,
				const std::string& payee);

  void populate_account(account_t& acct, repitem_t * item);
  void populate_accounts(entries_list& entries);
  void populate_accounts(entries_list& entries,
			 const valexpr_t& filter);

  void print_tree(std::ostream& out, int depth = 0);

  void calc(const std::string& expr, value_t& result) {
    calc(valexpr_t(expr), result);
  }
  void calc(const valexpr_t& expr, value_t& result) {
    expr.calc(result, this);
  }

  bool test(const std::string& expr) {
    return test(valexpr_t(expr));
  }
  bool test(const valexpr_t& expr) {
    value_t truth;
    calc(expr, truth);
    return truth;
  }

  //
  // Node selection
  //

  struct path_element_t
  {
    struct path_element_t * next;

    repitem_t::kind_t kind;
    valexpr_t         valexpr;
    bool              root;
    bool              parent;
    bool              recurse;

    path_element_t()
      : next(NULL), kind(repitem_t::UNKNOWN),
	root(false), parent(false), recurse(false) {
      TRACE_CTOR("path_element_t()");
    }

    ~path_element_t() {
      TRACE_DTOR("path_element_t");
      if (next)
	delete next;
    }

   private:
    path_element_t(const path_element_t&);
    path_element_t& operator=(const path_element_t&);
  };

  struct path_t
  {
    std::list<const path_element_t *> paths;

    path_t() {
      TRACE_CTOR("path_t()");
    }

    ~path_t() {
      TRACE_DTOR("path_t");
      for (std::list<const path_element_t *>::const_iterator
	     i = paths.begin();
	   i != paths.end();
	   i++)
	delete *i;
    }

   private:
    path_t(const path_t&);
    path_t& operator=(const path_t&);
  };

  typedef void (*select_callback_t)(repitem_t * item);

  void select(const std::string& expr, select_callback_t callback) {
    std::auto_ptr<const path_t> path(parse_selector(expr));
    if (path.get())
      select(path.get(), callback);
  }
  void select(const path_t * path, select_callback_t callback);
  void select_all(select_callback_t callback);

  void last(value_t& result);
  void position(value_t& result);

  static const path_t * parse_selector(const std::string& expr);

 private:
  void traverse_selection(const path_element_t * path,
			  select_callback_t callback);

  static const path_element_t * parse_subselector(const char *& p);

 public:
#ifdef DEBUG_ENABLED
  static void dump_path(std::ostream& out, const path_t * path);
#endif

  //
  // Scope members
  //

  virtual bool resolve(const std::string& name, value_t& result,
		       scope_t * locals = NULL);

  //
  // Report item formatter
  //

  struct formatter_t : public format_t::element_formatter_t
  {
    typedef format_t::element_t element_t;

    int write_elements(std::ostream& out, format_t& format,
		       repitem_t * items, bool recursive, bool children,
		       int column) const;

    virtual int operator()(std::ostream& out, element_t * elem,
			   valexpr_t::scope_t * scope, int column) const;
  };
};

class xact_repitem_t : public repitem_t
{
  xact_repitem_t(const xact_repitem_t&);
  xact_repitem_t& operator=(const xact_repitem_t&);

  account_t * reported_account;

 public:
  transaction_t * xact;

  xact_repitem_t(transaction_t * _xact, repitem_t * owner = NULL)
    : repitem_t(TRANSACTION, owner),
      xact(_xact), reported_account(NULL) {
    TRACE_CTOR("xact_repitem_t(transaction_t *, repitem_t *)");
    assert(xact);
  }
  virtual ~xact_repitem_t() {
    TRACE_DTOR("xact_repitem_t");
    if (istemp)
      delete xact;
  }

  virtual void clear() {
    assert(! contents);
    assert(! children);
  }

  virtual void add_value(value_t& val) {
    if (c_value) {
      val += *c_value;
    } else {
      if (xact->cost || ! val.realzero())
	val.add(xact->amount, xact->cost);
      else
	val = xact->amount;
    }
  }

  virtual datetime_t date() const {
    return xact->date();
  }
  virtual datetime_t effective_date() const {
    return xact->effective_date();
  }
  virtual datetime_t actual_date() const {
    return xact->actual_date();
  }

  virtual account_t * account() const {
    if (reported_account != NULL)
      return reported_account;
    else
      return xact->account;
  }

  void payee(value_t& result) {
    if (xact->entry)
      result.set_string(xact->entry->payee);
  }

  void account(value_t& result) {
    account_t * acct = account();
    result.set_string(acct ? acct->fullname() : "");
  }

  virtual bool resolve(const std::string& name, value_t& result,
		       scope_t * locals = NULL);
};

class entry_repitem_t : public repitem_t
{
  entry_repitem_t(const entry_repitem_t&);
  entry_repitem_t& operator=(const entry_repitem_t&);

 public:
  entry_t * entry;

  entry_repitem_t(entry_t * _entry, repitem_t * owner = NULL)
    : repitem_t(ENTRY, owner), entry(_entry) {
    TRACE_CTOR("entry_repitem_t(entry_t *, repitem_t *)");
    assert(entry);
  }
  virtual ~entry_repitem_t() {
    TRACE_DTOR("entry_repitem_t");
    if (istemp)
      delete entry;
  }

  virtual void clear() {
    assert(! children);
    repitem_t::clear();
  }

  virtual datetime_t date() const {
    return entry->date();
  }
  virtual datetime_t effective_date() const {
    return entry->effective_date();
  }
  virtual datetime_t actual_date() const {
    return entry->actual_date();
  }

  void payee(value_t& result) {
    result.set_string(entry->payee);
  }

  virtual bool resolve(const std::string& name, value_t& result,
		       scope_t * locals = NULL);
};

template <typename T>
inline T * get_ptr(valexpr_t::scope_t * locals, int idx) {
  assert(locals->args.size() > idx);
  T * ptr = static_cast<T *>(locals->args[idx].to_pointer());
  assert(ptr);
  return ptr;
}

class format_command : public valexpr_t::functor_t
{
  format_t formatter;

 public:
  format_command(const std::string& command_name,
		 const std::string& format_string)
    : valexpr_t::functor_t(command_name), formatter(format_string) {}

  virtual void operator()(value_t& result, valexpr_t::scope_t * locals);
};

} // namespace ledger

#endif // _REPITEM_H
