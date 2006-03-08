#ifndef _ERROR_H
#define _ERROR_H

#include <exception>
#include <string>
#include <cstring>
#include <sstream>
#include <list>

class error_context
{
 public:
  std::string desc;

  error_context(const std::string& _desc) throw() : desc(_desc) {}
  virtual ~error_context() throw() {}
  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << std::endl;
  }
};

class file_context : public error_context
{
 protected:
  std::string   file;
  unsigned long line;
 public:
  file_context(const std::string& _file, unsigned long _line,
	       const std::string& desc = "") throw()
    : file(_file), line(_line), error_context(desc) {}
  virtual ~file_context() throw() {}

  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << " ";

    out << "\"" << file << "\", line " << line << ": ";
  }
};

namespace ledger { class value_t; }
class value_context : public error_context
{
  ledger::value_t * bal;
 public:
  value_context(const ledger::value_t& _bal,
		  const std::string& desc = "") throw();
  virtual ~value_context() throw();

  virtual void describe(std::ostream& out) const throw();
};

namespace ledger { class value_expr_t; }
class valexpr_context : public error_context {
 public:
  const ledger::value_expr_t * expr;
  const ledger::value_expr_t * error_node;

  valexpr_context(const ledger::value_expr_t * _expr,
		  const std::string& desc = "") throw();
  virtual ~valexpr_context() throw();

  virtual void describe(std::ostream& out) const throw();
};

class line_context : public error_context {
 public:
  std::string line;
  long	      pos;

  line_context(const std::string& _line, long _pos,
	       const std::string& desc = "") throw()
    : line(_line), pos(_pos), error_context(desc) {}
  virtual ~line_context() throw() {}

  virtual void describe(std::ostream& out) const throw();
};

class include_context : public file_context {
 public:
  include_context(const std::string& file, unsigned long line,
		  const std::string& desc = "") throw()
    : file_context(file, line, desc) {}
  virtual ~include_context() throw() {}

  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << ": ";
    out << "\"" << file << "\", line " << line << ":" << std::endl;
  }
};

namespace ledger { class entry_base_t; }
class entry_context : public error_context {
 public:
  const ledger::entry_base_t& entry;

  entry_context(const ledger::entry_base_t& _entry,
		const std::string& desc = "") throw()
    : entry(_entry), error_context(desc) {}
  virtual ~entry_context() throw() {}

  virtual void describe(std::ostream& out) const throw();
};

namespace ledger { class transaction_t; }
class xact_context : public file_context {
 public:
  const ledger::transaction_t& xact;

  xact_context(const ledger::transaction_t& _xact,
	       const std::string& desc = "") throw();
  virtual ~xact_context() throw() {}
};

//////////////////////////////////////////////////////////////////////

class str_exception : public std::exception {
 protected:
  std::string reason;
 public:
  std::list<error_context *> context;

  str_exception(const std::string& _reason,
		error_context * ctxt = NULL) throw()
    : reason(_reason) {
    if (ctxt)
      context.push_back(ctxt);
  }

  virtual ~str_exception() throw() {
    for (std::list<error_context *>::iterator i = context.begin();
	 i != context.end();
	 i++)
      delete *i;
  }

  virtual void reveal_context(std::ostream& out,
			      const std::string& kind) const throw() {
    for (std::list<error_context *>::const_reverse_iterator i =
	   context.rbegin();
	 i != context.rend();
	 i++) {
      std::list<error_context *>::const_reverse_iterator x = i;
      if (++x == context.rend())
	out << kind << ": ";
      (*i)->describe(out);
    }
  }

  virtual const char* what() const throw() {
    return reason.c_str();
  }
};

class error : public str_exception {
 public:
  error(const std::string& reason, error_context * ctxt = NULL) throw()
    : str_exception(reason, ctxt) {}
  virtual ~error() throw() {}
};

class fatal : public str_exception {
 public:
  fatal(const std::string& reason, error_context * ctxt = NULL) throw()
    : str_exception(reason, ctxt) {}
  virtual ~fatal() throw() {}
};

class fatal_assert : public fatal {
 public:
  fatal_assert(const std::string& reason, error_context * ctxt = NULL) throw()
    : fatal(std::string("assertion failed '") + reason + "'", ctxt) {}
  virtual ~fatal_assert() throw() {}
};

namespace ledger {

class compute_error : public error {
 public:
  compute_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~compute_error() throw() {}
};

class value_expr_error : public error {
 public:
  value_expr_error(const std::string& reason,
		   error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~value_expr_error() throw() {}
};

class interval_expr_error : public error {
 public:
  interval_expr_error(const std::string& reason,
		      error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~interval_expr_error() throw() {}
};

class format_error : public error {
 public:
  format_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~format_error() throw() {}
};

class parse_error : public error {
 public:
  parse_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~parse_error() throw() {}
};

class value_error : public error {
 public:
  value_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~value_error() throw() {}
};

class balance_error : public error {
 public:
  balance_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~balance_error() throw() {}
};

} // namespace ledger

#endif // _ERROR_H
