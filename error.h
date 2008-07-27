#ifndef _ERROR_H
#define _ERROR_H

#include <exception>
#include <stdexcept>
#include <string>
#include <cstring>
#include <sstream>
#include <list>

namespace ledger {

class error_context
{
public:
  string desc;

  error_context(const string& _desc) throw() : desc(_desc) {}
  virtual ~error_context() throw() {}
  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << std::endl;
  }
};

class file_context : public error_context
{
 protected:
  path		file;
  unsigned long line;
 public:
  file_context(const path& _file, unsigned long _line,
	       const string& desc = "") throw()
    : error_context(desc), file(_file), line(_line) {}
  virtual ~file_context() throw() {}

  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << " ";

    out << "\"" << file << "\", line " << line << ": ";
  }
};

class line_context : public error_context
{
public:
  string line;
  long	      pos;

  line_context(const string& _line, long _pos,
	       const string& desc = "") throw()
    : error_context(desc), line(_line), pos(_pos) {}
  virtual ~line_context() throw() {}

  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << std::endl;

    out << "  " << line << std::endl << "  ";
    long idx = pos < 0 ? line.length() - 1 : pos;
    for (int i = 0; i < idx; i++)
      out << " ";
    out << "^" << std::endl;
  }
};

//////////////////////////////////////////////////////////////////////

class str_exception : public std::logic_error
{
public:
  std::list<error_context *> context;

  str_exception(const string& why,
		error_context * ctxt = NULL) throw()
    : std::logic_error(why) {
    if (ctxt)
      context.push_back(ctxt);
  }

  virtual ~str_exception() throw() {
    for (std::list<error_context *>::iterator i = context.begin();
	 i != context.end();
	 i++)
      checked_delete(*i);
  }

  virtual void reveal_context(std::ostream& out,
			      const string& kind) const throw() {
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
};

#define DECLARE_EXCEPTION(kind, name)					\
  class name : public kind {						\
  public:								\
    name(const string& why, error_context * ctxt = NULL) throw()	\
      : kind(why, ctxt) {}						\
  }

class error : public str_exception {
 public:
  error(const string& why, error_context * ctxt = NULL) throw()
    : str_exception(why, ctxt) {}
  virtual ~error() throw() {}
};

class fatal : public str_exception {
 public:
  fatal(const string& why, error_context * ctxt = NULL) throw()
    : str_exception(why, ctxt) {}
  virtual ~fatal() throw() {}
};

class fatal_assert : public fatal {
 public:
  fatal_assert(const string& why, error_context * ctxt = NULL) throw()
    : fatal(string("assertion failed '") + why + "'", ctxt) {}
  virtual ~fatal_assert() throw() {}
};

} // namespace ledger

#endif // _ERROR_H
