#ifndef _ERROR_H
#define _ERROR_H

#import "context.h"

namespace ledger {

class exception : public std::exception
{
protected:
  string reason;

public:
  std::list<context> context_stack;

  exception(const string&  _reason,
	    const context& immediate_ctxt) throw()
    : reason(_reason) {
    push(immediate_ctxt);
  }

  void push(const context& intermediate_ctxt) throw() {
    context_stack.push_front(intermediate_ctxt);
  }

  void write(std::ostream& out) const throw() {
    for (std::list<context>::const_iterator
	   i = context.begin();
	 i != context.end();
	 i++)
      (*i).write(out);
  }

  const char * what() const throw() {
    return reason.c_str();
  }
};

#if 0

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
  string   file;
  unsigned long line;
 public:
  file_context(const string& _file, unsigned long _line,
	       const string& _desc = "") throw()
    : error_context(_desc), file(_file), line(_line) {}
  virtual ~file_context() throw() {}

  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << " ";

    out << "\"" << file << "\", line " << line << ": ";
  }
};

class line_context : public error_context {
 public:
  string line;
  long	      pos;

  line_context(const string& _line, long _pos,
	       const string& _desc = "") throw()
    : error_context(_desc), line(_line), pos(_pos) {}
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

class error : public str_exception {
 public:
  error(const string& _reason, error_context * _ctxt = NULL) throw()
    : str_exception(_reason, _ctxt) {}
  virtual ~error() throw() {}
};

class fatal : public str_exception {
 public:
  fatal(const string& _reason, error_context * _ctxt = NULL) throw()
    : str_exception(_reason, _ctxt) {}
  virtual ~fatal() throw() {}
};

class fatal_assert : public fatal {
 public:
  fatal_assert(const string& _reason, error_context * _ctxt = NULL) throw()
    : fatal(string("assertion failed '") + _reason + "'", _ctxt) {}
  virtual ~fatal_assert() throw() {}
};

#endif // 0

inline void unexpected(char c, char wanted)
{
  if ((unsigned char) c == 0xff) {
    if (wanted)
      throw new error(string("Missing '") + wanted + "'");
    else
      throw new error("Unexpected end of input");
  } else {
    if (wanted)
      throw new error(string("Invalid char '") + c +
		      "' (wanted '" + wanted + "')");
    else
      throw new error(string("Invalid char '") + c + "'");
  }
}

} // namespace ledger

#endif // _ERROR_H
