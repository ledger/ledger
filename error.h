#ifndef _ERROR_H
#define _ERROR_H

#include <exception>
#include <sstream>

namespace ledger {

class error : public std::exception
{
  std::string reason;
 public:
  error(const std::string& _reason) throw() : reason(_reason) {}
  virtual ~error() throw() {}

  virtual const char* what() const throw() {
    return reason.c_str();
  }
};

class compute_error : public error
{
 public:
  compute_error(const std::string& reason) throw() : error(reason) {}
  virtual ~compute_error() throw() {}
};

class expr_error : public error
{
 public:
  expr_error(const std::string& reason) throw() : error(reason) {}
  virtual ~expr_error() throw() {}
};

class format_error : public error
{
 public:
  format_error(const std::string& reason) throw() : error(reason) {}
  virtual ~format_error() throw() {}
};

class parse_error : public error
{
  unsigned int line;
  std::string  file;
 public:
  parse_error(const std::string& _file, const unsigned int _line,
	      const std::string& reason) throw()
    : error(reason), line(_line), file(_file) {}
  virtual ~parse_error() throw() {}

  virtual const char* what() const throw() {
    static std::ostringstream msg;
    msg << file << ", line " << line << ": " << error::what();
    return msg.str().c_str();
  }
};

} // namespace ledger

#endif // _CONSTRAINT_H
