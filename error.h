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
    return reason;
  }
};

class parse_error : public error
{
  unsigned int line;
  std::string  file;
 public:
  parse_error(const std::string& _file, const unsigned int _line,
	      const std::string& reason) throw()
    : line(_line), file(_file), error(reason) {}
  virtual ~parse_error() throw() {}

  virtual const char* what() const throw() {
    static std::ostringstream msg;
    msg << "Error: " << file << ", line " << line << ": " << error::what();
    return msg.str().c_str();
  }
};

} // namespace ledger

#endif // _CONSTRAINT_H
