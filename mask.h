#ifndef _MASK_H
#define _MASK_H

#include <string>
#include <exception>

class mask_t
{
 public:
  bool        exclude;
  std::string pattern;
  void *      regexp;

  explicit mask_t(const std::string& pattern);
  mask_t(const mask_t&);
  ~mask_t();

  bool match(const std::string& str) const;
};

class mask_error : public std::exception {
  std::string reason;
 public:
  mask_error(const std::string& _reason) throw() : reason(_reason) {}
  virtual ~mask_error() throw() {}

  virtual const char* what() const throw() {
    return reason.c_str();
  }
};

#endif // _MASK_H
