#ifndef _MASK_H
#define _MASK_H

#include "error.h"

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

class mask_error : public error {
 public:
  mask_error(const std::string& reason) throw() : error(reason) {}
  virtual ~mask_error() throw() {}
};

#endif // _MASK_H
