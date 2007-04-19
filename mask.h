#ifndef _MASK_H
#define _MASK_H

#include "error.h"

#include <string>
#include <exception>

#include <boost/regex.hpp>

class mask_t
{
 public:
  bool	       exclude;
  boost::regex expr;

  explicit mask_t(const std::string& pattern);
  mask_t(const mask_t& m) : exclude(m.exclude), expr(m.expr) {}

  bool match(const std::string& str) const {
    return boost::regex_match(str, expr) && ! exclude;
  }
};

class mask_error : public error {
 public:
  mask_error(const std::string& _reason) throw() : error(_reason) {}
  virtual ~mask_error() throw() {}
};

#endif // _MASK_H
