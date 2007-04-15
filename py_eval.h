#ifndef _PY_EVAL_H
#define _PY_EVAL_H

#include "xpath.h"
#include "pyfstream.h"

#include <string>
#include <iostream>

#include <boost/python.hpp>

using namespace boost::python;

namespace ledger {

void initialize_ledger_for_python();
void shutdown_ledger_for_python();

class python_interpreter_t : public xml::xpath_t::scope_t
{
  handle<> mmodule;

 public:
  dict     nspace;

  python_interpreter_t(xml::xpath_t::scope_t * parent);

  virtual ~python_interpreter_t() {
    Py_Finalize();
  }

  object import(const std::string& name);

  enum py_eval_mode_t {
    PY_EVAL_EXPR,
    PY_EVAL_STMT,
    PY_EVAL_MULTI
  };

  object eval(std::istream& in,       py_eval_mode_t mode = PY_EVAL_EXPR);
  object eval(const std::string& str, py_eval_mode_t mode = PY_EVAL_EXPR);
  object eval(const char * c_str,     py_eval_mode_t mode = PY_EVAL_EXPR) {
    std::string str(c_str);
    return eval(str, mode);
  }

  class functor_t : public xml::xpath_t::functor_t {
   protected:
    object func;
   public:
    functor_t(const std::string& name, object _func)
      : xml::xpath_t::functor_t(name), func(_func) {}

    virtual void operator()(value_t& result, xml::xpath_t::scope_t * locals);
  };

  virtual void define(const std::string& name, xml::xpath_t::op_t * def) {
    // Pass any definitions up to our parent
    parent->define(name, def);
  }

  virtual xml::xpath_t::op_t * lookup(const std::string& name) {
    object func = eval(name);
    if (! func)
      return parent ? parent->lookup(name) : NULL;
    return xml::xpath_t::wrap_functor(new functor_t(name, func));
  }

  class lambda_t : public functor_t {
   public:
    lambda_t(object code) : functor_t("<lambda>", code) {}

    virtual void operator()(value_t& result, xml::xpath_t::scope_t * locals);
  };
};

} // namespace ledger

#endif // _PY_EVAL_H
