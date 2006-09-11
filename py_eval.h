#ifndef _PY_EVAL_H
#define _PY_EVAL_H

#include "valexpr.h"
#include "pyfstream.h"

#include <string>
#include <iostream>

#include <boost/python.hpp>

using namespace boost::python;

namespace ledger {

void shutdown_ledger_for_python();

object python_import(const std::string& name);

enum py_eval_mode_t {
  PY_EVAL_EXPR,
  PY_EVAL_STMT,
  PY_EVAL_MULTI
};

object python_eval(std::istream& in,       py_eval_mode_t mode = PY_EVAL_EXPR);
object python_eval(const std::string& str, py_eval_mode_t mode = PY_EVAL_EXPR);
object python_eval(const char * c_str,     py_eval_mode_t mode = PY_EVAL_EXPR);

class python_functor_t : public valexpr_t::functor_t
{
 protected:
  object func;
 public:
  python_functor_t(object _func) : func(_func) {}

  virtual value_t operator()(valexpr_t::scope_t * args);
};

class python_lambda_t : public python_functor_t
{
 public:
  python_lambda_t(object code) : python_functor_t(code) {}

  virtual value_t operator()(valexpr_t::scope_t * args);
};

#if 0
class python_scope_t : public scope_t
{
  python_scope_t(scope_t * parent = NULL) : scope_t(parent) {}

  virtual void define(const std::string& name, valexpr_t * def) {
    assert(0);
  }

  virtual valexpr_t * lookup(const std::string& name);
};
#endif

} // namespace ledger

#endif // _PY_EVAL_H
