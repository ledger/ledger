#ifndef _PYTHON_H
#define _PYTHON_H

#include "valexpr.h"

#include <string>
#include <iostream>

#include <boost/python.hpp>

using namespace boost::python;

namespace ledger {

enum py_eval_mode_t {
  PY_EVAL_EXPR,
  PY_EVAL_STMT,
  PY_EVAL_MULTI
};

object python_eval(std::istream& in, py_eval_mode_t mode = PY_EVAL_EXPR);
object python_eval(const std::string& str, py_eval_mode_t mode = PY_EVAL_EXPR);
object python_eval(const char * c_str, py_eval_mode_t mode = PY_EVAL_EXPR);

template <typename T>
bool python_call(const std::string& func_name, value_expr_t * arg_expr,
		 const details_t& details, T& result)
{
  try {
    object func = python_eval(func_name);
    if (arg_expr) {
      if (arg_expr->right) {
	list args;
	args.append(details);
	for (value_expr_t * arg = arg_expr; arg; arg = arg->right) {
	  assert(arg->kind == value_expr_t::O_ARG);
	  value_t value;
	  arg->left->compute(value, details);
	  args.append(value);
	}

	if (PyObject * val = PyObject_CallObject(func.ptr(),
						 tuple(args).ptr())) {
	  result = extract<T>(val)();
	  Py_DECREF(val);
	}
	else if (PyObject * err = PyErr_Occurred()) {
	  PyErr_Print();
	  throw value_expr_error(std::string("While calling Python function '") +
				 func_name + "'");
	}
	else {
	  return false;
	}
      } else {
	assert(arg_expr->kind == value_expr_t::O_ARG);
	value_t value;
	arg_expr->left->compute(value, details);
	result = call<T>(func.ptr(), details, value);
      }
    } else {
      result = call<T>(func.ptr(), details);
    }
    return true;
  }
  catch(const boost::python::error_already_set&) {
    PyErr_Print();
    throw value_expr_error(std::string("While calling Python function '") +
			   func_name + "'");
  }
 }

} // namespace ledger

#endif // _PYTHON_H
