/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _PYINTERP_H
#define _PYINTERP_H

#include "session.h"

#if defined(HAVE_BOOST_PYTHON)

namespace ledger {

class python_interpreter_t : public session_t
{
public:
  python::dict main_nspace;
  bool is_initialized;

  python_interpreter_t()
    : session_t(), main_nspace(), is_initialized(false) {
    TRACE_CTOR(python_interpreter_t, "");
  }
  
  virtual ~python_interpreter_t() {
    TRACE_DTOR(python_interpreter_t);

    if (is_initialized)
      Py_Finalize();
  }

  void initialize();

  python::object import(const string& name);

  enum py_eval_mode_t {
    PY_EVAL_EXPR,
    PY_EVAL_STMT,
    PY_EVAL_MULTI
  };

  python::object eval(std::istream& in,
			     py_eval_mode_t mode = PY_EVAL_EXPR);
  python::object eval(const string& str,
			     py_eval_mode_t mode = PY_EVAL_EXPR);
  python::object eval(const char * c_str,
			     py_eval_mode_t mode = PY_EVAL_EXPR) {
    string str(c_str);
    return eval(str, mode);
  }

  class functor_t {
    functor_t();

  protected:
    python::object func;

  public:
    string name;

    functor_t(const string& _name, python::object _func)
      : func(_func), name(_name) {
      TRACE_CTOR(functor_t, "const string&, python::object");
    }
    functor_t(const functor_t& other)
      : func(other.func), name(other.name) {
      TRACE_CTOR(functor_t, "copy");
    }
    virtual ~functor_t() throw() {
      TRACE_DTOR(functor_t);
    }
    virtual value_t operator()(call_scope_t& args);
  };

  option_t<python_interpreter_t> * lookup_option(const char * p);

  virtual expr_t::ptr_op_t lookup(const string& name);

  OPTION_(python_interpreter_t, import_, DO_(scope) {
      interactive_t args(scope, "s");

      path file(args.get<string>(0));

      python::object module_sys = parent->import("sys"); 
      python::object sys_dict	= module_sys.attr("__dict__");

      python::list paths(sys_dict["path"]);
#if BOOST_VERSION >= 103700
      paths.insert(0, file.parent_path().string());
#else
      paths.insert(0, file.branch_path().string());
#endif
      sys_dict["path"] = paths;

#if BOOST_VERSION >= 103700
      string name = file.filename();
      if (contains(name, ".py"))
	parent->import(file.stem());
      else
	parent->import(name);
#else
      parent->import(file.leaf());
#endif
    });
};

extern shared_ptr<python_interpreter_t> python_session;

} // namespace ledger

#endif // HAVE_BOOST_PYTHON

#endif // _PYINTERP_H
