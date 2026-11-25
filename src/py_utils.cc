/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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

/**
 * @file   py_utils.cc
 * @brief  Python bindings for utility types and fundamental type converters.
 * @ingroup python
 *
 * Registers bidirectional converters for bool and string (handling UTF-8/
 * UTF-16/UTF-32 transparently), and exposes the flags support classes
 * (SupportFlags8, SupportFlags16, DelegatesFlags16) used by many Ledger
 * types for bitfield state management.
 */

#include <system.hh>

#include "pyinterp.h"
#include "pyutils.h"

namespace ledger {

using namespace flags;
using namespace python;
using namespace boost::python;

/*--- bool <-> Python bool ---*/

struct bool_to_python {
  static PyObject* convert(const bool truth) {
    if (truth)
      Py_RETURN_TRUE;
    else
      Py_RETURN_FALSE;
  }
};

struct bool_from_python {
  static void* convertible(PyObject* obj_ptr) {
    if (!PyBool_Check(obj_ptr))
      return nullptr;
    return obj_ptr;
  }

  static void construct(PyObject* obj_ptr, converter::rvalue_from_python_stage1_data* data) {
    void* storage = ((converter::rvalue_from_python_storage<bool>*)data)->storage.bytes;
    if (obj_ptr == Py_True)
      new (storage) bool(true);
    else
      new (storage) bool(false);
    data->convertible = storage;
  }
};

using bool_python_conversion = register_python_conversion<bool, bool_to_python, bool_from_python>;

/*--- string <-> Python str (UTF-8) ---*/

struct string_to_python {
  static PyObject* convert(const string& str) {
    // Boost.Python converts std::string to a Python str (UTF-8 decoded).
    return incref(object(static_cast<const std::string&>(str)).ptr());
  }
};

struct string_from_python {
  static void* convertible(PyObject* obj_ptr) {
    if (!PyUnicode_Check(obj_ptr))
      return nullptr;
    return obj_ptr;
  }

  static void construct(PyObject* obj_ptr, converter::rvalue_from_python_stage1_data* data) {
    Py_ssize_t size;
    const char* utf8 = PyUnicode_AsUTF8AndSize(obj_ptr, &size);
    if (!utf8)
      throw_error_already_set();

    void* storage =
        reinterpret_cast<converter::rvalue_from_python_storage<string>*>(data)->storage.bytes;
    new (storage) string(utf8, static_cast<std::size_t>(size));
    data->convertible = storage;
  }
};

using string_python_conversion =
    register_python_conversion<string, string_to_python, string_from_python>;

/*--- Flags Classes and Registration ---*/

/// @brief Registers flag support classes and type converters.
///
/// Exposes SupportFlags8, SupportFlags16, and DelegatesFlags16 for bitfield
/// manipulation, and registers the bool and string Python converters.
void export_utils() {
  class_<supports_flags<uint_least8_t>>("SupportFlags8")
      .def(init<supports_flags<uint_least8_t>>())
      .def(init<uint_least8_t>())

      .def(flags_visitor<supports_flags<uint_least8_t>>());

  class_<supports_flags<uint_least16_t>>("SupportFlags16")
      .def(init<supports_flags<uint_least16_t>>())
      .def(init<uint_least16_t>())

      .def(flags_visitor<supports_flags<uint_least16_t>>());

  class_<delegates_flags<uint_least16_t>, boost::noncopyable>("DelegatesFlags16", no_init)
      .def(flags_visitor<delegates_flags<uint_least16_t>>());

  bool_python_conversion();
  string_python_conversion();
}

} // namespace ledger
