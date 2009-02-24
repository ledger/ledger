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

#include "pyinterp.h"
#include "pyutils.h"

namespace ledger {

using namespace boost::python;

struct bool_to_python
{
  static PyObject * convert(const bool truth)
  {
    if (truth)
      Py_RETURN_TRUE;
    else
      Py_RETURN_FALSE;
  }
};

struct bool_from_python
{
  static void* convertible(PyObject* obj_ptr)
  {
    if (!PyBool_Check(obj_ptr)) return 0;
    return obj_ptr;
  }

  static void construct(PyObject* obj_ptr,
			converter::rvalue_from_python_stage1_data* data)
  {
    void* storage = ((converter::rvalue_from_python_storage<bool>*) data)->storage.bytes;
    if (obj_ptr == Py_True)
      new (storage) bool(true);
    else
      new (storage) bool(false);
    data->convertible = storage;
  }
};

typedef register_python_conversion<bool, bool_to_python, bool_from_python>
  bool_python_conversion;


struct string_to_python
{
  static PyObject* convert(const string& str)
  {
    return incref(object(*boost::polymorphic_downcast<const std::string *>(&str)).ptr());
  }
};

struct string_from_python
{
  static void* convertible(PyObject* obj_ptr)
  {
    if (!PyString_Check(obj_ptr)) return 0;
    return obj_ptr;
  }

  static void construct(PyObject* obj_ptr, converter::rvalue_from_python_stage1_data* data)
  {
    const char* value = PyString_AsString(obj_ptr);
    if (value == 0) throw_error_already_set();
    void* storage =
      reinterpret_cast<converter::rvalue_from_python_storage<string> *>(data)->storage.bytes;
    new (storage) string(value);
    data->convertible = storage;
  }
};

typedef register_python_conversion<string, string_to_python, string_from_python>
  string_python_conversion;


struct istream_to_python
{
  static PyObject* convert(const std::istream&)
  {
    return incref(boost::python::detail::none());
  }
};

struct istream_from_python
{
  static void* convertible(PyObject* obj_ptr)
  {
    if (!PyFile_Check(obj_ptr)) return 0;
    return obj_ptr;
  }

  static void construct(PyObject* obj_ptr, converter::rvalue_from_python_stage1_data* data)
  {
    void* storage =
      reinterpret_cast<converter::rvalue_from_python_storage<pyifstream> *>(data)->storage.bytes;
    new (storage) pyifstream(reinterpret_cast<PyFileObject *>(obj_ptr));
    data->convertible = storage;
  }
};

typedef register_python_conversion<std::istream, istream_to_python, istream_from_python>
  istream_python_conversion;


struct ostream_to_python
{
  static PyObject* convert(const std::ostream&)
  {
    return incref(boost::python::detail::none());
  }
};

struct ostream_from_python
{
  static void* convertible(PyObject* obj_ptr)
  {
    if (!PyFile_Check(obj_ptr)) return 0;
    return obj_ptr;
  }

  static void construct(PyObject* obj_ptr, converter::rvalue_from_python_stage1_data* data)
  {
    void* storage = reinterpret_cast<converter::rvalue_from_python_storage<pyofstream> *>(data)->storage.bytes;
    new (storage) pyofstream(reinterpret_cast<PyFileObject *>(obj_ptr));
    data->convertible = storage;
  }
};

typedef register_python_conversion<std::ostream, ostream_to_python, ostream_from_python>
  ostream_python_conversion;


void export_utils()
{
  bool_python_conversion();
  string_python_conversion();
  istream_python_conversion();
  ostream_python_conversion();
}

} // namespace ledger
