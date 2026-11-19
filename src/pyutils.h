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

/*
 * @addtogroup python
 */

/**
 * @file   pyutils.h
 * @author John Wiegley
 *
 * @ingroup python
 *
 * @brief Basic utilities for Python API.
 */
#pragma once

namespace ledger::python {

template <typename T, typename TfromPy>
struct object_from_python {
  object_from_python() {
    boost::python::converter::registry::insert(&TfromPy::convertible, &TfromPy::construct,
                                               boost::python::type_id<T>());
  }
};

template <typename T, typename TtoPy, typename TfromPy>
struct register_python_conversion {
  register_python_conversion() {
    boost::python::to_python_converter<T, TtoPy>();
    object_from_python<T, TfromPy>();
  }
};

template <typename T>
struct register_optional_to_python : public boost::noncopyable {
  // Converters for boost::optional<T> (still used by members declared with
  // unqualified `optional`, which resolves to boost::optional via
  // `using namespace boost` in utils.h)
  struct boost_optional_to_python {
    static PyObject* convert(const boost::optional<T>& value) {
      return boost::python::incref(value ? boost::python::to_python_value<T>()(*value)
                                         : boost::python::detail::none());
    }
  };

  struct boost_optional_from_python {
    static void* convertible(PyObject* source) {
      using namespace boost::python::converter;
      if (source == Py_None)
        return source;
      const registration& converters(registered<T>::converters);
      if (implicit_rvalue_convertible_from_python(source, converters)) {
        rvalue_from_python_stage1_data data = rvalue_from_python_stage1(source, converters);
        return data.convertible;
      }
      return nullptr;
    }

    static void construct(PyObject* source,
                          boost::python::converter::rvalue_from_python_stage1_data* data) {
      using namespace boost::python::converter;
      void* storage = ((rvalue_from_python_storage<boost::optional<T>>*)data)->storage.bytes;
      if (source == Py_None)
        new (storage) boost::optional<T>();
      else
        new (storage) boost::optional<T>(boost::python::extract<T>(source)());
      data->convertible = storage;
    }
  };

  // Converters for std::optional<T> (used by explicitly migrated fields
  // like xact_t::code and wrapper function return types)
  struct std_optional_to_python {
    static PyObject* convert(const std::optional<T>& value) {
      return boost::python::incref(value ? boost::python::to_python_value<T>()(*value)
                                         : boost::python::detail::none());
    }
  };

  struct std_optional_from_python {
    static void* convertible(PyObject* source) {
      using namespace boost::python::converter;
      if (source == Py_None)
        return source;
      const registration& converters(registered<T>::converters);
      if (implicit_rvalue_convertible_from_python(source, converters)) {
        rvalue_from_python_stage1_data data = rvalue_from_python_stage1(source, converters);
        return data.convertible;
      }
      return nullptr;
    }

    static void construct(PyObject* source,
                          boost::python::converter::rvalue_from_python_stage1_data* data) {
      using namespace boost::python::converter;
      void* storage = ((rvalue_from_python_storage<std::optional<T>>*)data)->storage.bytes;
      if (source == Py_None)
        new (storage) std::optional<T>();
      else
        new (storage) std::optional<T>(boost::python::extract<T>(source)());
      data->convertible = storage;
    }
  };

  explicit register_optional_to_python() {
    register_python_conversion<boost::optional<T>, boost_optional_to_python,
                               boost_optional_from_python>();
    register_python_conversion<std::optional<T>, std_optional_to_python,
                               std_optional_from_python>();
  }
};

/// Binds the flags API -- the flags property and has_flags, clear_flags,
/// add_flags and drop_flags -- through Flags, the flags base the wrapped C++
/// class derives from.  Naming any other instantiation fails to compile,
/// instead of raising ArgumentError on every call from Python.
template <typename Flags>
class flags_visitor : public boost::python::def_visitor<flags_visitor<Flags>> {
  friend class boost::python::def_visitor_access;

  template <typename Class>
  void visit(Class& c) const {
    static_assert(std::is_base_of_v<Flags, typename Class::wrapped_type>,
                  "flags_visitor must name a flags base of the wrapped class");
    c.add_property("flags", &Flags::flags, &Flags::set_flags)
        .def("has_flags", &Flags::has_flags)
        .def("clear_flags", &Flags::clear_flags)
        .def("add_flags", &Flags::add_flags)
        .def("drop_flags", &Flags::drop_flags);
  }
};

template <typename T>
PyObject* str_to_py_unicode(const T& str) {
  return PyUnicode_FromString(str.c_str());
}

/// Maps a Python sequence index onto [0, len): a negative index counts
/// from the end.  Any index outside [-len, len) sets IndexError and
/// throws error_already_set.
inline std::size_t py_sequence_index(long i, std::size_t len) {
  const long n = static_cast<long>(len);
  if (i >= n || i < -n) {
    PyErr_SetString(PyExc_IndexError, _("Index out of range"));
    boost::python::throw_error_already_set();
  }
  return static_cast<std::size_t>(i < 0 ? n + i : i);
}

/// Registers a translator that raises the C++ exception @p E in Python as
/// @p py_type (e.g. PyExc_ArithmeticError), carrying its what() text.
template <class E>
void register_error_translator(PyObject* py_type) {
  boost::python::register_exception_translator<E>(
      [py_type](const E& err) { PyErr_SetString(py_type, err.what()); });
}

/// Builds a Python list from [first, last), appending convert(*it) for each
/// element in order.
template <class It, class Fn>
boost::python::list to_py_list(It first, It last, Fn convert) {
  boost::python::list result;
  for (; first != last; ++first)
    result.append(convert(*first));
  return result;
}

} // namespace ledger::python

// boost::python::register_ptr_to_python< std::shared_ptr<Base> >();
