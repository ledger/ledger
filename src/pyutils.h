/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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

#ifndef _PY_UTILS_H
#define _PY_UTILS_H

template <typename T, typename TfromPy>
struct object_from_python
{
  object_from_python() {
    boost::python::converter::registry::insert
      (&TfromPy::convertible, &TfromPy::construct,
       boost::python::type_id<T>());
  }
};

template <typename T, typename TtoPy, typename TfromPy>
struct register_python_conversion
{
  register_python_conversion() {
    boost::python::to_python_converter<T, TtoPy>();
    object_from_python<T, TfromPy>();
  }
};

template <typename T>
struct register_optional_to_python : public boost::noncopyable
{
  struct optional_to_python
  {
    static PyObject * convert(const boost::optional<T>& value)
    {
      return boost::python::incref
        (value ? boost::python::to_python_value<T>()(*value) :
                 boost::python::detail::none());
    }
  };

  struct optional_from_python
  {
    static void * convertible(PyObject * source)
    {
      using namespace boost::python::converter;

      if (source == Py_None)
        return source;

      const registration& converters(registered<T>::converters);

      if (implicit_rvalue_convertible_from_python(source, converters)) {
        rvalue_from_python_stage1_data data =
          rvalue_from_python_stage1(source, converters);
        return rvalue_from_python_stage2(source, data, converters);
      }
      return NULL;
    }

    static void construct(PyObject * source,
                          boost::python::converter::rvalue_from_python_stage1_data * data)
    {
      using namespace boost::python::converter;

      void * const storage =
        reinterpret_cast<rvalue_from_python_storage<T> *>(data)->storage.bytes;

      if (data->convertible == source)      // == None
        new (storage) boost::optional<T>(); // A Boost uninitialized value
      else
        new (storage) boost::optional<T>(*reinterpret_cast<T *>(data->convertible));

      data->convertible = storage;
    }
  };

  explicit register_optional_to_python() {
    register_python_conversion<boost::optional<T>,
                               optional_to_python, optional_from_python>();
  }
};

template <typename T1, typename T2>
struct PairToTupleConverter
{
  static PyObject * convert(const std::pair<T1, T2>& pair) {
    return boost::python::incref
      (boost::python::make_tuple(pair.first, pair.second).ptr());
  }
};

template <typename MapType>
struct map_value_type_converter
{
  map_value_type_converter() {
    boost::python::to_python_converter
      <typename MapType::value_type,
       PairToTupleConverter<const typename MapType::key_type,
                            typename MapType::mapped_type> >();
  }
};

template <typename T>
PyObject * str_to_py_unicode(const T& str)
{
  using namespace boost::python;
  PyObject * pstr = PyString_FromString(str.c_str());
  PyObject * uni  = PyUnicode_FromEncodedObject(pstr, "UTF-8", NULL);
  return object(handle<>(borrowed(uni))).ptr();
}

namespace boost { namespace python {

// Use expr to create the PyObject corresponding to x
# define BOOST_PYTHON_RETURN_TO_PYTHON_BY_VALUE(T, expr, pytype)\
    template <> struct to_python_value<T&>                      \
        : detail::builtin_to_python                             \
    {                                                           \
        inline PyObject* operator()(T const& x) const           \
        {                                                       \
            return (expr);                                      \
        }                                                       \
        inline PyTypeObject const* get_pytype() const           \
        {                                                       \
            return (pytype);                                    \
        }                                                       \
    };                                                          \
    template <> struct to_python_value<T const&>                \
        : detail::builtin_to_python                             \
    {                                                           \
        inline PyObject* operator()(T const& x) const           \
        {                                                       \
            return (expr);                                      \
        }                                                       \
        inline PyTypeObject const* get_pytype() const           \
        {                                                       \
            return (pytype);                                    \
        }                                                       \
    };

# define BOOST_PYTHON_ARG_TO_PYTHON_BY_VALUE(T, expr)   \
    namespace converter                                 \
    {                                                   \
      template <> struct arg_to_python< T >             \
        : handle<>                                      \
      {                                                 \
          arg_to_python(T const& x)                     \
            : python::handle<>(expr) {}                 \
      };                                                \
    }

// Specialize argument and return value converters for T using expr
# define BOOST_PYTHON_TO_PYTHON_BY_VALUE(T, expr, pytype)  \
        BOOST_PYTHON_RETURN_TO_PYTHON_BY_VALUE(T,expr, pytype)  \
        BOOST_PYTHON_ARG_TO_PYTHON_BY_VALUE(T,expr)

} } // namespace boost::python

//boost::python::register_ptr_to_python< boost::shared_ptr<Base> >();

#endif // _PY_UTILS_H
