#ifndef _PY_UTILS_H
#define _PY_UTILS_H

template <typename T, typename TfromPy>
struct object_from_python
{
  object_from_python() {
    boost::python::converter::registry::push_back
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
      return (value ? boost::python::to_python_value<T>()(*value) :
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

      void * const storage = ((rvalue_from_python_storage<T> *) data)->storage.bytes;

      if (data->convertible == source)	    // == None
	new (storage) boost::optional<T>(); // A Boost uninitialized value
      else
	new (storage) boost::optional<T>(*static_cast<T *>(data->convertible));

      data->convertible = storage;
    }
  };

  explicit register_optional_to_python() {
    register_python_conversion<boost::optional<T>,
      optional_to_python, optional_from_python>();
  }
};

//boost::python::register_ptr_to_python< boost::shared_ptr<Base> >();

#endif // _PY_UTILS_H
