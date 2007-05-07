#include "pyinterp.h"
#include "pyutils.h"

#include <boost/python/module.hpp>
#include <boost/python/def.hpp>
#include <boost/python/to_python_converter.hpp>

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
    void* storage = ((converter::rvalue_from_python_storage<string>*) data)->storage.bytes;
    new (storage) string(value);
    data->convertible = storage;
  }
};

typedef register_python_conversion<string, string_to_python, string_from_python>
  string_python_conversion;

void export_utils()
{
  bool_python_conversion();
  string_python_conversion();
}

} // namespace ledger
