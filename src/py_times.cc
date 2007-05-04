#include "pyinterp.h"
#include "pyutils.h"

#include <boost/cast.hpp>
#include <boost/python/module.hpp>
#include <boost/python/def.hpp>
#include <boost/python/to_python_converter.hpp>

#include <Python.h>
#include <datetime.h>

namespace ledger {

using namespace boost::python;

typedef boost::gregorian::date date;

struct date_to_python
{
  static PyObject* convert(const date& dte)
  {
    PyDateTime_IMPORT;
    return PyDate_FromDate(dte.year(), dte.month(), dte.day());
  }
};
   
struct date_from_python
{
  static void* convertible(PyObject* obj_ptr)
  {
    PyDateTime_IMPORT;
    if(PyDate_Check(obj_ptr) || PyDateTime_Check(obj_ptr)) return obj_ptr;
    return 0;
  }

  static void construct(PyObject* obj_ptr, converter::rvalue_from_python_stage1_data* data)
  {
    PyDateTime_IMPORT;
    int y = PyDateTime_GET_YEAR(obj_ptr);
    int m = PyDateTime_GET_MONTH(obj_ptr);
    int d = PyDateTime_GET_DAY(obj_ptr);
    date* dte = new date(y,m,d);
    data->convertible = (void*)dte;
  }
};

typedef register_python_conversion<date, date_to_python, date_from_python>
  date_python_conversion;


typedef boost::posix_time::ptime datetime;

struct datetime_to_python
{
  static PyObject* convert(const datetime& moment)
  {
    PyDateTime_IMPORT;
    date dte = moment.date();
    datetime::time_duration_type tod = moment.time_of_day();
    return PyDateTime_FromDateAndTime(dte.year(), dte.month(), dte.day(),
				      tod.hours(), tod.minutes(), tod.seconds(),
				      tod.total_microseconds() % 1000000);
  }
};
   
struct datetime_from_python
{
  static void* convertible(PyObject* obj_ptr)
  {
    PyDateTime_IMPORT;
    if(PyDateTime_Check(obj_ptr)) return obj_ptr;
    return 0;
  }

  static void construct(PyObject* obj_ptr, converter::rvalue_from_python_stage1_data* data)
  {
    PyDateTime_IMPORT;
    int y = PyDateTime_GET_YEAR(obj_ptr);
    int m = PyDateTime_GET_MONTH(obj_ptr);
    int d = PyDateTime_GET_DAY(obj_ptr);
    int h = PyDateTime_DATE_GET_HOUR(obj_ptr);
    int min = PyDateTime_DATE_GET_MINUTE(obj_ptr);
    int s = PyDateTime_DATE_GET_SECOND(obj_ptr);
    datetime* moment = new datetime(date(y,m,d),
				    datetime::time_duration_type(h, min, s));
    data->convertible = (void*)moment;
  }
};

typedef register_python_conversion<datetime, datetime_to_python, datetime_from_python>
  datetime_python_conversion;

void export_times()
{
  date_python_conversion();
  datetime_python_conversion();
}

} // namespace ledger
