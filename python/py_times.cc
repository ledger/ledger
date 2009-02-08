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

// jww (2007-05-04): Convert time duration objects to PyDelta

namespace ledger {

using namespace boost::python;

typedef boost::gregorian::date date;

#define MY_PyDateTime_IMPORT				\
  PyDateTimeAPI = (PyDateTime_CAPI*)			\
  PyCObject_Import(const_cast<char *>("datetime"),	\
		   const_cast<char *>("datetime_CAPI"))

struct date_to_python
{
  static PyObject* convert(const date& dte)
  {
    MY_PyDateTime_IMPORT;
    return PyDate_FromDate(dte.year(), dte.month(), dte.day());
  }
};

struct date_from_python
{
  static void* convertible(PyObject* obj_ptr)
  {
    MY_PyDateTime_IMPORT;
    if (PyDate_Check(obj_ptr)) return obj_ptr;
    return 0;
  }

  static void construct(PyObject* obj_ptr, converter::rvalue_from_python_stage1_data* data)
  {
    MY_PyDateTime_IMPORT;
    int y = PyDateTime_GET_YEAR(obj_ptr);
    int m = PyDateTime_GET_MONTH(obj_ptr);
    int d = PyDateTime_GET_DAY(obj_ptr);
    date* dte = new date(y,m,d);
    data->convertible = (void*)dte;
  }
};

typedef register_python_conversion<date, date_to_python, date_from_python>
  date_python_conversion;


struct datetime_to_python
{
  static PyObject* convert(const datetime_t& moment)
  {
    MY_PyDateTime_IMPORT;
    date dte = moment.date();
    datetime_t::time_duration_type tod = moment.time_of_day();
    return PyDateTime_FromDateAndTime(dte.year(), dte.month(), dte.day(),
				      tod.hours(), tod.minutes(), tod.seconds(),
				      tod.total_microseconds() % 1000000);
  }
};

struct datetime_from_python
{
  static void* convertible(PyObject* obj_ptr)
  {
    MY_PyDateTime_IMPORT;
    if(PyDateTime_Check(obj_ptr)) return obj_ptr;
    return 0;
  }

  static void construct(PyObject* obj_ptr, converter::rvalue_from_python_stage1_data* data)
  {
    MY_PyDateTime_IMPORT;
    int y = PyDateTime_GET_YEAR(obj_ptr);
    int m = PyDateTime_GET_MONTH(obj_ptr);
    int d = PyDateTime_GET_DAY(obj_ptr);
    int h = PyDateTime_DATE_GET_HOUR(obj_ptr);
    int min = PyDateTime_DATE_GET_MINUTE(obj_ptr);
    int s = PyDateTime_DATE_GET_SECOND(obj_ptr);
    datetime_t* moment = new datetime_t(date(y,m,d),
				    datetime_t::time_duration_type(h, min, s));
    data->convertible = (void*)moment;
  }
};

typedef register_python_conversion<datetime_t, datetime_to_python, datetime_from_python>
  datetime_python_conversion;

datetime_t py_parse_datetime(const string& str) {
  return parse_datetime(str);
}

date_t py_parse_date(const string& str) {
  return parse_date(str);
}

void export_times()
{
  datetime_python_conversion();
  date_python_conversion();

  register_optional_to_python<datetime_t>();
  register_optional_to_python<date_t>();

  scope().attr("parse_datetime") = &py_parse_datetime;
  scope().attr("parse_date")	 = &py_parse_date;

#if 0
  class_< interval_t > ("Interval")
    ;
#endif
}

} // namespace ledger
