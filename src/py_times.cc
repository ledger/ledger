/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "pyinterp.h"
#include "pyutils.h"
#include "times.h"

// jww (2007-05-04): Convert time duration objects to PyDelta

namespace ledger {

using namespace boost::python;

#define MY_PyDateTime_IMPORT                            \
  PyDateTimeAPI = (PyDateTime_CAPI*)                    \
  PyCObject_Import(const_cast<char *>("datetime"),      \
                   const_cast<char *>("datetime_CAPI"))

struct date_to_python
{
  static PyObject* convert(const date_t& dte)
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

  static void construct(PyObject * obj_ptr,
                        converter::rvalue_from_python_stage1_data * data)
  {
    MY_PyDateTime_IMPORT;

    int year = PyDateTime_GET_YEAR(obj_ptr);
    date::year_type y = gregorian::greg_year(static_cast<unsigned short>(year));
    date::month_type m =
      static_cast<date::month_type>(PyDateTime_GET_MONTH(obj_ptr));
    date::day_type d =
      static_cast<date::day_type>(PyDateTime_GET_DAY(obj_ptr));

    date_t * dte = new date_t(y, m, d);

    data->convertible = (void *) dte;
  }
};

typedef register_python_conversion<date_t, date_to_python, date_from_python>
  date_python_conversion;


struct datetime_to_python
{
  static PyObject* convert(const datetime_t& moment)
  {
    MY_PyDateTime_IMPORT;

    date_t dte = moment.date();
    datetime_t::time_duration_type tod = moment.time_of_day();

    return PyDateTime_FromDateAndTime
      (static_cast<int>(dte.year()), static_cast<int>(dte.month()),
       static_cast<int>(dte.day()), static_cast<int>(tod.hours()),
       static_cast<int>(tod.minutes()), static_cast<int>(tod.seconds()),
       static_cast<int>(tod.total_microseconds() % 1000000));
  }
};

struct datetime_from_python
{
  static void* convertible(PyObject* obj_ptr)
  {
    MY_PyDateTime_IMPORT;
    if (PyDateTime_Check(obj_ptr)) return obj_ptr;
    return 0;
  }

  static void construct(PyObject * obj_ptr,
                        converter::rvalue_from_python_stage1_data * data)
  {
    MY_PyDateTime_IMPORT;

    int year = PyDateTime_GET_YEAR(obj_ptr);
    date::year_type y = gregorian::greg_year(static_cast<unsigned short>(year));
    date::month_type m =
      static_cast<date::month_type>(PyDateTime_GET_MONTH(obj_ptr));
    date::day_type d =
      static_cast<date::day_type>(PyDateTime_GET_DAY(obj_ptr));

    datetime_t::time_duration_type::hour_type h =
      static_cast<datetime_t::time_duration_type::hour_type>
      (PyDateTime_DATE_GET_HOUR(obj_ptr));
    datetime_t::time_duration_type::min_type min =
      static_cast<datetime_t::time_duration_type::min_type>
      (PyDateTime_DATE_GET_MINUTE(obj_ptr));
    datetime_t::time_duration_type::sec_type s =
      static_cast<datetime_t::time_duration_type::sec_type>
      (PyDateTime_DATE_GET_SECOND(obj_ptr));
    datetime_t::time_duration_type::fractional_seconds_type ms =
      static_cast<datetime_t::time_duration_type::fractional_seconds_type>
      (PyDateTime_DATE_GET_MICROSECOND(obj_ptr)) * 1000000;

    datetime_t * moment
      = new datetime_t(date_t(y, m, d),
                       datetime_t::time_duration_type(h, min, s, ms));

    data->convertible = (void *) moment;
  }
};

typedef register_python_conversion<datetime_t,
                                   datetime_to_python, datetime_from_python>
  datetime_python_conversion;


/* Convert time_duration to/from python */
struct duration_to_python
{
  static int get_usecs(boost::posix_time::time_duration const& d)
  {
    static int64_t resolution =
      boost::posix_time::time_duration::ticks_per_second();
    int64_t fracsecs = d.fractional_seconds();
    if (resolution > 1000000)
      return static_cast<int>(fracsecs / (resolution / 1000000));
    else
      return static_cast<int>(fracsecs * (1000000 / resolution));
  }

  static PyObject * convert(posix_time::time_duration d)
  {
    int days = d.hours() / 24;
    if (days < 0)
      days --;
    int seconds = d.total_seconds() - days*(24*3600);
    int usecs = get_usecs(d);
    if (days < 0)
      usecs = 1000000-1 - usecs;
    return PyDelta_FromDSU(days, seconds, usecs);
  }
};

/* Should support the negative values, but not the special boost time
   durations */
struct duration_from_python
{
  static void* convertible(PyObject * obj_ptr)
  {
    if ( ! PyDelta_Check(obj_ptr))
      return 0;
    return obj_ptr;
  }

  static void construct(PyObject* obj_ptr,
                        python::converter::rvalue_from_python_stage1_data * data)
  {
    PyDateTime_Delta const* pydelta
      = reinterpret_cast<PyDateTime_Delta*>(obj_ptr);

    int days = pydelta->days;
    bool is_negative = (days < 0);
    if (is_negative)
      days = -days;

    // Create time duration object
    posix_time::time_duration
      duration = (posix_time::hours(24) * days +
                  posix_time::seconds(pydelta->seconds) +
                  posix_time::microseconds(pydelta->microseconds));
    if (is_negative)
      duration = duration.invert_sign();

    void * storage =
      reinterpret_cast<converter::rvalue_from_python_storage
                       <posix_time::time_duration> *>
                      (data)->storage.bytes;

    new (storage) posix_time::time_duration(duration);
    data->convertible = storage;
  }
};

typedef register_python_conversion<time_duration_t,
                                   duration_to_python, duration_from_python>
  duration_python_conversion;


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
  duration_python_conversion();

  register_optional_to_python<datetime_t>();
  register_optional_to_python<date_t>();

  scope().attr("parse_datetime")   = &py_parse_datetime;
  scope().attr("parse_date")       = &py_parse_date;
  scope().attr("times_initialize") = &times_initialize;
  scope().attr("times_shutdown")   = &times_shutdown;

#if 0
  class_< interval_t > ("Interval")
    ;
#endif
}

} // namespace ledger
