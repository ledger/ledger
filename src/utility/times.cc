/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#include "utils.h"

namespace ledger {

#ifdef BOOST_DATE_TIME_HAS_HIGH_PRECISION_CLOCK
const ptime time_now = boost::posix_time::microsec_clock::universal_time();
#else
const ptime time_now = boost::posix_time::second_clock::universal_time();
#endif
const date  date_now = boost::gregorian::day_clock::universal_day();

#ifdef SUPPORT_DATE_AND_TIME
const moment_t& now(time_now);
#else
const moment_t& now(date_now);
#endif

bool day_before_month = false;
static bool  day_before_month_initialized = false;

moment_t parse_datetime(const char * str)
{
  if (! day_before_month_initialized) {
#ifdef HAVE_NL_LANGINFO
    const char * d_fmt = nl_langinfo(D_FMT);
    if (d_fmt && std::strlen(d_fmt) > 1 && d_fmt[1] == 'd')
      day_before_month = true;
    day_before_month_initialized = true;
#endif
  }
#if 0
  return parse_abs_datetime(in);
#else
  int year = ((str[0] - '0') * 1000 +
	      (str[1] - '0') * 100 +
	      (str[2] - '0') * 10 +
	      (str[3] - '0'));

  int mon = ((str[5] - '0') * 10 +
	     (str[6] - '0'));

  int day = ((str[8] - '0') * 10 +
	     (str[9] - '0'));

  return moment_t(boost::gregorian::date(year, mon, day));
#endif
}

} // namespace ledger
