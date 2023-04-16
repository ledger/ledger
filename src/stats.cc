/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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

#include "draft.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "report.h"
#include "session.h"

namespace ledger {

value_t report_statistics(call_scope_t& args)
{
  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  const account_t::xdata_t::details_t&
    statistics(report.session.journal->master->family_details(true));

  if (! is_valid(statistics.earliest_post) &&
      ! is_valid(statistics.latest_post))
    return NULL_VALUE;

  assert(is_valid(statistics.earliest_post));
  assert(is_valid(statistics.latest_post));

  const int days = (statistics.latest_post - statistics.earliest_post).days();
  // TRANSLATORS:
  // %1% signifies the start date,
  // %2% signifies the end date
  // %3% signifies the number of days from start to end date.
  out << _fn("Time period: %1% to %2% (%3% day)",
             "Time period: %1% to %2% (%3% days)",
             days)
             % format_date(statistics.earliest_post)
             % format_date(statistics.latest_post)
             % days
      << std::endl << std::endl;

  const char* indent = "  ";
  out << indent << _("Files these postings came from:") << std::endl;
  foreach (const path& pathname, statistics.filenames)
    if (! pathname.empty())
      out << indent << indent << pathname.string() << std::endl;
  out << std::endl;

  out << indent << _f("Unique payees:          %|1$6|")
          % statistics.payees_referenced.size() << std::endl;

  out << indent << _f("Unique accounts:        %|1$6|")
          % statistics.accounts_referenced.size() << std::endl;

  out << std::endl;

  out << indent << _f("Number of postings:     %|1$6| (%|2$.2| per day)")
    % statistics.posts_count
    % (double(statistics.posts_count)/double(days))
    << std::endl;

  out << indent << _f("Uncleared postings:     %|1$6|")
          % (statistics.posts_count - statistics.posts_cleared_count)
      << std::endl;

  out << std::endl;

  out << indent << _f("Days since last post:   %|1$6|")
          % (CURRENT_DATE() - statistics.latest_post).days()
      << std::endl;

  out << indent << _f("Posts in last 7 days:   %|1$6|") % statistics.posts_last_7_count
      << std::endl;

  out << indent << _f("Posts in last 30 days:  %|1$6|") % statistics.posts_last_30_count
      << std::endl;

  out << indent << _f("Posts seen this month:  %|1$6|")
          % statistics.posts_this_month_count
      << std::endl;

  out.flush();

  return NULL_VALUE;
}

} // namespace ledger
