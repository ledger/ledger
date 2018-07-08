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

  out << format(_f("Time period: %1% to %2% (%3% days)")
                % format_date(statistics.earliest_post)
                % format_date(statistics.latest_post)
                % (statistics.latest_post -
                   statistics.earliest_post).days())
      << std::endl << std::endl;

  out << _("  Files these postings came from:") << std::endl;

  foreach (const path& pathname, statistics.filenames)
    if (! pathname.empty())
      out << "    " << pathname.string() << std::endl;
  out << std::endl;

  out << _("  Unique payees:          ");
  out.width(6);
  out << statistics.payees_referenced.size() << std::endl;

  out << _("  Unique accounts:        ");
  out.width(6);
  out << statistics.accounts_referenced.size() << std::endl;

  out << std::endl;

  out << _("  Number of postings:     ");
  out.width(6);
  out << statistics.posts_count;

  out << " (";
  out.precision(2);
  out << (double(statistics.posts_count)/
          double((statistics.latest_post - statistics.earliest_post).days()))
      << _(" per day)") << std::endl;

  out << _("  Uncleared postings:     ");
  out.width(6);
  out << (statistics.posts_count -
                        statistics.posts_cleared_count) << std::endl;

  out << std::endl;

  out << _("  Days since last post:   ");
  out.width(6);
  out << (CURRENT_DATE() - statistics.latest_post).days()
      << std::endl;

  out << _("  Posts in last 7 days:   ");
  out.width(6);
  out << statistics.posts_last_7_count << std::endl;
  out << _("  Posts in last 30 days:  ");
  out.width(6);
  out << statistics.posts_last_30_count << std::endl;
  out << _("  Posts seen this month:  ");
  out.width(6);
  out << statistics.posts_this_month_count << std::endl;

  out.flush();

  return NULL_VALUE;
}

} // namespace ledger
