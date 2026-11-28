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

#include <system.hh>

#include "stats.h"

#include "post.h"
#include "report.h"
#include "xact.h"

namespace ledger {

void report_statistics::operator()(post_t& post) {
  statistics.update(post, today, /* gather_all= */ true);
}

void report_statistics::flush() {
  std::ostream& out(report.output_stream);

  if (!is_valid(statistics.earliest_post) && !is_valid(statistics.latest_post))
    return;

  assert(is_valid(statistics.earliest_post));
  assert(is_valid(statistics.latest_post));

  // Use inclusive day count: a single-day period spans 1 day, not 0.
  // This also prevents division by zero when computing the per-day rate
  // for journals whose transactions all fall on the same date.
  long period_days = (statistics.latest_post - statistics.earliest_post).days() + 1;

  out << format(_f("Time period: %1% to %2% (%3% days)") % format_date(statistics.earliest_post) %
                format_date(statistics.latest_post) % period_days)
      << '\n'
      << '\n';

  out << _("  Files these postings came from:") << '\n';

  for (const path& pathname : statistics.filenames)
    if (!pathname.empty())
      out << "    " << pathname.string() << '\n';
  out << '\n';

  out << _("  Unique payees:          ");
  out.width(6);
  out << statistics.payees_referenced.size() << '\n';

  out << _("  Unique accounts:        ");
  out.width(6);
  out << statistics.accounts_referenced.size() << '\n';

  out << '\n';

  out << _("  Number of postings:     ");
  out.width(6);
  out << statistics.posts_count;

  // Format the rate on its own stream so the report stream's precision
  // is untouched.  Two significant digits suit small rates (0.018, 0.17,
  // 2); from ten upward print whole numbers, never scientific notation.
  const double per_day = double(statistics.posts_count) / double(period_days);
  std::ostringstream rate;
  if (per_day >= 10)
    rate << std::fixed << std::setprecision(0);
  else
    rate << std::setprecision(2);
  rate << per_day;
  out << " (" << rate.str() << _(" per day)") << '\n';

  out << _("  Uncleared postings:     ");
  out.width(6);
  out << (statistics.posts_count - statistics.posts_cleared_count) << '\n';

  out << '\n';

  // Use latest_past_post (most recent non-future posting) so that
  // future-dated transactions don't yield negative "days since" values.
  // Fall back to latest_post only if every posting is in the future.
  out << _("  Days since last post:   ");
  out.width(6);
  out << (today - (is_valid(statistics.latest_past_post) ? statistics.latest_past_post
                                                         : statistics.latest_post))
             .days()
      << '\n';

  out << _("  Posts in last 7 days:   ");
  out.width(6);
  out << statistics.posts_last_7_count << '\n';
  out << _("  Posts in last 30 days:  ");
  out.width(6);
  out << statistics.posts_last_30_count << '\n';
  out << _("  Posts seen this month:  ");
  out.width(6);
  out << statistics.posts_this_month_count << '\n';

  out.flush();
}

} // namespace ledger
