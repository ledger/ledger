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

#include "convert.h"
#include "csv.h"
#include "scope.h"
#include "iterators.h"
#include "report.h"
#include "xact.h"
#include "print.h"
#include "lookup.h"

namespace ledger {

value_t convert_command(call_scope_t& args)
{
  report_t&  report(args.context<report_t>());
  journal_t& journal(*report.session.journal.get());

  string bucket_name;
  if (report.HANDLED(account_))
    bucket_name = report.HANDLER(account_).str();
  else
    bucket_name = _("Equity:Unknown");

  account_t * bucket  = journal.master->find_account(bucket_name);
  account_t * unknown = journal.master->find_account(_("Expenses:Unknown"));

  // Create a flat list
  xacts_list current_xacts(journal.xacts_begin(), journal.xacts_end());

  // Read in the series of transactions from the CSV file

  print_xacts formatter(report);
  path        csv_file_path(args.get<string>(0));

  report.session.parsing_context.push(csv_file_path);
  parse_context_t& context(report.session.parsing_context.get_current());
  context.journal = &journal;
  context.master  = bucket;

  csv_reader reader(context);

  try {
    while (xact_t * xact = reader.read_xact(report.HANDLED(rich_data))) {
      if (report.HANDLED(invert)) {
        foreach (post_t * post, xact->posts)
          post->amount.in_place_negate();
      }

      string ref = (xact->has_tag(_("UUID")) ?
                    xact->get_tag(_("UUID"))->to_string() :
                    sha1sum(reader.get_last_line()));

      checksum_map_t::const_iterator entry = journal.checksum_map.find(ref);
      if (entry != journal.checksum_map.end()) {
        INFO(file_context(reader.get_pathname(),
                          reader.get_linenum())
             << " " << "Ignoring known UUID " << ref);
        checked_delete(xact);     // ignore it
        continue;
      }

      if (report.HANDLED(rich_data) && ! xact->has_tag(_("UUID")))
        xact->set_tag(_("UUID"), string_value(ref));

      if (xact->posts.front()->account == NULL) {
        if (account_t * acct =
            (report.HANDLED(auto_match) ?
             lookup_probable_account(xact->payee, current_xacts.rbegin(),
                                     current_xacts.rend(), bucket).second :
             NULL))
          xact->posts.front()->account = acct;
        else
          xact->posts.front()->account = unknown;
      }

      if (! journal.add_xact(xact)) {
        checked_delete(xact);
        throw_(std::runtime_error,
               _("Failed to finalize derived transaction (check commodities)"));
      }
      else {
        xact_posts_iterator xact_iter(*xact);
        while (post_t * post = *xact_iter++)
          formatter(*post);
      }
    }
    formatter.flush();
  }
  catch (const std::exception&) {
    add_error_context(_f("While parsing file %1%")
                      % file_context(reader.get_pathname(),
                                      reader.get_linenum()));
    add_error_context(_("While parsing CSV line:"));
    add_error_context(line_context(reader.get_last_line()));
    throw;
  }

  // If not, transform the payee according to regexps

  // Set the account to a default vaule, then transform the account according
  // to the payee

  // Print out the final form of the transaction

  return true;
}

} // namespace ledger
