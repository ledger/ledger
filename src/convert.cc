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

/**
 * @file   convert.cc
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Implementation of the `ledger convert` CSV import command.
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

/*--- CSV import entry point ---*/

value_t convert_command(call_scope_t& args) {
  report_t& report(args.context<report_t>());
  journal_t& journal(*report.session.journal.get());

  // Determine the balancing account.  The --account option specifies which
  // account the CSV amounts come from (e.g. "Assets:Checking").  If not
  // given, a placeholder "Equity:Unknown" is used.
  string bucket_name;
  if (report.HANDLED(account_))
    bucket_name = report.HANDLER(account_).str();
  else
    bucket_name = _("Equity:Unknown");

  account_t* bucket = journal.master->find_account(bucket_name);
  account_t* unknown = journal.master->find_account(_("Expenses:Unknown"));

  // Snapshot existing journal transactions for payee-based account lookup.
  xacts_list current_xacts(journal.xacts_begin(), journal.xacts_end());

  /*--- CSV parsing and transaction creation ---*/

  print_xacts formatter(report);
  path csv_file_path(args.get<string>(0));

  report.session.parsing_context.push(csv_file_path);
  parse_context_t& context(report.session.parsing_context.get_current());
  context.journal = &journal;
  context.master = bucket;
  context.scope = &report.session;

  // RAII guard to set and restore journal.current_context during CSV parsing.
  // This ensures metadata checks and warnings have a valid parse context.
  struct current_context_guard {
    journal_t& journal;
    parse_context_t* saved;
    current_context_guard(journal_t& j, parse_context_t* current)
        : journal(j), saved(j.current_context) {
      journal.current_context = current;
    }
    ~current_context_guard() { journal.current_context = saved; }
  } guard(journal, &context);

  char separator = ',';
  if (report.HANDLED(csv_separator_)) {
    const string& sep = report.HANDLER(csv_separator_).str();
    if (sep == "\\t" || sep == "tab")
      separator = '\t';
    else if (!sep.empty())
      separator = sep[0];
  }

  csv_reader reader(context, separator);

  try {
    while (xact_t* xact = reader.read_xact(report.HANDLED(rich_data))) {
      // Step 1: Optionally negate amounts (--invert), useful for
      // credit-card CSVs where debits are positive.
      if (report.HANDLED(invert)) {
        for (post_t* post : xact->posts) {
          post->amount.in_place_negate();
          if (post->cost)
            post->cost->in_place_negate();
          if (post->given_cost)
            post->given_cost->in_place_negate();
        }
      }

      // Step 2: Compute a unique reference for duplicate detection.
      // Prefer an explicit UUID tag; fall back to SHA-1 of the raw CSV line.
      const string ref = xact->has_tag(_("UUID")) ? xact->get_tag(_("UUID"))->to_string()
                                                  : sha1sum(reader.get_last_line());

      // Skip this transaction if its reference already exists in the journal.
      if (auto entry = journal.checksum_map.find(ref); entry != journal.checksum_map.end()) {
        INFO(file_context(reader.get_pathname(), reader.get_linenum())
             << " " << "Ignoring known UUID " << ref);
        checked_delete(xact); // ignore it
        continue;
      }

      if (report.HANDLED(rich_data) && !xact->has_tag(_("UUID")))
        xact->set_tag(_("UUID"), string_value(ref));

      // Step 2b: Apply UUID-to-payee mapping.  A journal `payee` directive
      // can declare a UUID that identifies this transaction (computed as the
      // SHA-1 of the CSV line).  When found, override the CSV description with
      // the canonical payee name so that subsequent payee-to-account mappings
      // (from `account` directives) can match correctly.
      {
        auto uuid_it = journal.payee_uuid_mappings.find(ref);
        if (uuid_it != journal.payee_uuid_mappings.end()) {
          xact->payee = uuid_it->second;
          // Re-apply payee-to-account mapping with the resolved payee name
          // so that `account` directives with a matching `payee` sub-directive
          // take effect even though the CSV reader ran before we knew the payee.
          if (xact->posts.front()->account == nullptr) {
            for (account_mapping_t& value : journal.payees_for_unknown_accounts) {
              if (value.first.match(xact->payee)) {
                xact->posts.front()->account = value.second;
                break;
              }
            }
          }
        }
      }

      // Step 3: Resolve the expense account.  With --auto-match, search
      // existing transactions for a payee match and reuse its account.
      if (xact->posts.front()->account == nullptr) {
        if (account_t* acct = (report.HANDLED(auto_match)
                                   ? lookup_probable_account(xact->payee, current_xacts.rbegin(),
                                                             current_xacts.rend(), bucket)
                                         .second
                                   : NULL))
          xact->posts.front()->account = acct;
        else
          xact->posts.front()->account = unknown;
      }

      // Step 4: Finalize the transaction and print it in Ledger format.
      if (!journal.add_xact(xact)) {
        checked_delete(xact);
        throw_(std::runtime_error, _("Failed to finalize derived transaction (check commodities)"));
      } else {
        xact_posts_iterator xact_iter(*xact);
        while (post_t* post = *xact_iter++)
          formatter(*post);
      }
    }
    formatter.flush();
  } catch (const std::exception&) {
    add_error_context(_f("While parsing file %1%") %
                      file_context(reader.get_pathname(), reader.get_linenum()));
    add_error_context(_("While parsing CSV line:"));
    add_error_context(line_context(reader.get_last_line()));
    throw;
  }

  // If not, transform the payee according to regexps

  // Set the account to a default value, then transform the account according
  // to the payee

  // Print out the final form of the transaction

  return true;
}

} // namespace ledger
