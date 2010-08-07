/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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

  // Make an amounts mapping for the account under consideration

  typedef std::map<value_t, std::list<post_t *> > post_map_t;
  post_map_t post_map;

  xacts_iterator journal_iter(journal);
  while (xact_t * xact = journal_iter()) {
    post_t * post = NULL;
    xact_posts_iterator xact_iter(*xact);
    while ((post = xact_iter()) != NULL) {
      if (post->account == bucket)
        break;
    }
    if (post) {
      post_map_t::iterator i = post_map.find(post->amount);
      if (i == post_map.end()) {
        std::list<post_t *> post_list;
        post_list.push_back(post);
        post_map.insert(post_map_t::value_type(post->amount, post_list));
      } else {
        (*i).second.push_back(post);
      }
    }
  }

  // Create a flat list
  xacts_list current_xacts(journal.xacts_begin(), journal.xacts_end());

  // Read in the series of transactions from the CSV file

  print_xacts formatter(report);
  ifstream    data(path(args.get<string>(0)));
  csv_reader  reader(data);

  while (xact_t * xact = reader.read_xact(journal, bucket)) {
    if (report.HANDLED(invert)) {
      foreach (post_t * post, xact->posts)
        post->amount.in_place_negate();
    }
      
    bool matched = false;
    if (! xact->posts.front()->amount.is_null()) {
      post_map_t::iterator i = post_map.find(- xact->posts.front()->amount);
      if (i != post_map.end()) {
        std::list<post_t *>& post_list((*i).second);
        foreach (post_t * post, post_list) {
          if (xact->code && post->xact->code &&
              *xact->code == *post->xact->code) {
            matched = true;
            break;
          }
          else if (xact->actual_date() == post->actual_date()) {
            matched = true;
            break;
          }
        }
      }
    }

    if (matched) {
      DEBUG("convert.csv", "Ignored xact with code: " << *xact->code);
      checked_delete(xact);     // ignore it
    }
    else {
      if (xact->posts.front()->account == NULL) {
        xacts_iterator xi;
        xi.xacts_i   = current_xacts.begin();
        xi.xacts_end = current_xacts.end();
        xi.xacts_uninitialized = false;

        // jww (2010-03-07): Bind this logic to an option: --auto-match
        if (account_t * acct =
            lookup_probable_account(xact->payee, xi, bucket).second)
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
        while (post_t * post = xact_iter())
          formatter(*post);
      }
    }
  }
  formatter.flush();

  // If not, transform the payee according to regexps

  // Set the account to a default vaule, then transform the account according
  // to the payee

  // Print out the final form of the transaction

  return true;
}

} // namespace ledger
