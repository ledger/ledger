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

#include "ptree.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "session.h"
#include "report.h"

namespace ledger {

namespace {
  bool account_visited_p(const account_t& acct) {
    return ((acct.has_xdata() &&
            acct.xdata().has_flags(ACCOUNT_EXT_VISITED)) ||
            acct.children_with_flags(ACCOUNT_EXT_VISITED));
  }
}

void format_ptree::flush()
{
  std::ostream& out(report.output_stream);

  property_tree::ptree pt;

  pt.put("ledger.<xmlattr>.version",
         lexical_cast<string>((Ledger_VERSION_MAJOR << 16) |
                              (Ledger_VERSION_MINOR << 8) |
                              Ledger_VERSION_PATCH));

  property_tree::ptree& ct(pt.put("ledger.commodities", ""));
  foreach (const commodities_pair& pair, commodities)
    put_commodity(ct.add("commodity", ""), *pair.second, true);

  property_tree::ptree& at(pt.put("ledger.accounts", ""));
  put_account(at.add("account", ""), *report.session.journal->master, account_visited_p);

  property_tree::ptree& tt(pt.put("ledger.transactions", ""));
  foreach (const xact_t * xact, transactions) {
    property_tree::ptree& t(tt.add("transaction", ""));
    put_xact(t, *xact);

    property_tree::ptree& post_tree(t.put("postings", ""));
    foreach (const post_t * post, xact->posts)
      if (post->has_xdata() &&
          post->xdata().has_flags(POST_EXT_VISITED))
        put_post(post_tree.add("posting", ""), *post);
  }

  switch (format) {
  case FORMAT_XML:
#if BOOST_VERSION >= 105600
    auto indented = property_tree::xml_writer_make_settings<std::string> (' ', 2);
#else
    property_tree::xml_writer_settings<char> indented(' ', 2);
#endif

    property_tree::write_xml(out, pt, indented);
    out << std::endl;
    break;
  }
}

void format_ptree::operator()(post_t& post)
{
  assert(post.xdata().has_flags(POST_EXT_VISITED));

  commodities.insert(commodities_pair(post.amount.commodity().symbol(),
                                      &post.amount.commodity()));

  std::pair<std::set<xact_t *>::iterator, bool> result =
    transactions_set.insert(post.xact);
  if (result.second)            // we haven't seen this transaction before
    transactions.push_back(post.xact);
}

} // namespace ledger
