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

#include "xml.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "session.h"
#include "report.h"

namespace ledger {

namespace {
  void xml_account(std::ostream& out, const account_t * acct) {
    if ((acct->has_xdata() &&
         acct->xdata().has_flags(ACCOUNT_EXT_VISITED)) ||
        acct->children_with_flags(ACCOUNT_EXT_VISITED)) {
      out << "<account id=\"";
      out.width(sizeof(unsigned long) * 2);
      out.fill('0');
      out << std::hex << reinterpret_cast<unsigned long>(acct);
      out << "\">\n";

      out << "<name>" << acct->name << "</name>\n";
      value_t total = acct->amount();
      if (! total.is_null()) {
        out << "<amount>\n";
        to_xml(out, total);
        out << "</amount>\n";
      }
      total = acct->total();
      if (! total.is_null()) {
        out << "<total>\n";
        to_xml(out, total);
        out << "</total>\n";
      }
      out << "</account>\n";
    }

    foreach (const accounts_map::value_type& pair, acct->accounts)
      xml_account(out, pair.second);
  }

  void xml_transaction(std::ostream& out, const xact_t * xact) {
    to_xml(out, *xact);

    foreach (const post_t * post, xact->posts)
      if (post->has_xdata() &&
          post->xdata().has_flags(POST_EXT_VISITED))
        to_xml(out, *post);

    out << "</transaction>\n";
  }
}

void format_xml::flush()
{
  std::ostream& out(report.output_stream);

  out << "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";
  out << "<ledger version=\"" << VERSION << "\">\n";

  out << "<commodities>\n";
  foreach (const commodities_pair& pair, commodities) {
    to_xml(out, *pair.second, true);
    out << '\n';
  }
  out << "</commodities>\n";

  out << "<accounts>\n";
  xml_account(out, report.session.journal->master);
  out << "</accounts>\n";

  out << "<transactions>\n";
  foreach (const xact_t * xact, transactions)
    xml_transaction(out, xact);
  out << "</transactions>\n";

  out << "</ledger>\n";
  out.flush();
}

void format_xml::operator()(post_t& post)
{
  assert(post.xdata().has_flags(POST_EXT_VISITED));

  commodities.insert(commodities_pair(post.amount.commodity().symbol(),
                                      &post.amount.commodity()));

  if (transactions_set.find(post.xact) == transactions_set.end())
    transactions.push_back(post.xact);
}

} // namespace ledger
