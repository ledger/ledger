/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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

#include "emacs.h"

namespace ledger {

void format_emacs_xacts::write_entry(entry_t& entry)
{
  int idx = entry.src_idx;
  foreach (const path& path, entry.journal->sources)
    if (! idx--) {
      out << "\"" << path << "\" ";
      break;
    }

  out << (static_cast<unsigned long>(entry.beg_line) + 1) << " ";

  tm when = gregorian::to_tm(entry.date());
  std::time_t date = std::mktime(&when); // jww (2008-04-20): Is this GMT or local?

  out << "(" << (date / 65536) << " " << (date % 65536) << " 0) ";

  if (! entry.code)
    out << "nil ";
  else
    out << "\"" << *entry.code << "\" ";

  if (entry.payee.empty())
    out << "nil";
  else
    out << "\"" << entry.payee << "\"";

  out << "\n";
}

void format_emacs_xacts::operator()(xact_t& xact)
{
  if (! xact.has_xdata() ||
      ! xact.xdata().has_flags(XACT_EXT_DISPLAYED)) {
    if (! last_entry) {
      out << "((";
      write_entry(*xact.entry);
    }
    else if (xact.entry != last_entry) {
      out << ")\n (";
      write_entry(*xact.entry);
    }
    else {
      out << "\n";
    }

    out << "  (" << (static_cast<unsigned long>(xact.beg_line) + 1) << " ";
    out << "\"" << xact.reported_account()->fullname() << "\" \""
	<< xact.amount << "\"";

    switch (xact.state) {
    case xact_t::CLEARED:
      out << " t";
      break;
    case xact_t::PENDING:
      out << " pending";
      break;
    default:
      out << " nil";
      break;
    }

    if (xact.cost)
      out << " \"" << *xact.cost << "\"";
    if (xact.note)
      out << " \"" << *xact.note << "\"";
    out << ")";

    last_entry = xact.entry;

    xact.xdata().add_flags(XACT_EXT_DISPLAYED);
  }
}

} // namespace ledger
