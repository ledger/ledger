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

#include "csv.h"

namespace ledger {

namespace {
  inline void write_escaped_string(std::ostream& out, const string& xact)
  {
    out << "\"";
    foreach (char ch, xact)
      if (ch == '"') {
	out << "\\";
	out << "\"";
      } else {
	out << ch;
      }
    out << "\"";
  }
}

void format_csv_xacts::operator()(xact_t& xact)
{
  if (! xact.has_xdata() ||
      ! xact.xdata().has_flags(XACT_EXT_DISPLAYED)) {
    {
      format_t fmt("%D");
      std::ostringstream str;
#if 0
      fmt.format(str, details_t(xact));
#endif
      write_escaped_string(out, str.str());
    }
    out << ',';

    {
      format_t fmt("%P");
      std::ostringstream str;
#if 0
      fmt.format(str, details_t(xact));
#endif
      write_escaped_string(out, str.str());
    }
    out << ',';

    {
      format_t fmt("%A");
      std::ostringstream str;
#if 0
      fmt.format(str, details_t(xact));
#endif
      write_escaped_string(out, str.str());
    }
    out << ',';

    {
      format_t fmt("%t");
      std::ostringstream str;
#if 0
      fmt.format(str, details_t(xact));
#endif
      write_escaped_string(out, str.str());
    }
    out << ',';

    {
      format_t fmt("%T");
      std::ostringstream str;
#if 0
      fmt.format(str, details_t(xact));
#endif
      write_escaped_string(out, str.str());
    }
    out << ',';

    switch (xact.state()) {
    case item_t::CLEARED:
      write_escaped_string(out, "*");
      break;
    case item_t::PENDING:
      write_escaped_string(out, "!");
      break;
    default:
      break;
    }
    out << ',';

    if (xact.entry->code)
      write_escaped_string(out, *xact.entry->code);
    out << ',';

    {
      format_t fmt("%N");
      std::ostringstream str;
#if 0
      fmt.format(str, details_t(xact));
#endif
      write_escaped_string(out, str.str());
    }
    out << '\n';

    xact.xdata().add_flags(XACT_EXT_DISPLAYED);
  }
}

} // namespace ledger
