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

#include "mask.h"

namespace ledger {

mask_t::mask_t(const string& pat) : expr()
{
  *this = pat;
  TRACE_CTOR(mask_t, "const string&");
}

mask_t& mask_t::operator=(const string& pat)
{
#if HAVE_BOOST_REGEX_UNICODE
  expr = boost::make_u32regex(pat.c_str(), boost::regex::perl | boost::regex::icase);
#else
  expr.assign(pat.c_str(), boost::regex::perl | boost::regex::icase);
#endif
  VERIFY(valid());
  return *this;
}

mask_t& mask_t::assign_glob(const string& pat)
{
  string re_pat = "";
  string::size_type len = pat.length();
  for (string::size_type i = 0; i < len; i++) {
    switch (pat[i]) {
    case '?':
      re_pat += '.';
      break;
    case '*':
      re_pat += ".*";
      break;
    case '[':
      while (i < len && pat[i] != ']')
        re_pat += pat[i++];
      if (i < len)
        re_pat += pat[i];
      break;

    case '\\':
      if (i + 1 < len) {
        re_pat += pat[++i];
        break;
      } else {
        // fallthrough...
      }
    default:
      re_pat += pat[i];
      break;
    }
  }
  return (*this = re_pat);
}

} // namespace ledger
