/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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
 * @addtogroup utils
 */

/**
 * @file   unistring.h
 * @author John Wiegley
 *
 * @ingroup utils
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _UNISTRING_H
#define _UNISTRING_H

#include "utils.h"

namespace ledger {

/**
 * @class unistring
 *
 * @brief Abstract working with UTF-32 encoded Unicode strings
 *
 * The input to the string is a UTF8 encoded ledger::string, which can
 * then have its true length be taken, or characters extracted.
 */
class unistring
{
  std::vector<boost::uint32_t> utf32chars;

public:
  unistring(const std::string& input)
  {
    TRACE_CTOR(unistring, "");

    const char * p   = input.c_str();
    std::size_t	 len = input.length();

    assert(utf8::is_valid(p, p + len));
    utf8::utf8to32(p, p + len, std::back_inserter(utf32chars));
  }
  ~unistring() {
    TRACE_DTOR(unistring);
  }

  std::size_t length() const {
    return utf32chars.size();
  }

  std::string extract(const std::size_t begin = 0,
		      const std::size_t len   = 0) const
  {
    std::string utf8result;
    std::size_t this_len = length();
    assert(begin <= this_len);
    assert(begin + len <= this_len);
    if (this_len)
      utf8::utf32to8(utf32chars.begin() + begin,
		     utf32chars.begin() + begin +
		     (len ? (len > this_len ? this_len : len) : this_len),
		     std::back_inserter(utf8result));
    return utf8result;
  }
};

inline void justify(std::ostream&      out,
		    const std::string& str,
		    int		       width,
		    bool               right = false)
{
  if (! right)
    out << str;

  unistring temp(str);

  int spacing = width - int(temp.length());
  while (spacing-- > 0)
    out << ' ';

  if (right)
    out << str;
}

} // namespace ledger

#endif // _UNISTRING_H
