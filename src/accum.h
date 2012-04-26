/*
 * Copyright (c) 2003-2012, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
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
 * @addtogroup util
 */

/**
 * @file   accum.h
 * @author John Wiegley
 *
 * @ingroup util
 */
#ifndef _ACCUM_H
#define _ACCUM_H

namespace ledger {

class straccbuf : public std::streambuf
{
protected:
  std::string            str;   // accumulator
  std::string::size_type index;

public:
  straccbuf() : index(0) {
    TRACE_CTOR(straccbuf, "");
  }
  ~straccbuf() throw() {
    TRACE_DTOR(straccbuf);
  }

protected:
  virtual std::streamsize xsputn(const char * s, std::streamsize num);

  friend class straccstream;
};

class straccstream : public std::ostream
{
protected:
  straccbuf buf;

public:
  straccstream() : std::ostream(0) {
    TRACE_CTOR(straccstream, "");
    rdbuf(&buf);
  }
  ~straccstream() throw() {
    TRACE_DTOR(straccstream);
  }

  void clear() {
    std::ostream::clear();
    buf.pubseekoff(0, ios_base::beg);
    buf.str.clear();
    buf.index = 0;
  }

  std::string str() const {
    return buf.str;
  }
};

#define ACCUM(obj) (static_cast<const straccstream&>(obj).str())

extern straccstream       _accum;
extern std::ostringstream _accum_buffer;

inline string str_helper_func() {
  string buf = _accum_buffer.str();
  _accum_buffer.clear();
  _accum_buffer.str("");
  return buf;
}

#define STR(msg)                                \
  ((_accum_buffer << ACCUM(_accum << msg)),     \
   _accum.clear(), str_helper_func())

} // namespace ledger

#endif // _ACCUM_H
