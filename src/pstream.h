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

/**
 * @addtogroup util
 */

/**
 * @file   pstream.h
 * @author John Wiegley
 *
 * @ingroup util
 */
#ifndef _PSTREAM_H
#define _PSTREAM_H

//#include <istream>
//#include <streambuf>

class ptristream : public std::istream
{
  class ptrinbuf : public std::streambuf
  {
    ptrinbuf(const ptrinbuf&);
    ptrinbuf& operator=(const ptrinbuf&);

  protected:
    char *      ptr;
    std::size_t len;

  public:
    ptrinbuf(char * _ptr, std::size_t _len) : ptr(_ptr), len(_len) {
      if (*ptr && len == 0)
        len = std::strlen(ptr);

      setg(ptr,                 // beginning of putback area
           ptr,                 // read position
           ptr+len);            // end position

      TRACE_CTOR(ptrinbuf, "char *, std::size_t");
    }
    ~ptrinbuf() throw() {
      TRACE_DTOR(ptrinbuf);
    }

  protected:
    virtual int_type underflow() {
      // is read position before end of buffer?
      if (gptr() < egptr())
        return traits_type::to_int_type(*gptr());
      else
        return EOF;
    }

    virtual pos_type seekoff(off_type off, ios_base::seekdir way,
                             ios_base::openmode)
    {
      // cast to avoid gcc '-Wswitch' warning
      // as ios_base::beg/cur/end are not necesssarily values of 'way' enum type ios_base::seekdir
      // based on https://svn.boost.org/trac/boost/ticket/7644
      switch (static_cast<int>(way)) {
      case std::ios::cur:
        setg(ptr, gptr()+off, ptr+len);
        break;
      case std::ios::beg:
        setg(ptr, ptr+off, ptr+len);
        break;
      case std::ios::end:
        setg(ptr, egptr()+off, ptr+len);
        break;
      }
      return pos_type(gptr() - ptr);
    }
  };

protected:
  ptrinbuf buf;

public:
  ptristream(char * ptr, std::size_t len = 0)
    : std::istream(0), buf(ptr, len) {
    rdbuf(&buf);
  }
};

#endif // _PSTREAM_H
