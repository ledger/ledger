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
 * @addtogroup util
 */

/**
 * @file   pstream.h
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief An input stream backed by an in-memory character buffer.
 *
 * `ptristream` wraps a raw `char*` buffer as a standard `std::istream`,
 * allowing existing parsing code (which expects istream references) to
 * operate on in-memory data without copying into a `std::istringstream`.
 *
 * This is used when Ledger needs to parse expressions or data that has
 * already been read into memory (e.g., from a pipe, a Python binding
 * call, or an interactive command).  The stream supports seeking
 * (`seekg`) so that the parser can backtrack when needed.
 *
 * Note: despite the file name suggesting "process stream" (popen),
 * this class is actually a pointer-to-memory input stream.  The pager
 * subprocess functionality lives in `stream.h`/`stream.cc`.
 */
#pragma once

// #include <istream>
// #include <streambuf>

namespace ledger {

/**
 * @brief An istream that reads from a raw character pointer.
 *
 * Wraps an existing `char*` buffer and length as a `std::istream`.
 * The buffer is not owned or copied -- the caller must ensure it
 * outlives the stream.  Seeking is supported (beg, cur, end).
 */
class ptristream : public std::istream {
  /// Custom streambuf that reads from a fixed memory region.
  class ptrinbuf : public std::streambuf {
    ptrinbuf(const ptrinbuf&);
    ptrinbuf& operator=(const ptrinbuf&);

  protected:
    char* ptr;       ///< Pointer to the beginning of the buffer
    std::size_t len; ///< Length of the buffer in bytes

  public:
    ptrinbuf(char* _ptr, std::size_t _len) : ptr(_ptr), len(_len) {
      if (*ptr && len == 0)
        len = std::strlen(ptr);

      setg(ptr,        // beginning of putback area
           ptr,        // read position
           ptr + len); // end position

      TRACE_CTOR(ptrinbuf, "char *, std::size_t");
    }
    ~ptrinbuf() noexcept override { TRACE_DTOR(ptrinbuf); }

  protected:
    int_type underflow() override {
      // is read position before end of buffer?
      if (gptr() < egptr())
        return traits_type::to_int_type(*gptr());
      else
        return EOF;
    }

    pos_type seekoff(off_type off, ios_base::seekdir way, ios_base::openmode) override {
      // cast to avoid gcc '-Wswitch' warning
      // as ios_base::beg/cur/end are not necessarily values of 'way' enum type ios_base::seekdir
      // based on https://svn.boost.org/trac/boost/ticket/7644
      switch (static_cast<int>(way)) {
      case std::ios::beg: {
        std::streamoff new_pos = std::max(std::streamoff(0), std::min(off, std::streamoff(len)));
        setg(ptr, ptr + new_pos, ptr + len);
        break;
      }
      case std::ios::cur: {
        std::streamoff cur_off = gptr() - ptr;
        std::streamoff new_pos =
            std::max(std::streamoff(0), std::min(cur_off + off, std::streamoff(len)));
        setg(ptr, ptr + new_pos, ptr + len);
        break;
      }
      case std::ios::end: {
        std::streamoff new_pos =
            std::max(std::streamoff(0), std::min(std::streamoff(len) + off, std::streamoff(len)));
        setg(ptr, ptr + new_pos, ptr + len);
        break;
      }
      }
      return pos_type(gptr() - ptr);
    }
  };

protected:
  ptrinbuf buf;

public:
  ptristream(char* ptr, std::size_t len = 0) : std::istream(nullptr), buf(ptr, len) { rdbuf(&buf); }
};

} // namespace ledger
