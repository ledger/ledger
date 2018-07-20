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
 * @file   error.h
 * @author John Wiegley
 *
 * @ingroup util
 */
#ifndef _ERROR_H
#define _ERROR_H

namespace ledger {

extern std::ostringstream _desc_buffer;

template <typename T>
inline void throw_func(const string& message) {
  _desc_buffer.clear();
  _desc_buffer.str("");
  throw T(message);
}

#define throw_(cls, msg)                        \
  ((_desc_buffer << (msg)),                     \
   throw_func<cls>(_desc_buffer.str()))

inline void warning_func(const string& message) {
  std::cerr << "Warning: " << message << std::endl;
  _desc_buffer.clear();
  _desc_buffer.str("");
}

#define warning_(msg)                           \
  ((_desc_buffer << (msg)),                     \
   warning_func(_desc_buffer.str()))

extern std::ostringstream _ctxt_buffer;

#define add_error_context(msg)                  \
  ((long(_ctxt_buffer.tellp()) == 0) ?          \
   (_ctxt_buffer << (msg)) :                    \
   (_ctxt_buffer << std::endl << (msg)))

string error_context();

string file_context(const path& file, std::size_t line);
string line_context(const string&           line,
                    const string::size_type pos     = 0,
                    const string::size_type end_pos = 0);

string source_context(const path&            file,
                      const istream_pos_type pos,
                      const istream_pos_type end_pos,
                      const string&          prefix = "");

#define DECLARE_EXCEPTION(name, kind)                           \
  class name : public kind {                                    \
  public:                                                       \
  explicit name(const string& why) throw() : kind(why) {}       \
  virtual ~name() throw() {}                                    \
  }

struct error_count {
  std::size_t count;
  std::string message;
  explicit error_count(std::size_t _count, std::string _msg) : count(_count), message(_msg) {}
  const char * what() const { return message.c_str(); }
};

} // namespace ledger

#endif // _ERROR_H
