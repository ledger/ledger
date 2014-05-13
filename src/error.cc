/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
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

#include "utils.h"

namespace ledger {

std::ostringstream _ctxt_buffer;
std::ostringstream _desc_buffer;

string error_context()
{
  string context = _ctxt_buffer.str();
  _ctxt_buffer.clear();
  _ctxt_buffer.str("");
  return context;
}

string file_context(const path& file, const std::size_t line)
{
  std::ostringstream buf;
  buf << '"' << file.string() << "\", line " << line << ":";
  return buf.str();
}

string line_context(const string&           line,
                    const string::size_type pos,
                    const string::size_type end_pos)
{
  std::ostringstream buf;
  buf << "  " << line << "\n";

  if (pos != 0) {
    buf << "  ";
    if (end_pos == 0) {
      for (string::size_type i = 0; i < pos; i += 1)
        buf << " ";
      buf << "^";
    } else {
      for (string::size_type i = 0; i < end_pos; i += 1) {
        if (i >= pos)
          buf << "^";
        else
          buf << " ";
      }
    }
  }
  return buf.str();
}

string source_context(const path&            file,
                      const istream_pos_type pos,
                      const istream_pos_type end_pos,
                      const string&          prefix)
{
  const std::streamoff len = end_pos - pos;
  if (! len || file.empty())
    return _("<no source context>");

  assert(len > 0);
  assert(len < 8192);

  std::ostringstream out;

  ifstream in(file);
  in.seekg(pos, std::ios::beg);

  scoped_array<char> buf(new char[static_cast<std::size_t>(len) + 1]);
  in.read(buf.get(), static_cast<std::streamsize>(len));
  assert(in.gcount() == static_cast<std::streamsize>(len));
  buf[static_cast<std::ptrdiff_t>(len)] = '\0';

  bool first = true;
  for (char * p = std::strtok(buf.get(), "\n");
       p;
       p = std::strtok(NULL, "\n")) {
    if (first)
      first = false;
    else
      out << '\n';
    out << prefix << p;
  }

  return out.str();
}

} // namespace ledger
