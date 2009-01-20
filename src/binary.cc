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

#include "binary.h"

namespace ledger {
namespace binary {

void read_bool(std::istream& in, bool& num)
{
  read_guard(in, 0x2005);
  unsigned char val;
  in.read(reinterpret_cast<char *>(&val), sizeof(val));
  num = val == 1;
  read_guard(in, 0x2006);
}

void read_bool(const char *& data, bool& num)
{
  read_guard(data, 0x2005);
  const unsigned char val = *reinterpret_cast<const unsigned char *>(data);
  data += sizeof(unsigned char);
  num = val == 1;
  read_guard(data, 0x2006);
}

void read_string(std::istream& in, string& str)
{
  read_guard(in, 0x3001);

  unsigned char len;
  read_number_nocheck(in, len);
  if (len == 0xff) {
    unsigned short slen;
    read_number_nocheck(in, slen);
    char * buf = new char[slen + 1];
    in.read(buf, slen);
    buf[slen] = '\0';
    str = buf;
    checked_array_delete(buf);
  }
  else if (len) {
    char buf[256];
    in.read(buf, len);
    buf[len] = '\0';
    str = buf;
  } else {
    str = "";
  }

  read_guard(in, 0x3002);
}

void read_string(const char *& data, string& str)
{
  read_guard(data, 0x3001);

  unsigned char len;
  read_number_nocheck(data, len);
  if (len == 0xff) {
    unsigned short slen;
    read_number_nocheck(data, slen);
    str = string(data, slen);
    data += slen;
  }
  else if (len) {
    str = string(data, len);
    data += len;
  }
  else {
    str = "";
  }

  read_guard(data, 0x3002);
}

void read_string(const char *& data, string * str)
{
  read_guard(data, 0x3001);

  unsigned char len;
  read_number_nocheck(data, len);
  if (len == 0xff) {
    unsigned short slen;
    read_number_nocheck(data, slen);
    new(str) string(data, slen);
    data += slen;
  }
  else if (len) {
    new(str) string(data, len);
    data += len;
  }
  else {
    new(str) string("");
  }

  read_guard(data, 0x3002);
}

void read_string(std::istream& in, optional<string>& str)
{
  if (read_bool(in)) {
    string temp;
    read_string(in, temp);
    str = temp;
  } else {
    str = none;
  }
}

void read_string(const char *& data, optional<string>& str)
{
  if (read_bool(data)) {
    string temp;
    read_string(data, temp);
    str = temp;
  } else {
    str = none;
  }
}

void write_bool(std::ostream& out, bool num)
{
  write_guard(out, 0x2005);
  unsigned char val = num ? 1 : 0;
  out.write(reinterpret_cast<char *>(&val), sizeof(val));
  write_guard(out, 0x2006);
}

void write_string(std::ostream& out, const string& str)
{
  write_guard(out, 0x3001);

  unsigned long len = str.length();
  if (len > 255) {
    assert(len < 65536);
    write_number_nocheck<unsigned char>(out, 0xff);
    write_number_nocheck<unsigned short>(out, len);
  } else {
    write_number_nocheck<unsigned char>(out, len);
  }

  if (len)
    out.write(str.c_str(), len);

  write_guard(out, 0x3002);
}

void write_string(std::ostream& out, const optional<string>& str)
{
  if (str) {
    write_bool(out, true);
    write_string(out, *str);
  } else {
    write_bool(out, false);
  }
}

} // namespace binary
} // namespace ledger
