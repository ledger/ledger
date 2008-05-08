/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#ifndef BINARY_H
#define BINARY_H

#include "parser.h"

namespace ledger {

class journal_t;
  
namespace binary {

template <typename T>
inline void read_number_nocheck(std::istream& in, T& num) {
  in.read(reinterpret_cast<char *>(&num), sizeof(num));
}

template <typename T>
inline void read_number_nocheck(const char *& data, T& num) {
  num = *reinterpret_cast<const T *>(data);
  data += sizeof(T);
}

template <typename T>
inline T read_number_nocheck(std::istream& in) {
  T num;
  read_number_nocheck(in, num);
  return num;
}

template <typename T>
inline T read_number_nocheck(const char *& data) {
  T num;
  read_number_nocheck(data, num);
  return num;
}

#if DEBUG_LEVEL >= ALPHA
#define read_guard(in, id)				\
  if (read_number_nocheck<unsigned short>(in) != id)	\
      assert(false);
#else
#define read_guard(in, id)
#endif

template <typename T>
inline void read_number(std::istream& in, T& num) {
  read_guard(in, 0x2003);
  in.read(reinterpret_cast<char *>(&num), sizeof(num));
  read_guard(in, 0x2004);
}

template <typename T>
inline void read_number(const char *& data, T& num) {
  read_guard(data, 0x2003);
  num = *reinterpret_cast<const T *>(data);
  data += sizeof(T);
  read_guard(data, 0x2004);
}

template <typename T>
inline T read_number(std::istream& in) {
  T num;
  read_number(in, num);
  return num;
}

template <typename T>
inline T read_number(const char *& data) {
  T num;
  read_number(data, num);
  return num;
}

void read_bool(std::istream& in, bool& num);
void read_bool(const char *& data, bool& num);

inline bool read_bool(std::istream& in) {
  bool num;
  read_bool(in, num);
  return num;
}

inline bool read_bool(const char *& data) {
  bool num;
  read_bool(data, num);
  return num;
}

template <typename T>
void read_long(std::istream& in, T& num)
{
  read_guard(in, 0x2001);

  unsigned char len;
  read_number_nocheck(in, len);

  num = 0;
  unsigned char temp;
  if (len > 3) {
    read_number_nocheck(in, temp);
    num |= static_cast<unsigned long>(temp) << 24;
  }
  if (len > 2) {
    read_number_nocheck(in, temp);
    num |= static_cast<unsigned long>(temp) << 16;
  }
  if (len > 1) {
    read_number_nocheck(in, temp);
    num |= static_cast<unsigned long>(temp) << 8;
  }

  read_number_nocheck(in, temp);
  num |= static_cast<unsigned long>(temp);

  read_guard(in, 0x2002);
}

template <typename T>
void read_long(const char *& data, T& num)
{
  read_guard(data, 0x2001);

  unsigned char len;
  read_number_nocheck(data, len);

  num = 0;
  unsigned char temp;
  if (len > 3) {
    read_number_nocheck(data, temp);
    num |= static_cast<unsigned long>(temp) << 24;
  }
  if (len > 2) {
    read_number_nocheck(data, temp);
    num |= static_cast<unsigned long>(temp) << 16;
  }
  if (len > 1) {
    read_number_nocheck(data, temp);
    num |= static_cast<unsigned long>(temp) << 8;
  }

  read_number_nocheck(data, temp);
  num |= static_cast<unsigned long>(temp);

  read_guard(data, 0x2002);
}

template <typename T>
inline T read_long(std::istream& in) {
  T num;
  read_long(in, num);
  return num;
}

template <typename T>
inline T read_long(const char *& data) {
  T num;
  read_long(data, num);
  return num;
}

void read_string(std::istream& in, string& str);
void read_string(const char *& data, string& str);
void read_string(const char *& data, string * str);

inline string read_string(std::istream& in) {
  string temp;
  read_string(in, temp);
  return temp;
}

inline string read_string(const char *& data) {
  string temp;
  read_string(data, temp);
  return temp;
}


template <typename T>
inline void write_number_nocheck(std::ostream& out, T num) {
  out.write(reinterpret_cast<char *>(&num), sizeof(num));
}

#if DEBUG_LEVEL >= ALPHA
#define write_guard(out, id)			\
  write_number_nocheck<unsigned short>(out, id)
#else
#define write_guard(in, id)
#endif

template <typename T>
inline void write_number(std::ostream& out, T num) {
  write_guard(out, 0x2003);
  out.write(reinterpret_cast<char *>(&num), sizeof(num));
  write_guard(out, 0x2004);
}

void write_bool(std::ostream& out, bool num);

template <typename T>
void write_long(std::ostream& out, T num)
{
  write_guard(out, 0x2001);

  unsigned char len = 4;
  if (static_cast<unsigned long>(num) < 0x00000100UL)
    len = 1;
  else if (static_cast<unsigned long>(num) < 0x00010000UL)
    len = 2;
  else if (static_cast<unsigned long>(num) < 0x01000000UL)
    len = 3;
  write_number_nocheck<unsigned char>(out, len);

  unsigned char temp;
  if (len > 3) {
    temp = (static_cast<unsigned long>(num) & 0xFF000000UL) >> 24;
    write_number_nocheck(out, temp);
  }
  if (len > 2) {
    temp = (static_cast<unsigned long>(num) & 0x00FF0000UL) >> 16;
    write_number_nocheck(out, temp);
  }
  if (len > 1) {
    temp = (static_cast<unsigned long>(num) & 0x0000FF00UL) >> 8;
    write_number_nocheck(out, temp);
  }

  temp = (static_cast<unsigned long>(num) & 0x000000FFUL);
  write_number_nocheck(out, temp);

  write_guard(out, 0x2002);
}

void write_string(std::ostream& out, const string& str);

template <typename T>
inline void write_object(std::ostream& out, const T& journal) {
  assert(false);
}

void write_journal(std::ostream& out, journal_t * journal);

} // namespace binary
} // namespace ledger

#endif // BINARY_H
