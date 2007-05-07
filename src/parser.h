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

#ifndef _PARSER_H
#define _PARSER_H

#include "utils.h"

namespace ledger {

class account_t;
class journal_t;

class parser_t
{
 public:
  virtual ~parser_t() {}

  virtual bool test(std::istream& in) const = 0;

  virtual unsigned int parse(std::istream&	   in,
			     journal_t *	   journal,
			     account_t *	   master   = NULL,
			     const optional<path>& original = optional<path>()) = 0;
};

DECLARE_EXCEPTION(parse_error);

/************************************************************************
 *
 * General utility parsing functions
 */

inline char * skip_ws(char * ptr) {
  while (*ptr == ' ' || *ptr == '\t' || *ptr == '\n')
    ptr++;
  return ptr;
}

inline char peek_next_nonws(std::istream& in) {
  char c = in.peek();
  while (! in.eof() && std::isspace(c)) {
    in.get(c);
    c = in.peek();
  }
  return c;
}

#define READ_INTO(str, targ, size, var, cond) {				\
  char * _p = targ;							\
  var = str.peek();							\
  while (! str.eof() && var != '\n' && (cond) && _p - targ < size) {	\
    str.get(var);							\
    if (str.eof())							\
      break;								\
    if (var == '\\') {							\
      str.get(var);							\
      if (in.eof())							\
	break;								\
    }									\
    *_p++ = var;							\
    var = str.peek();							\
  }									\
  *_p = '\0';								\
}

#define READ_INTO_(str, targ, size, var, idx, cond) {			\
  char * _p = targ;							\
  var = str.peek();							\
  while (! str.eof() && var != '\n' && (cond) && _p - targ < size) {	\
    str.get(var);							\
    if (str.eof())							\
      break;								\
    idx++;								\
    if (var == '\\') {							\
      str.get(var);							\
      if (in.eof())							\
	break;								\
      idx++;								\
    }									\
    *_p++ = var;							\
    var = str.peek();							\
  }									\
  *_p = '\0';								\
}

} // namespace ledger

#endif // _PARSER_H
