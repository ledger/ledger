/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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

#include "token.h"
#include "parser.h"

namespace ledger {

void expr_t::token_t::parse_ident(std::istream& in)
{
  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  assert(in.good());

  char c = peek_next_nonws(in);

  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  assert(in.good());

  kind	 = IDENT;
  length = 0;

  char buf[256];
  READ_INTO_(in, buf, 255, c, length,
	     std::isalnum(c) || c == '_' || c == '.' || c == '-');

  switch (buf[0]) {
  case 'f':
    if (std::strcmp(buf, "false") == 0) {
      kind = VALUE;
      value = false;
    }
    break;
  case 't':
    if (std::strcmp(buf, "true") == 0) {
      kind = VALUE;
      value = true;
    }
    break;
  }

  if (kind == IDENT)
    value.set_string(buf);
}

void expr_t::token_t::next(std::istream& in, const uint_least8_t pflags)
{
  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  assert(in.good());

  char c = peek_next_nonws(in);

  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  assert(in.good());

  symbol[0] = c;
  symbol[1] = '\0';

  length = 1;

  switch (c) {
  case '&':
    in.get(c);
    kind = KW_AND;
    break;
  case '|':
    in.get(c);
    kind = KW_OR;
    break;

  case '(':
    in.get(c);
    kind = LPAREN;
    break;
  case ')':
    in.get(c);
    kind = RPAREN;
    break;

  case '[': {
    in.get(c);

    char buf[256];
    READ_INTO_(in, buf, 255, c, length, c != ']');
    if (c != ']')
      expected(']', c);

    in.get(c);
    length++;

    interval_t timespan(buf);
    kind  = VALUE;
    value = timespan.first();
    break;
  }

  case '\'':
  case '"': {
    char delim;
    in.get(delim);
    char buf[4096];
    READ_INTO_(in, buf, 4095, c, length, c != delim);
    if (c != delim)
      expected(delim, c);
    in.get(c);
    length++;
    kind = VALUE;
    value.set_string(buf);
    break;
  }

  case '{': {
    in.get(c);
    amount_t temp;
    temp.parse(in, AMOUNT_PARSE_NO_MIGRATE);
    in.get(c);
    if (c != '}')
      expected('}', c);
    length++;
    kind  = VALUE;
    value = temp;
    break;
  }

  case '!':
    in.get(c);
    c = in.peek();
    if (c == '=') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = NEQUAL;
      length = 2;
      break;
    }
    kind = EXCLAM;
    break;

  case '-':
    in.get(c);
    kind = MINUS;
    break;
  case '+':
    in.get(c);
    kind = PLUS;
    break;

  case '*':
    in.get(c);
    kind = STAR;
    break;

  case '?':
    in.get(c);
    kind = QUERY;
    break;

  case ':':
    in.get(c);
    kind = COLON;
    break;

  case '/': {
    in.get(c);

    // Read in the regexp
    char buf[256];
    READ_INTO_(in, buf, 255, c, length, c != '/');
    if (c != '/')
      expected('/', c);
    in.get(c);
    length++;

    kind = MASK;
    value.set_string(buf);
    break;
  }

  case '=':
    in.get(c);
    c = in.peek();
    if (c == '~') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = MATCH;
      length = 2;
      break;
    }
    kind = EQUAL;
    break;

  case '<':
    in.get(c);
    if (in.peek() == '=') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = LESSEQ;
      length = 2;
      break;
    }
    kind = LESS;
    break;

  case '>':
    in.get(c);
    if (in.peek() == '=') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = GREATEREQ;
      length = 2;
      break;
    }
    kind = GREATER;
    break;

  case ',':
    in.get(c);
    kind = COMMA;
    break;

  default: {
    amount_t temp;
    istream_pos_type pos = 0;

    // When in relaxed parsing mode, we want to migrate commodity
    // flags so that any precision specified by the user updates the
    // current maximum displayed precision.
    pos = in.tellg();

    amount_t::flags_t parse_flags = 0;
    if (pflags & EXPR_PARSE_NO_MIGRATE)
      parse_flags |= AMOUNT_PARSE_NO_MIGRATE;
    if (pflags & EXPR_PARSE_NO_REDUCE)
      parse_flags |= AMOUNT_PARSE_NO_REDUCE;

    if (! temp.parse(in, parse_flags | AMOUNT_PARSE_SOFT_FAIL)) {
      // If the amount had no commodity, it must be an unambiguous
      // variable reference

      in.clear();
      in.seekg(pos, std::ios::beg);

      c = in.peek();
      assert(! (std::isdigit(c) || c == '.'));
      parse_ident(in);
    } else {
      kind = VALUE;
      value = temp;
    }
    break;
  }
  }
}

void expr_t::token_t::rewind(std::istream& in)
{
  for (unsigned int i = 0; i < length; i++)
    in.unget();
}


void expr_t::token_t::unexpected()
{
  switch (kind) {
  case TOK_EOF:
    throw_(parse_error, "Unexpected end of expression");
  case IDENT:
    throw_(parse_error, "Unexpected symbol '" << value << "'");
  case VALUE:
    throw_(parse_error, "Unexpected value '" << value << "'");
  default:
    throw_(parse_error, "Unexpected operator '" << symbol << "'");
  }
}

void expr_t::token_t::expected(char wanted, char c)
{
  if (c == '\0') {
    if (wanted)
      throw_(parse_error, "Missing '" << wanted << "'");
    else
      throw_(parse_error, "Unexpected end");
  } else {
    if (wanted)
      throw_(parse_error, "Invalid char '" << c
	     << "' (wanted '" << wanted << "')");
    else
      throw_(parse_error, "Invalid char '" << c << "'");
  }
}

} // namespace ledger
