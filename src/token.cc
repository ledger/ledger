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

#include "token.h"
#include "parser.h"

namespace ledger {

int expr_t::token_t::parse_reserved_word(std::istream& in)
{
  char c = in.peek();

  if (c == 'a' || c == 'd' || c == 'f' || c == 'o' || c == 'n' || c == 't') {
    length = 0;

    char buf[6];
    READ_INTO_(in, buf, 5, c, length, std::isalpha(c));

    switch (buf[0]) {
    case 'a':
      if (std::strcmp(buf, "and") == 0) {
	symbol[0] = '&';
	symbol[1] = '\0';
	kind = KW_AND;
	return 1;
      }
      break;

    case 'd':
      if (std::strcmp(buf, "div") == 0) {
	symbol[0] = '/';
	symbol[1] = '/';
	symbol[2] = '\0';
	kind = KW_DIV;
	return 1;
      }
      break;

    case 'f':
      if (std::strcmp(buf, "false") == 0) {
	kind = VALUE;
	value = false;
	return 1;
      }
      break;

    case 'o':
      if (std::strcmp(buf, "or") == 0) {
	symbol[0] = '|';
	symbol[1] = '\0';
	kind = KW_OR;
	return 1;
      }
      break;

    case 'n':
      if (std::strcmp(buf, "not") == 0) {
	symbol[0] = '!';
	symbol[1] = '\0';
	kind = EXCLAM;
	return 1;
      }
      break;

    case 't':
      if (std::strcmp(buf, "true") == 0) {
	kind = VALUE;
	value = true;
	return 1;
      }
      break;
    }

    return 0;
  }
  return -1;
}

void expr_t::token_t::parse_ident(std::istream& in)
{
  kind	 = IDENT;
  length = 0;

  char c, buf[256];
  READ_INTO_(in, buf, 255, c, length, std::isalnum(c) || c == '_');

  value.set_string(buf);
}

void expr_t::token_t::next(std::istream& in, const uint_least8_t pflags)
{
  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  if (! in.good())
    throw_(parse_error, _("Input stream no longer valid"));

  char c = peek_next_nonws(in);

  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  if (! in.good())
    throw_(parse_error, _("Input stream no longer valid"));

  symbol[0] = c;
  symbol[1] = '\0';

  length = 1;

  switch (c) {
  case '&':
    in.get(c);
    c = in.peek();
    if (c == '&') {
      in.get(c);
      kind = KW_AND;
      length = 2;
      break;
    }
    kind = KW_AND;
    break;
  case '|':
    in.get(c);
    c = in.peek();
    if (c == '|') {
      in.get(c);
      kind = KW_OR;
      length = 2;
      break;
    }
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
    temp.parse(in, amount_t::PARSE_NO_MIGRATE);
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
    else if (c == '~') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = NMATCH;
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
    c = in.peek();
    if (c == '=') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = DEFINE;
      length = 2;
      break;
    }
    kind = COLON;
    break;

  case '/': {
    in.get(c);
    if (pflags & PARSE_OP_CONTEXT) { // operator context
      kind = SLASH;
    } else {			// terminal context
      // Read in the regexp
      char buf[256];
      READ_INTO_(in, buf, 255, c, length, c != '/');
      if (c != '/')
	expected('/', c);
      in.get(c);
      length++;

      kind = VALUE;
      value.set_mask(buf);
    }
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
    else if (c == '=') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = EQUAL;
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

  case '.':
    in.get(c);
    kind = DOT;
    break;

  case ',':
    in.get(c);
    kind = COMMA;
    break;

  default: {
    istream_pos_type pos = in.tellg();

    // First, check to see if it's a reserved word, such as: and or not
    int result = parse_reserved_word(in);
    if (std::isalpha(c) && result == 1)
      break;

    // If not, rewind back to the beginning of the word to scan it
    // again.  If the result was -1, it means no identifier was scanned
    // so we don't have to rewind.
    if (result == 0) {
      in.clear();
      in.seekg(pos, std::ios::beg);
      if (in.fail())
	throw_(parse_error, _("Failed to reset input stream"));
    }

    // When in relaxed parsing mode, we want to migrate commodity flags
    // so that any precision specified by the user updates the current
    // maximum displayed precision.
    amount_t::parse_flags_t parse_flags;
    parser_t::parse_flags_t pflags_copy(pflags);

    if (pflags_copy.has_flags(PARSE_NO_MIGRATE))
      parse_flags.add_flags(amount_t::PARSE_NO_MIGRATE);
    if (pflags_copy.has_flags(PARSE_NO_REDUCE))
      parse_flags.add_flags(amount_t::PARSE_NO_REDUCE);

    try {
      amount_t temp;
      if (! temp.parse(in, parse_flags.plus_flags(amount_t::PARSE_SOFT_FAIL))) {
	// If the amount had no commodity, it must be an unambiguous
	// variable reference

	in.clear();
	in.seekg(pos, std::ios::beg);
	if (in.fail())
	  throw_(parse_error, _("Failed to reset input stream"));

	c = in.peek();
	if (std::isdigit(c) || c == '.')
	  expected('\0', c);

	parse_ident(in);
      } else {
	kind   = VALUE;
	value  = temp;
	length = in.tellg() - pos;
      }
    }
    catch (const std::exception& err) {
      kind   = ERROR;
      length = in.tellg() - pos;
      throw;
    }
    break;
  }
  }
}

void expr_t::token_t::rewind(std::istream& in)
{
  in.seekg(- length, std::ios::cur);
  if (in.fail())
    throw_(parse_error, _("Failed to rewind input stream"));
}


void expr_t::token_t::unexpected()
{
  kind_t prev_kind = kind;

  kind = ERROR;

  switch (prev_kind) {
  case TOK_EOF:
    throw_(parse_error, _("Unexpected end of expression"));
  case IDENT:
    throw_(parse_error, _("Unexpected symbol '%1'") << value);
  case VALUE:
    throw_(parse_error, _("Unexpected value '%1'") << value);
  default:
    throw_(parse_error, _("Unexpected operator '%1'") << symbol);
  }
}

void expr_t::token_t::expected(char wanted, char c)
{
  kind = ERROR;

  if (c == '\0' || c == -1) {
    if (wanted == '\0' || wanted == -1)
      throw_(parse_error, _("Unexpected end"));
    else
      throw_(parse_error, _("Missing '%1'") << wanted);
  } else {
    if (wanted == '\0' || wanted == -1)
      throw_(parse_error, _("Invalid char '%1'") << c);
    else
      throw_(parse_error, _("Invalid char '%1' (wanted '%2')") << c << wanted);
  }
}

} // namespace ledger
