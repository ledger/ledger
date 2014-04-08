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

#include "token.h"
#include "parser.h"

namespace ledger {

int expr_t::token_t::parse_reserved_word(std::istream& in)
{
  char c = static_cast<char>(in.peek());

  if (c == 'a' || c == 'd' || c == 'e' || c == 'f' ||
      c == 'i' || c == 'o' || c == 'n' || c == 't') {
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
        symbol[1] = '\0';
        kind = KW_DIV;
        return 1;
      }
      break;

    case 'e':
      if (std::strcmp(buf, "else") == 0) {
        std::strcpy(symbol, "else");
        kind = KW_ELSE;
        return 1;
      }
      break;

    case 'f':
      if (std::strcmp(buf, "false") == 0) {
        std::strcpy(symbol, "false");
        kind = VALUE;
        value = false;
        return 1;
      }
      break;

    case 'i':
      if (std::strcmp(buf, "if") == 0) {
        symbol[0] = 'i';
        symbol[1] = 'f';
        symbol[2] = '\0';
        kind = KW_IF;
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
        std::strcpy(symbol, "true");
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
  kind   = IDENT;
  length = 0;

  char c, buf[256];
  READ_INTO_(in, buf, 255, c, length, std::isalnum(c) || c == '_');

  value.set_string(buf);
}

void expr_t::token_t::next(std::istream& in, const parse_flags_t& pflags)
{
  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  if (! in.good())
    throw_(parse_error, _("Input stream no longer valid"));

  char c = peek_next_nonws(in);

  if (in.eof() || c == -1) {
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
    c = static_cast<char>(in.peek());
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
    c = static_cast<char>(in.peek());
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

    date_interval_t timespan(buf);
    optional<date_t> begin = timespan.begin();
    if (! begin)
      throw_(parse_error,
             _("Date specifier does not refer to a starting date"));
    kind  = VALUE;
    value = *begin;
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
    temp.parse(in, PARSE_NO_MIGRATE);
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
    c = static_cast<char>(in.peek());
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
    c = static_cast<char>(in.peek());
    if (c == '>') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = ARROW;
      length = 2;
      break;
    }
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
    c = static_cast<char>(in.peek());
    kind = COLON;
    break;

  case '/': {
    in.get(c);
    if (pflags.has_flags(PARSE_OP_CONTEXT)) { // operator context
      kind = SLASH;
    } else {                    // terminal context
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
    c = static_cast<char>(in.peek());
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
    kind = ASSIGN;
    break;

  case '<':
    in.get(c);
    if (static_cast<char>(in.peek()) == '=') {
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
    if (static_cast<char>(in.peek()) == '=') {
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

  case ';':
    in.get(c);
    kind = SEMI;
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
    if (result == 0 || ! in.good()) {
      in.clear();
      in.seekg(pos, std::ios::beg);
      if (in.fail())
        throw_(parse_error, _("Failed to reset input stream"));
    }

    assert(in.good());
    assert(! in.eof());
    assert(static_cast<int>(in.tellg()) != -1);

    // When in relaxed parsing mode, we want to migrate commodity flags
    // so that any precision specified by the user updates the current
    // maximum displayed precision.
    parse_flags_t parse_flags;

    parse_flags.add_flags(PARSE_NO_ANNOT);
    if (pflags.has_flags(PARSE_NO_MIGRATE))
      parse_flags.add_flags(PARSE_NO_MIGRATE);
    if (pflags.has_flags(PARSE_NO_REDUCE))
      parse_flags.add_flags(PARSE_NO_REDUCE);

    try {
      amount_t temp;
      if (! temp.parse(in, parse_flags.plus_flags(PARSE_SOFT_FAIL))) {
        in.clear();
        in.seekg(pos, std::ios::beg);
        if (in.fail() || ! in.good())
          throw_(parse_error, _("Failed to reset input stream"));

        c = static_cast<char>(in.peek());
        if (c != -1) {
          if (! std::isalpha(c) && c != '_')
            expected('\0', c);

          parse_ident(in);
        } else {
          throw_(parse_error, _("Unexpected EOF"));
        }

        if (! value.is_string() || value.as_string().empty()) {
          kind = ERROR;
          symbol[0] = c;
          symbol[1] = '\0';
          throw_(parse_error, _("Failed to parse identifier"));
        }
      } else {
        if (! in.good()) {
          in.clear();
          in.seekg(0, std::ios::end);
          if (in.fail())
            throw_(parse_error, _("Failed to reset input stream"));
        }

        kind   = VALUE;
        value  = temp;
        length = static_cast<std::size_t>(in.tellg() - pos);
      }
    }
    catch (const std::exception&) {
      kind   = ERROR;
      length = static_cast<std::size_t>(in.tellg() - pos);
      throw;
    }
    break;
  }
  }
}

void expr_t::token_t::rewind(std::istream& in)
{
  in.clear();
  in.seekg(- int(length), std::ios::cur);
  if (in.fail())
    throw_(parse_error, _("Failed to rewind input stream"));
}


void expr_t::token_t::unexpected(const char wanted)
{
  kind_t prev_kind = kind;

  kind = ERROR;

  if (wanted == '\0') {
    switch (prev_kind) {
    case TOK_EOF:
      throw_(parse_error, _("Unexpected end of expression"));
    case IDENT:
      throw_(parse_error, _f("Unexpected symbol '%1%'") % value);
    case VALUE:
      throw_(parse_error, _f("Unexpected value '%1%'") % value);
    default:
      throw_(parse_error, _f("Unexpected expression token '%1%'") % symbol);
    }
  } else {
    switch (prev_kind) {
    case TOK_EOF:
      throw_(parse_error,
             _f("Unexpected end of expression (wanted '%1%')") % wanted);
    case IDENT:
      throw_(parse_error,
             _f("Unexpected symbol '%1%' (wanted '%2%')") % value % wanted);
    case VALUE:
      throw_(parse_error,
             _f("Unexpected value '%1%' (wanted '%2%')") % value % wanted);
    default:
      throw_(parse_error, _f("Unexpected expression token '%1%' (wanted '%2%')")
             % symbol % wanted);
    }
  }
}

void expr_t::token_t::expected(const char wanted, char c)
{
  if (c == '\0' || c == -1) {
    if (wanted == '\0' || wanted == -1)
      throw_(parse_error, _("Unexpected end"));
    else
      throw_(parse_error, _f("Missing '%1%'") % wanted);
  } else {
    if (wanted == '\0' || wanted == -1)
      throw_(parse_error, _f("Invalid char '%1%'") % c);
    else
      throw_(parse_error,
             _f("Invalid char '%1%' (wanted '%2%')") % c % wanted);
  }
}

void expr_t::token_t::expected(const kind_t wanted)
{
  try {
    if (wanted == expr_t::token_t::ERROR ||
        wanted == expr_t::token_t::UNKNOWN)
      throw_(parse_error, _f("Invalid token '%1%'") % *this);
    else
      throw_(parse_error,
             _f("Invalid token '%1%' (wanted '%2%')") % *this % wanted);
  }
  catch (...) {
    kind = ERROR;
    throw;
  }
}

std::ostream& operator<<(std::ostream& out, const expr_t::token_t::kind_t& kind)
{
  switch (kind) {
  case expr_t::token_t::ERROR:     out << "<error token>"; break;
  case expr_t::token_t::VALUE:     out << "<value>"; break;
  case expr_t::token_t::IDENT:     out << "<identifier>"; break;
  case expr_t::token_t::MASK:      out << "<regex mask>"; break;

  case expr_t::token_t::LPAREN:    out << "("; break;
  case expr_t::token_t::RPAREN:    out << ")"; break;
  case expr_t::token_t::LBRACE:    out << "{"; break;
  case expr_t::token_t::RBRACE:    out << "}"; break;

  case expr_t::token_t::EQUAL:     out << "=="; break;
  case expr_t::token_t::NEQUAL:    out << "!="; break;
  case expr_t::token_t::LESS:      out << "<"; break;
  case expr_t::token_t::LESSEQ:    out << "<="; break;
  case expr_t::token_t::GREATER:   out << ">"; break;
  case expr_t::token_t::GREATEREQ: out << ">="; break;

  case expr_t::token_t::ASSIGN:    out << "="; break;
  case expr_t::token_t::MATCH:     out << "=~"; break;
  case expr_t::token_t::NMATCH:    out << "!~"; break;
  case expr_t::token_t::MINUS:     out << "-"; break;
  case expr_t::token_t::PLUS:      out << "+"; break;
  case expr_t::token_t::STAR:      out << "*"; break;
  case expr_t::token_t::SLASH:     out << "/"; break;
  case expr_t::token_t::ARROW:     out << "->"; break;
  case expr_t::token_t::KW_DIV:    out << "div"; break;

  case expr_t::token_t::EXCLAM:    out << "!"; break;
  case expr_t::token_t::KW_AND:    out << "and"; break;
  case expr_t::token_t::KW_OR:     out << "or"; break;
  case expr_t::token_t::KW_MOD:    out << "mod"; break;

  case expr_t::token_t::KW_IF:     out << "if"; break;
  case expr_t::token_t::KW_ELSE:   out << "else"; break;

  case expr_t::token_t::QUERY:     out << "?"; break;
  case expr_t::token_t::COLON:     out << ":"; break;

  case expr_t::token_t::DOT:       out << "."; break;
  case expr_t::token_t::COMMA:     out << ","; break;
  case expr_t::token_t::SEMI:      out << ";"; break;

  case expr_t::token_t::TOK_EOF:   out << "<end of input>"; break;
  case expr_t::token_t::UNKNOWN:   out << "<unknown>"; break;
  }

  return out;
}

std::ostream& operator<<(std::ostream& out, const expr_t::token_t& token)
{
  switch (token.kind) {
  case expr_t::token_t::VALUE:
    out << "<value '" << token.value << "'>";
    break;
  case expr_t::token_t::IDENT:
    out << "<ident '" << token.value << "'>";
    break;
  case expr_t::token_t::MASK:
    out << "<mask '" << token.value << "'>";
    break;

  default:
    out << token.kind;
    break;
  }

  return out;
}

} // namespace ledger
