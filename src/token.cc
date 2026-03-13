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
 * @file   token.cc
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Implementation of the expression tokenizer.
 *
 * This file implements the lexical analysis (tokenization) phase of Ledger's
 * expression engine.  The central routine is token_t::next(), which reads
 * characters from an input stream and populates a reusable token_t with the
 * next token's kind, symbol text, parsed value, and source length.
 *
 * The tokenizer handles a rich set of syntactic forms:
 *   - Single- and multi-character operators (`+`, `==`, `->`, `&&`)
 *   - Reserved words (`and`, `or`, `not`, `div`, `if`, `else`, `true`, `false`)
 *   - Bracketed date literals (`[2024/01/01]`)
 *   - Quoted strings (`'hello'`, `"$10.00"` with amount-parse fallback)
 *   - Braced amount literals (`{$10.00}`)
 *   - Regular expression masks (`/pattern/`)
 *   - Numeric amounts and commodity values (delegated to amount_t::parse)
 *   - Identifiers (`payee`, `account`, `amount`)
 *
 * The PARSE_OP_CONTEXT flag is important: it tells the tokenizer whether
 * we are in an operator position (where `/` means division) or a terminal
 * position (where `/` begins a regex).  The parser sets this flag via
 * tflags.plus_flags(PARSE_OP_CONTEXT) when it expects an operator.
 */

#include <system.hh>

#include "token.h"
#include "parser.h"

namespace ledger {

/*--- Reserved Word Recognition ---*/

int expr_t::token_t::parse_reserved_word(std::istream& in) {
  int c = in.peek();

  if (c == 'a' || c == 'd' || c == 'e' || c == 'f' || c == 'i' || c == 'o' || c == 'n' ||
      c == 't') {
    length = 0;

    char buf[6];
    READ_INTO_(in, buf, 5, c, length, std::isalpha(static_cast<unsigned char>(c)));

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
    default:
      break;
    }

    return 0;
  }
  return -1;
}

/*--- Identifier Parsing ---*/

void expr_t::token_t::parse_ident(std::istream& in) {
  kind = IDENT;
  length = 0;

  int c;
  char buf[256];
  READ_INTO_(in, buf, 255, c, length, std::isalpha(static_cast<unsigned char>(c)) || c == '_');

  value.set_string(buf);
}

/*--- Main Tokenizer ---*/

void expr_t::token_t::next(std::istream& in, const parse_flags_t& pflags) {
  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  if (!in.good())
    throw_(parse_error, _("Input stream no longer valid"));

  int c = peek_next_nonws(in);

  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  if (!in.good())
    throw_(parse_error, _("Input stream no longer valid"));

  symbol[0] = c;
  symbol[1] = '\0';

  length = 1;

  switch (c) {
  // Logical operators: & (and), | (or)
  case '&':
    in.get();
    c = in.peek();
    if (c == '&') {
      in.get();
      kind = KW_AND;
      length = 2;
      break;
    }
    kind = KW_AND;
    break;
  case '|':
    in.get();
    c = in.peek();
    if (c == '|') {
      in.get();
      kind = KW_OR;
      length = 2;
      break;
    }
    kind = KW_OR;
    break;

  // Grouping delimiters
  case '(':
    in.get();
    kind = LPAREN;
    break;
  case ')':
    in.get();
    kind = RPAREN;
    break;

  // Bracketed date literal: [2024/01/01] parses as a date VALUE token
  case '[': {
    in.get();

    char buf[256];
    READ_INTO_(in, buf, 255, c, length, c != ']');
    if (c != ']')
      expected(']', c);

    in.get();
    length++;

    date_interval_t timespan(buf);
    optional<date_t> begin = timespan.begin();
    if (!begin)
      throw_(parse_error, _("Date specifier does not refer to a starting date"));
    kind = VALUE;
    value = *begin;
    break;
  }

  // Single-quoted string literal: 'hello world'
  case '\'': {
    char delim;
    in.get(delim);
    char buf[4096];
    READ_INTO_(in, buf, 4095, c, length, c != delim);
    if (c != delim)
      expected(delim, c);
    in.get();
    length++;
    kind = VALUE;
    value.set_string(buf);
    break;
  }

  // Double-quoted string: first tries to parse as a commodity amount
  // (e.g., "$10.00"), falling back to a plain string if that fails.
  case '"': {
    std::istream::pos_type pos = in.tellg();

    // Phase 1: attempt amount parsing (handles commodity strings like "$10")
    parse_flags_t parse_flags;
    parse_flags.add_flags(PARSE_NO_ANNOT);
    if (pflags.has_flags(PARSE_NO_MIGRATE))
      parse_flags.add_flags(PARSE_NO_MIGRATE);
    if (pflags.has_flags(PARSE_NO_REDUCE))
      parse_flags.add_flags(PARSE_NO_REDUCE);

    try {
      amount_t temp;
      if (temp.parse(in, parse_flags.plus_flags(PARSE_SOFT_FAIL))) {
        if (!in.good()) {
          in.clear();
          in.seekg(0, std::ios::end);
          if (in.fail())
            throw_(parse_error, _("Failed to reset input stream"));
        }
        kind = VALUE;
        value = temp;
        length = static_cast<std::size_t>(in.tellg() - pos);
        break;
      }
    } catch (const std::exception&) {} // NOLINT(bugprone-empty-catch)

    // Phase 2: amount parse failed; rewind and read as a plain string
    in.clear();
    in.seekg(pos, std::ios::beg);
    if (in.fail())
      throw_(parse_error, _("Failed to reset input stream"));

    char delim;
    in.get(delim);
    char buf[4096];
    READ_INTO_(in, buf, 4095, c, length, c != delim);
    if (c != delim)
      expected(delim, c);
    in.get();
    length++;
    kind = VALUE;
    value.set_string(buf);
    break;
  }

  // Braced amount literal: {$10.00} -- always parsed as an amount
  // without commodity migration (preserves the exact commodity form)
  case '{': {
    in.get();
    amount_t temp;
    (void)temp.parse(in, PARSE_NO_MIGRATE);
    c = in.get();
    if (c != '}')
      expected('}', c);
    length++;
    kind = VALUE;
    value = temp;
    break;
  }

  // Operators beginning with '!': negation, inequality, regex non-match
  case '!':
    in.get();
    c = in.peek();
    if (c == '=') {
      in.get();
      symbol[1] = c;
      symbol[2] = '\0';
      kind = NEQUAL;
      length = 2;
      break;
    } else if (c == '~') {
      in.get();
      symbol[1] = c;
      symbol[2] = '\0';
      kind = NMATCH;
      length = 2;
      break;
    }
    kind = EXCLAM;
    break;

  // Arithmetic operators: -, +, *
  case '-':
    in.get();
    c = in.peek();
    if (c == '>') {
      in.get();
      symbol[1] = c;
      symbol[2] = '\0';
      kind = ARROW;
      length = 2;
      break;
    }
    kind = MINUS;
    break;
  case '+':
    in.get();
    kind = PLUS;
    break;

  case '*':
    in.get();
    kind = STAR;
    break;

  // Ternary operator tokens
  case '?':
    in.get();
    kind = QUERY;
    break;
  case ':':
    in.get();
    kind = COLON;
    break;

  // Slash: division in operator context, regex delimiter in terminal context.
  // The PARSE_OP_CONTEXT flag (set by the parser after reading an operand)
  // disambiguates these two meanings.
  case '/': {
    in.get();
    if (pflags.has_flags(PARSE_OP_CONTEXT)) { // operator context
      kind = SLASH;
    } else { // terminal context
      // Read the regexp pattern, preserving backslash sequences for the regex
      // engine. Only \/ is treated as an escaped delimiter (the backslash is
      // stripped so that / does not terminate the pattern early); all other
      // \X sequences are passed through unchanged so that the regex engine can
      // interpret them (e.g., \| for literal pipe, \d for digit class).
      string pat;
      bool found_closing = false;
      while (in.good() && !in.eof()) {
        c = in.get();
        if (in.eof())
          break;
        length++;
        if (c == '\\') {
          int next = in.peek();
          if (next == '/') {
            // Escaped delimiter: consume the slash and emit just '/'
            in.get();
            length++;
            pat.push_back('/');
          } else {
            // All other escape sequences: pass the backslash through so the
            // regex engine sees the full \X pair (e.g., \| → literal pipe)
            pat.push_back('\\');
          }
        } else if (c == '/') {
          found_closing = true;
          break;
        } else {
          pat.push_back(static_cast<char>(c));
        }
      }
      if (!found_closing)
        expected('/', c);

      kind = VALUE;
      value.set_mask(pat);
    }
    break;
  }

  // Operators beginning with '=': regex match, equality, assignment
  case '=':
    in.get();
    c = in.peek();
    if (c == '~') {
      in.get();
      symbol[1] = c;
      symbol[2] = '\0';
      kind = MATCH;
      length = 2;
      break;
    } else if (c == '=') {
      in.get();
      symbol[1] = c;
      symbol[2] = '\0';
      kind = EQUAL;
      length = 2;
      break;
    }
    kind = ASSIGN;
    break;

  // Comparison operators: <, <=, >, >=
  case '<':
    in.get();
    if (in.peek() == '=') {
      symbol[1] = in.get();
      symbol[2] = '\0';
      kind = LESSEQ;
      length = 2;
      break;
    }
    kind = LESS;
    break;

  case '>':
    in.get();
    if (in.peek() == '=') {
      symbol[1] = in.get();
      symbol[2] = '\0';
      kind = GREATEREQ;
      length = 2;
      break;
    }
    kind = GREATER;
    break;

  // Punctuation tokens
  case '.':
    in.get();
    kind = DOT;
    break;

  case ',':
    in.get();
    kind = COMMA;
    break;

  case ';':
    in.get();
    kind = SEMI;
    break;

  // Default case: handles numeric amounts, reserved words, and identifiers.
  // The strategy is: first try reserved words (and/or/not/div/if/else/true/false),
  // then try amount parsing (e.g., "$10.00", "100"), and finally fall back
  // to identifier parsing.
  default: {
    std::istream::pos_type pos = in.tellg();

    // First, check to see if it's a reserved word, such as: and or not
    int result = parse_reserved_word(in);
    if (std::isalpha(static_cast<unsigned char>(c)) && result == 1)
      break;

    // If not, rewind back to the beginning of the word to scan it
    // again.  If the result was -1, it means no identifier was scanned
    // so we don't have to rewind.
    if (result == 0 || !in.good()) {
      in.clear();
      in.seekg(pos, std::ios::beg);
      if (in.fail())
        throw_(parse_error, _("Failed to reset input stream"));
    }

    assert(in.good());
    assert(!in.eof());
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
      if (!temp.parse(in, parse_flags.plus_flags(PARSE_SOFT_FAIL))) {
        in.clear();
        in.seekg(pos, std::ios::beg);
        if (in.fail() || !in.good())
          throw_(parse_error, _("Failed to reset input stream"));

        c = in.peek();
        if (c != -1) {
          if (!std::isalpha(static_cast<unsigned char>(c)) && c != '_')
            expected('\0', c);

          parse_ident(in);
        } else {
          throw_(parse_error, _("Unexpected EOF"));
        }

        if (!value.is_string() || value.as_string().empty()) {
          kind = ERROR;
          symbol[0] = c;
          symbol[1] = '\0';
          throw_(parse_error, _("Failed to parse identifier"));
        }
      } else {
        if (!in.good()) {
          in.clear();
          in.seekg(0, std::ios::end);
          if (in.fail())
            throw_(parse_error, _("Failed to reset input stream"));
        }

        kind = VALUE;
        value = temp;
        length = static_cast<std::size_t>(in.tellg() - pos);
      }
    } catch (const std::exception&) {
      kind = ERROR;
      length = static_cast<std::size_t>(in.tellg() - pos);
      throw;
    }
    break;
  }
  }
}

/*--- Stream Rewinding ---*/

void expr_t::token_t::rewind(std::istream& in) {
  in.clear();
  in.seekg(-static_cast<std::streamoff>(length), std::ios::cur);
  if (in.fail())
    throw_(parse_error, _("Failed to rewind input stream"));
}

/*--- Error Reporting ---*/

void expr_t::token_t::unexpected(const char wanted) {
  kind_t prev_kind = kind;

  kind = ERROR;

  // NOLINTBEGIN(bugprone-branch-clone)
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
      throw_(parse_error, _f("Unexpected end of expression (wanted '%1%')") % wanted);
    case IDENT:
      throw_(parse_error, _f("Unexpected symbol '%1%' (wanted '%2%')") % value % wanted);
    case VALUE:
      throw_(parse_error, _f("Unexpected value '%1%' (wanted '%2%')") % value % wanted);
    default:
      throw_(parse_error, _f("Unexpected expression token '%1%' (wanted '%2%')") % symbol % wanted);
    }
  }
  // NOLINTEND(bugprone-branch-clone)
}

void expr_t::token_t::expected(const char wanted, const int c) {
  if (c == -1) {
    if (wanted == '\0')
      throw_(parse_error, _("Unexpected end"));
    else
      throw_(parse_error, _f("Missing '%1%'") % wanted);
  } else {
    char ch = c;
    if (wanted == '\0')
      throw_(parse_error, _f("Invalid char '%1%'") % ch);
    else
      throw_(parse_error, _f("Invalid char '%1%' (wanted '%2%')") % ch % wanted);
  }
}

void expr_t::token_t::expected(const kind_t wanted) {
  try {
    if (wanted == expr_t::token_t::ERROR || wanted == expr_t::token_t::UNKNOWN)
      throw_(parse_error, _f("Invalid token '%1%'") % *this);
    else
      throw_(parse_error, _f("Invalid token '%1%' (wanted '%2%')") % *this % wanted);
  } catch (...) {
    kind = ERROR;
    throw;
  }
}

/*--- Stream Output Operators ---*/

std::ostream& operator<<(std::ostream& out, const expr_t::token_t::kind_t& kind) {
  switch (kind) {
  case expr_t::token_t::ERROR:
    out << "<error token>";
    break;
  case expr_t::token_t::VALUE:
    out << "<value>";
    break;
  case expr_t::token_t::IDENT:
    out << "<identifier>";
    break;
  case expr_t::token_t::MASK:
    out << "<regex mask>";
    break;

  case expr_t::token_t::LPAREN:
    out << "(";
    break;
  case expr_t::token_t::RPAREN:
    out << ")";
    break;
  case expr_t::token_t::LBRACE:
    out << "{";
    break;
  case expr_t::token_t::RBRACE:
    out << "}";
    break;

  case expr_t::token_t::EQUAL:
    out << "==";
    break;
  case expr_t::token_t::NEQUAL:
    out << "!=";
    break;
  case expr_t::token_t::LESS:
    out << "<";
    break;
  case expr_t::token_t::LESSEQ:
    out << "<=";
    break;
  case expr_t::token_t::GREATER:
    out << ">";
    break;
  case expr_t::token_t::GREATEREQ:
    out << ">=";
    break;

  case expr_t::token_t::ASSIGN:
    out << "=";
    break;
  case expr_t::token_t::MATCH:
    out << "=~";
    break;
  case expr_t::token_t::NMATCH:
    out << "!~";
    break;
  case expr_t::token_t::MINUS:
    out << "-";
    break;
  case expr_t::token_t::PLUS:
    out << "+";
    break;
  case expr_t::token_t::STAR:
    out << "*";
    break;
  case expr_t::token_t::SLASH:
    out << "/";
    break;
  case expr_t::token_t::ARROW:
    out << "->";
    break;
  case expr_t::token_t::KW_DIV:
    out << "div";
    break;

  case expr_t::token_t::EXCLAM:
    out << "!";
    break;
  case expr_t::token_t::KW_AND:
    out << "and";
    break;
  case expr_t::token_t::KW_OR:
    out << "or";
    break;
  case expr_t::token_t::KW_MOD:
    out << "mod";
    break;

  case expr_t::token_t::KW_IF:
    out << "if";
    break;
  case expr_t::token_t::KW_ELSE:
    out << "else";
    break;

  case expr_t::token_t::QUERY:
    out << "?";
    break;
  case expr_t::token_t::COLON:
    out << ":";
    break;

  case expr_t::token_t::DOT:
    out << ".";
    break;
  case expr_t::token_t::COMMA:
    out << ",";
    break;
  case expr_t::token_t::SEMI:
    out << ";";
    break;

  case expr_t::token_t::TOK_EOF:
    out << "<end of input>";
    break;
  case expr_t::token_t::UNKNOWN:
    out << "<unknown>";
    break;
  }

  return out;
}

std::ostream& operator<<(std::ostream& out, const expr_t::token_t& token) {
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
