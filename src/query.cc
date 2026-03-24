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
 * @file   query.cc
 * @author John Wiegley
 * @brief  Implementation of the query lexer and recursive-descent parser.
 *
 * @ingroup expr
 *
 * This file contains the core logic that translates command-line arguments
 * into expression trees.  The pipeline is:
 *
 * 1. **Lexing** (next_token): Scans characters from the argument sequence,
 *    recognizing operators (`@`, `#`, `%`, `=`, `&`, `|`, `!`), keywords
 *    (`and`, `or`, `not`, `payee`, `code`, `expr`, `show`, `for`, etc.),
 *    quoted patterns (`/food/`, `'grocery'`), and bare terms.
 *
 * 2. **Parsing** (parse_query_term through parse_query_expr): A recursive-
 *    descent parser with standard boolean precedence builds expression
 *    trees (expr_t::op_t nodes).  Terms are converted into O_MATCH nodes
 *    that regex-match against the appropriate field (account, payee, code,
 *    note), while the `expr` keyword passes raw expressions through.
 *
 * 3. **Section splitting** (parse_query_expr): After the main predicate,
 *    keywords like `show`, `only`, `bold`, and `for`/`since`/`until`
 *    start new sections, each producing a separate predicate stored in
 *    the query_map.
 *
 * ## EBNF Grammar
 *
 * The query sub-language parsed by the recursive-descent parser below:
 *
 * @verbatim
 *   query       = or_expr { section or_expr } ;
 *   section     = "show" | "only" | "bold" | "for" | "since" | "until" ;
 *
 *   or_expr     = and_expr { ( "or" | "|" ) and_expr } ;
 *   and_expr    = unary_expr { ( "and" | "&" ) unary_expr }
 *               | unary_expr unary_expr           (* implicit AND between adjacent terms *)
 *               ;
 *   unary_expr  = ( "not" | "!" ) term
 *               | term
 *               ;
 *
 *   term        = field_prefix term                (* changes matching context *)
 *               | "(" query ")"
 *               | TERM                             (* atom: pattern, comparison, or metadata *)
 *               ;
 *   field_prefix = "payee" | "desc" | "@"          (* sets context to payee *)
 *               | "code"  | "#"                    (* sets context to code *)
 *               | "note"  | "="                    (* sets context to note *)
 *               | "meta"  | "tag" | "data" | "%"  (* sets context to metadata *)
 *               | "expr"                           (* sets context to raw expression *)
 *               ;
 *
 *   (* TERM interpretation depends on the current context: *)
 *   (*   TOK_ACCOUNT / TOK_PAYEE / TOK_CODE / TOK_NOTE: *)
 *   (*     pattern → field =~ /pattern/  (O_MATCH node)  *)
 *   (*     except: if pattern contains ">["/"<[" or ends with ">","<" *)
 *   (*       followed by "=", it is parsed as a comparison expression.   *)
 *   (*   TOK_META:                                                       *)
 *   (*     pattern [ "=" value_pattern ] → has_tag(/pattern/ [, /value/]) (O_CALL node) *)
 *   (*   TOK_EXPR:                                                       *)
 *   (*     text → parsed as a raw value expression                       *)
 * @endverbatim
 *
 * Operator precedence (lowest to highest): `or`, `and`/implicit, `not`, atoms.
 */

#include <system.hh>

#include "query.h"
#include "op.h"

namespace ledger {

/*--- Lexer Utilities ---*/

// TODO: Extend this to handle all characters which define enclosures
bool query_t::lexer_t::unbalanced_braces(const string& str) {
  int balance = 0;
  for (const char& c : str) {
    if (c == '(')
      ++balance;
    else if (c == ')')
      if (--balance < 0)
        return true;
  }
  return balance != 0;
}

/*--- Lexer: Helper Methods ---*/

/// Scan a quoted or delimited pattern.  The opening delimiter (one of
/// `'`, `"`, `/`) is at *arg_i on entry; on return arg_i points past
/// the closing delimiter.
query_t::lexer_t::token_t query_t::lexer_t::scan_quoted_pattern() {
  string pat;
  char closing = *arg_i;
  bool is_regex = (closing == '/');
  bool found_closing = false;
  for (++arg_i; arg_i != arg_end; ++arg_i) {
    if (*arg_i == '\\') {
      if (++arg_i == arg_end)
        throw_(parse_error, _("Unexpected '\\' at end of pattern"));
      if (is_regex && *arg_i == '\\') {
        // Collapse \\\\ to \\ for backward compatibility: users who previously
        // wrote \\\\* to get regex \\* (literal star) still get the same result
        // (fixes #2946). Just emit the single backslash.
      } else if (is_regex && *arg_i != closing) {
        // For regex patterns /.../: preserve the backslash so the regex engine
        // can interpret escape sequences (e.g., \| for literal pipe). Only
        // \/ is consumed as a delimiter escape (backslash stripped).
        pat.push_back('\\');
      }
    } else if (*arg_i == closing) {
      ++arg_i;
      found_closing = true;
      break;
    }
    pat.push_back(*arg_i);
  }
  if (!found_closing)
    throw_(parse_error, _f("Expected '%1%' at end of pattern") % closing);
  if (pat.empty())
    throw_(parse_error, _("Match pattern is empty"));

  return token_t(token_t::TERM, pat);
}

/// Accumulate a bare-word identifier from arg_i until an operator boundary
/// character is reached or the argument string is exhausted.
///
/// @param tok_context  Determines which characters act as boundaries.
///                     In TOK_EXPR context, `(`, `&`, `|`, etc. are NOT
///                     boundaries (they're part of the expression).
/// @param consume_next When true (set by `\\` escape), the next operator
///                     character is consumed into the identifier instead
///                     of acting as a boundary.
/// @param hit_boundary Set to true if scanning stopped at an operator
///                     boundary (arg_i points at the boundary char);
///                     false if the entire argument was consumed.
string query_t::lexer_t::scan_identifier(token_t::kind_t tok_context, bool consume_next,
                                         bool& hit_boundary) {
  string ident;
  hit_boundary = false;

  for (; arg_i != arg_end; ++arg_i) {
    switch (*arg_i) {
    case ' ':
    case '\t':
    case '\n':
    case '\r':
      if (!multiple_args && !consume_whitespace && !consume_next_arg)
        hit_boundary = true;
      else
        ident.push_back(*arg_i);
      break;

    case ')':
      if (unbalanced_braces(ident))
        consume_next = true;
      if (!consume_next) {
        hit_boundary = true;
      } else {
        ident.push_back(*arg_i);
      }
      break;

    case '(':
    case '&':
    case '|':
    case '!':
    case '@':
    case '#':
    case '%':
    case '=':
      if (!consume_next && tok_context != token_t::TOK_EXPR) {
        hit_boundary = true;
      } else {
        ident.push_back(*arg_i);
      }
      break;

    default:
      ident.push_back(*arg_i);
      break;
    }

    if (hit_boundary)
      break;
  }

  if (!hit_boundary)
    consume_whitespace = false;

  // Strip trailing whitespace (can accumulate when multiple_args is true
  // and whitespace precedes an operator).
  while (!ident.empty() && (ident.back() == ' ' || ident.back() == '\t' || ident.back() == '\r' ||
                            ident.back() == '\n'))
    ident.pop_back();

  return ident;
}

/// Match a completed identifier string against known query keywords.
/// If the identifier is a keyword, return the corresponding token;
/// otherwise return a TERM token containing the identifier text.
query_t::lexer_t::token_t query_t::lexer_t::match_keyword(const string& ident,
                                                          string::const_iterator ident_start) {
  // When multiple_args is true, whitespace is included in identifiers, so
  // "expr foo" may be read as a single token instead of recognizing "expr"
  // as a keyword.  Detect this case and rewind arg_i to after the keyword
  // so the remainder is consumed as the expression argument.
  if (multiple_args && ident.size() > 4 && ident.substr(0, 4) == "expr" &&
      (ident[4] == ' ' || ident[4] == '\t')) {
    arg_i = ident_start + 4;
    consume_next_arg = true;
    return token_t(token_t::TOK_EXPR);
  }

  // NOLINTBEGIN(bugprone-branch-clone)
  if (ident == "and")
    return token_t(token_t::TOK_AND);
  else if (ident == "or")
    return token_t(token_t::TOK_OR);
  else if (ident == "not")
    return token_t(token_t::TOK_NOT);
  else if (ident == "code")
    return token_t(token_t::TOK_CODE);
  else if (ident == "desc")
    return token_t(token_t::TOK_PAYEE);
  else if (ident == "payee")
    return token_t(token_t::TOK_PAYEE);
  else if (ident == "note")
    return token_t(token_t::TOK_NOTE);
  else if (ident == "tag")
    return token_t(token_t::TOK_META);
  else if (ident == "meta")
    return token_t(token_t::TOK_META);
  else if (ident == "data")
    return token_t(token_t::TOK_META);
  else if (ident == "show")
    return token_t(token_t::TOK_SHOW);
  else if (ident == "only")
    return token_t(token_t::TOK_ONLY);
  else if (ident == "bold")
    return token_t(token_t::TOK_BOLD);
  else if (ident == "for")
    return token_t(token_t::TOK_FOR);
  else if (ident == "since")
    return token_t(token_t::TOK_SINCE);
  else if (ident == "until")
    return token_t(token_t::TOK_UNTIL);
  else if (ident == "expr") {
    consume_next_arg = true;
    return token_t(token_t::TOK_EXPR);
  } else
    return token_t(token_t::TERM, ident);
  // NOLINTEND(bugprone-branch-clone)
}

/*--- Lexer: Token Production ---*/

query_t::lexer_t::token_t
query_t::lexer_t::next_token(query_t::lexer_t::token_t::kind_t tok_context) {
  // Return cached token if one was pushed back by peek_token() or push_token().
  if (token_cache.kind != token_t::UNKNOWN) {
    token_t tok = token_cache;
    token_cache = token_t();
    return tok;
  }

  // Main scanning loop: each iteration processes one character or advances
  // to the next argument.  Whitespace is consumed by looping back to the
  // top, replacing the former `goto resume`.
  while (true) {
    // Advance to the next argument string if the current one is exhausted.
    if (arg_i == arg_end) {
      if (begin == end || ++begin == end)
        return token_t(token_t::END_REACHED);
      arg_i = (*begin).as_string().begin();
      arg_end = (*begin).as_string().end();
    }

    // Phase 1: Quoted/delimited patterns.
    if (*arg_i == '\'' || *arg_i == '"' || *arg_i == '/')
      return scan_quoted_pattern();

    // Phase 2: When consume_next_arg is set (by the `expr` keyword), consume
    // the entire remainder of the current argument as a single TERM.
    if (multiple_args && consume_next_arg) {
      consume_next_arg = false;
      token_t tok(token_t::TERM, string(arg_i, arg_end));
      prev_arg_i = arg_i;
      arg_i = arg_end;
      return tok;
    }

    // Phase 3: Recognize single-character operators.  Whitespace loops back
    // to the top of the scanning loop.  Other operators return immediately.
    // Backslash or unrecognized characters fall through to Phase 4.
    bool consume_next = false;
    switch (*arg_i) {
    case ' ':
    case '\t':
    case '\r':
    case '\n':
      ++arg_i;
      continue; // skip whitespace, re-enter loop to advance arg if needed

    case '(':
      ++arg_i;
      if (tok_context == token_t::TOK_EXPR)
        consume_whitespace = true;
      return token_t(token_t::LPAREN);
    case ')':
      ++arg_i;
      if (tok_context == token_t::TOK_EXPR)
        consume_whitespace = false;
      return token_t(token_t::RPAREN);
    case '&':
      ++arg_i;
      return token_t(token_t::TOK_AND);
    case '|':
      ++arg_i;
      return token_t(token_t::TOK_OR);
    case '!':
      ++arg_i;
      return token_t(token_t::TOK_NOT);
    case '@':
      ++arg_i;
      return token_t(token_t::TOK_PAYEE);
    case '#':
      ++arg_i;
      return token_t(token_t::TOK_CODE);
    case '%':
      ++arg_i;
      return token_t(token_t::TOK_META);
    case '=':
      if (arg_i == (*begin).as_string().begin()) {
        ++arg_i;
        return token_t(token_t::TOK_NOTE);
      }
      ++arg_i;
      consume_next = true;
      return token_t(token_t::TOK_EQ);

    case '\\':
      consume_next = true;
      ++arg_i;
      [[fallthrough]];
    default:
      break; // fall through to Phase 4
    }

    // Phase 4: Accumulate an identifier and match against keywords.
    string::const_iterator ident_start = arg_i;
    bool hit_boundary = false;
    string ident = scan_identifier(tok_context, consume_next, hit_boundary);
    return match_keyword(ident, ident_start);
  }
}

void query_t::lexer_t::token_t::expected(char wanted) {
  throw_(parse_error, _f("Missing '%1%'") % wanted);
}

/*--- Parser: Recursive-Descent Expression Construction ---*/

/**
 * Build a metadata matching node: has_tag(pattern), or has_tag(pattern, value)
 * if followed by `= value_pattern`.
 */
expr_t::ptr_op_t query_t::parser_t::make_meta_node(const string& tag_pattern,
                                                   query_t::lexer_t::token_t::kind_t tok_context) {
  expr_t::ptr_op_t node = new expr_t::op_t(expr_t::op_t::O_CALL);

  expr_t::ptr_op_t ident = new expr_t::op_t(expr_t::op_t::IDENT);
  ident->set_ident("has_tag");
  node->set_left(ident);

  expr_t::ptr_op_t arg1 = new expr_t::op_t(expr_t::op_t::VALUE);
  arg1->set_value(mask_t(tag_pattern));

  lexer_t::token_t tok = lexer.peek_token(tok_context);
  if (tok.kind == lexer_t::token_t::TOK_EQ) {
    tok = lexer.next_token(tok_context);
    tok = lexer.next_token(tok_context);
    if (tok.kind != lexer_t::token_t::TERM)
      throw_(parse_error, _("Metadata equality operator not followed by term"));

    expr_t::ptr_op_t arg2 = new expr_t::op_t(expr_t::op_t::VALUE);
    assert(tok.value);
    arg2->set_value(mask_t(*tok.value));

    node->set_right(expr_t::op_t::new_node(expr_t::op_t::O_CONS, arg1, arg2));
  } else {
    node->set_right(arg1);
  }
  return node;
}

/**
 * Try to parse a term as a comparison expression.
 *
 * Supports two forms:
 * - "d>" followed by TOK_EQ and a term → parse "d>=[date]" as expression
 * - "d>[date]" with embedded comparison → parse the whole term as expression
 *
 * @return The expression node, or nullptr if the term is not a comparison.
 */
expr_t::ptr_op_t
query_t::parser_t::try_parse_comparison(const string& term,
                                        query_t::lexer_t::token_t::kind_t tok_context) {
  bool ends_with_cmp = !term.empty() && (term.back() == '>' || term.back() == '<');

  if (ends_with_cmp && lexer.peek_token(tok_context).kind == lexer_t::token_t::TOK_EQ) {
    lexer.next_token(tok_context); // consume the '='
    lexer_t::token_t rhs = lexer.next_token(tok_context);
    if (rhs.kind == lexer_t::token_t::TERM && rhs.value) {
      try {
        string expr_str = term + "=" + *rhs.value;
        return expr_t(expr_str).get_op();
      } catch (...) {
        return nullptr;
      }
    }
  } else if (!ends_with_cmp &&
             (term.find(">[") != string::npos || term.find("<[") != string::npos)) {
    try {
      return expr_t(term).get_op();
    } catch (...) {
      return nullptr;
    }
  }
  return nullptr;
}

/**
 * Build an O_MATCH node that regex-matches a pattern against the field
 * indicated by tok_context.  E.g., in TOK_ACCOUNT context, "food" becomes:
 *   account =~ /food/
 */
expr_t::ptr_op_t query_t::parser_t::make_match_node(query_t::lexer_t::token_t::kind_t tok_context,
                                                    const string& pattern) {
  expr_t::ptr_op_t node = new expr_t::op_t(expr_t::op_t::O_MATCH);

  expr_t::ptr_op_t ident = new expr_t::op_t(expr_t::op_t::IDENT);
  switch (tok_context) {
  case lexer_t::token_t::TOK_ACCOUNT:
    ident->set_ident("account");
    break;
  case lexer_t::token_t::TOK_PAYEE:
    ident->set_ident("payee");
    break;
  case lexer_t::token_t::TOK_CODE:
    ident->set_ident("code");
    break;
  case lexer_t::token_t::TOK_NOTE:
    ident->set_ident("note");
    break;
  default:
    assert(false);
    break;
  }

  expr_t::ptr_op_t mask = new expr_t::op_t(expr_t::op_t::VALUE);
  DEBUG("query.mask", "Mask from string: " << pattern);
  mask->set_value(mask_t(pattern));
  DEBUG("query.mask", "Mask is: " << mask->as_value().as_mask().str());

  node->set_left(ident);
  node->set_right(mask);
  return node;
}

/**
 * Parse a single query term -- the lowest level of the grammar.
 *
 * This handles:
 * - **Field context switches**: `code`, `payee`, `note`, `account`, `meta`,
 *   `expr` tokens change tok_context and recurse to get the actual term.
 * - **Section boundaries**: `show`, `only`, `bold`, `for`, `since`, `until`
 *   are pushed back for the top-level parse_query_expr to handle.
 * - **TERM tokens**: Depending on tok_context, become either:
 *   - An O_MATCH node (`account =~ /pattern/`) for account/payee/code/note.
 *   - An O_CALL to `has_tag()` for metadata, optionally with a value match.
 *   - A raw expression parse for `expr` context.
 *   - A comparison expression for terms containing `>[` or `<[` syntax.
 * - **Parenthesized sub-expressions**: Recurse into parse_query_expr.
 */
expr_t::ptr_op_t
query_t::parser_t::parse_query_term(query_t::lexer_t::token_t::kind_t tok_context) {
  expr_t::ptr_op_t node;

  lexer_t::token_t tok = lexer.next_token(tok_context);
  // NOLINTBEGIN(bugprone-branch-clone)
  switch (tok.kind) {
  case lexer_t::token_t::TOK_SHOW:
  case lexer_t::token_t::TOK_ONLY:
  case lexer_t::token_t::TOK_BOLD:
  case lexer_t::token_t::TOK_FOR:
  case lexer_t::token_t::TOK_SINCE:
  case lexer_t::token_t::TOK_UNTIL:
  case lexer_t::token_t::END_REACHED:
    lexer.push_token(tok);
    break;

  // Field context operators: switch the matching target and recurse to
  // parse the following term under the new context.
  case lexer_t::token_t::TOK_CODE:
  case lexer_t::token_t::TOK_PAYEE:
  case lexer_t::token_t::TOK_NOTE:
  case lexer_t::token_t::TOK_ACCOUNT:
  case lexer_t::token_t::TOK_META:
  case lexer_t::token_t::TOK_EXPR:
    node = parse_query_term(tok.kind);
    if (!node)
      throw_(parse_error, _f("%1% operator not followed by argument") % tok.symbol());
    break;

  case lexer_t::token_t::TERM:
    assert(tok.value);
    switch (tok_context) {
    case lexer_t::token_t::TOK_EXPR:
      node = expr_t(*tok.value).get_op();
      break;

    case lexer_t::token_t::TOK_META:
      node = make_meta_node(*tok.value, tok_context);
      break;

    default:
      node = try_parse_comparison(*tok.value, tok_context);
      if (!node)
        node = make_match_node(tok_context, *tok.value);
      break;
    }
    break;

  case lexer_t::token_t::LPAREN:
    if (++parse_depth > MAX_PARSE_DEPTH)
      throw_(parse_error, _("Query expression nested too deeply"));
    node = parse_query_expr(tok_context, true);
    --parse_depth;
    tok = lexer.next_token(tok_context);
    if (tok.kind != lexer_t::token_t::RPAREN)
      tok.expected(')');
    break;

  default:
    lexer.push_token(tok);
    break;
  }
  // NOLINTEND(bugprone-branch-clone)

  return node;
}

/// Parse a unary expression: either `not <term>` or a plain query term.
expr_t::ptr_op_t query_t::parser_t::parse_unary_expr(lexer_t::token_t::kind_t tok_context) {
  expr_t::ptr_op_t node;

  lexer_t::token_t tok = lexer.next_token(tok_context);
  switch (tok.kind) {
  case lexer_t::token_t::TOK_NOT: {
    expr_t::ptr_op_t term(parse_query_term(tok_context));
    if (!term)
      throw_(parse_error, _f("%1% operator not followed by argument") % tok.symbol());

    node = new expr_t::op_t(expr_t::op_t::O_NOT);
    node->set_left(term);
    break;
  }

  default:
    lexer.push_token(tok);
    node = parse_query_term(tok_context);
    break;
  }

  return node;
}

/// Parse a left-associative chain of AND-connected unary expressions.
expr_t::ptr_op_t query_t::parser_t::parse_and_expr(lexer_t::token_t::kind_t tok_context) {
  if (expr_t::ptr_op_t node = parse_unary_expr(tok_context)) {
    while (true) {
      lexer_t::token_t tok = lexer.next_token(tok_context);
      if (tok.kind == lexer_t::token_t::TOK_AND) {
        expr_t::ptr_op_t prev(node);
        node = new expr_t::op_t(expr_t::op_t::O_AND);
        node->set_left(prev);
        node->set_right(parse_unary_expr(tok_context));
        if (!node->right())
          throw_(parse_error, _f("%1% operator not followed by argument") % tok.symbol());
      } else {
        lexer.push_token(tok);
        break;
      }
    }
    return node;
  }
  return expr_t::ptr_op_t();
}

/// Parse a left-associative chain of OR-connected AND expressions.
expr_t::ptr_op_t query_t::parser_t::parse_or_expr(lexer_t::token_t::kind_t tok_context) {
  if (expr_t::ptr_op_t node = parse_and_expr(tok_context)) {
    while (true) {
      lexer_t::token_t tok = lexer.next_token(tok_context);
      if (tok.kind == lexer_t::token_t::TOK_OR) {
        expr_t::ptr_op_t prev(node);
        node = new expr_t::op_t(expr_t::op_t::O_OR);
        node->set_left(prev);
        node->set_right(parse_and_expr(tok_context));
        if (!node->right())
          throw_(parse_error, _f("%1% operator not followed by argument") % tok.symbol());
      } else {
        lexer.push_token(tok);
        break;
      }
    }
    return node;
  }
  return expr_t::ptr_op_t();
}

/*--- Parser: Top-Level Query Expression and Section Splitting ---*/

/**
 * Parse a complete query expression, handling section keywords.
 *
 * The top-level grammar is:
 *   query := or_expr* ['show' or_expr*] ['only' or_expr*]
 *            ['bold' or_expr*] ['for'|'since'|'until' date_terms...]
 *
 * Multiple consecutive or_expr results at the top level are implicitly
 * OR-ed together (bare terms like `food groceries` mean
 * `account =~ /food/ | account =~ /groceries/`).
 *
 * When not parsing a subexpression, section keywords split the query
 * into separate predicates stored in query_map.
 */
expr_t::ptr_op_t query_t::parser_t::parse_query_expr(lexer_t::token_t::kind_t tok_context,
                                                     bool subexpression) {
  expr_t::ptr_op_t limiter;

  // Parse the main (limit) predicate: accumulate OR-connected expressions.
  while (expr_t::ptr_op_t next = parse_or_expr(tok_context)) {
    if (!limiter) {
      limiter = next;
    } else {
      expr_t::ptr_op_t prev(limiter);
      limiter = new expr_t::op_t(expr_t::op_t::O_OR);
      limiter->set_left(prev);
      limiter->set_right(next);
    }
  }

  // For top-level queries, store the limit predicate and then process
  // any trailing section keywords (show, only, bold, for/since/until).
  if (!subexpression) {
    if (limiter)
      query_map.insert(
          query_map_t::value_type(QUERY_LIMIT, predicate_t(limiter, what_to_keep).print_to_str()));

    // Process section keywords that follow the main predicate.
    bool sections_done = false;
    lexer_t::token_t tok = lexer.peek_token(tok_context);
    while (tok.kind != lexer_t::token_t::END_REACHED && !sections_done) {
      switch (tok.kind) {

      // show/only/bold: parse the following expressions as a separate
      // predicate and store under the appropriate query kind.
      case lexer_t::token_t::TOK_SHOW:
      case lexer_t::token_t::TOK_ONLY:
      case lexer_t::token_t::TOK_BOLD: {
        lexer.next_token(tok_context);

        kind_t kind = QUERY_SHOW; // Initialize with a default value
        switch (tok.kind) {
        case lexer_t::token_t::TOK_SHOW:
          kind = QUERY_SHOW;
          break;
        case lexer_t::token_t::TOK_ONLY:
          kind = QUERY_ONLY;
          break;
        case lexer_t::token_t::TOK_BOLD:
          kind = QUERY_BOLD;
          break;
        default:
          break;
        }

        expr_t::ptr_op_t node;
        while (expr_t::ptr_op_t next = parse_or_expr(tok_context)) {
          if (!node) {
            node = next;
          } else {
            expr_t::ptr_op_t prev(node);
            node = new expr_t::op_t(expr_t::op_t::O_OR);
            node->set_left(prev);
            node->set_right(next);
          }
        }

        if (node)
          query_map.insert(
              query_map_t::value_type(kind, predicate_t(node, what_to_keep).print_to_str()));
        break;
      }

      // for/since/until: accumulate the remaining terms as a date range
      // string (not parsed into an expression tree here -- it is passed
      // to the period parser later).
      case lexer_t::token_t::TOK_FOR:
      case lexer_t::token_t::TOK_SINCE:
      case lexer_t::token_t::TOK_UNTIL: {
        tok = lexer.next_token(tok_context);

        string for_string;

        if (tok.kind == lexer_t::token_t::TOK_SINCE)
          for_string = "since";
        else if (tok.kind == lexer_t::token_t::TOK_UNTIL)
          for_string = "until";

        lexer.consume_next_arg = true;
        tok = lexer.peek_token(tok_context);

        while (tok.kind != lexer_t::token_t::END_REACHED) {
          tok = lexer.next_token(tok_context);
          if (tok.kind != lexer_t::token_t::TERM)
            throw_(parse_error, _("Unexpected token in date range expression"));

          if (*tok.value == "show" || *tok.value == "bold" || *tok.value == "for" ||
              *tok.value == "since" || *tok.value == "until") {
            lexer.token_cache = lexer_t::token_t();
            lexer.arg_i = lexer.prev_arg_i;
            lexer.consume_next_arg = false;
            break;
          }

          if (!for_string.empty())
            for_string += " ";
          for_string += *tok.value;

          lexer.consume_next_arg = true;
          tok = lexer.peek_token(tok_context);
        }

        if (!for_string.empty())
          query_map.insert(query_map_t::value_type(QUERY_FOR, for_string));
        break;
      }

      default:
        sections_done = true;
        break;
      }

      if (!sections_done)
        tok = lexer.peek_token(tok_context);
    }
  }

  return limiter;
}

} // namespace ledger
