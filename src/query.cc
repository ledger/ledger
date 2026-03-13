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

/*--- Lexer: Token Production ---*/

query_t::lexer_t::token_t
query_t::lexer_t::next_token(query_t::lexer_t::token_t::kind_t tok_context) {
  // Return cached token if one was pushed back by peek_token() or push_token().
  if (token_cache.kind != token_t::UNKNOWN) {
    token_t tok = token_cache;
    token_cache = token_t();
    return tok;
  }

  // Advance to the next argument string if the current one is exhausted.
  if (arg_i == arg_end) {
    if (begin == end || ++begin == end) {
      return token_t(token_t::END_REACHED);
    } else {
      arg_i = (*begin).as_string().begin();
      arg_end = (*begin).as_string().end();
    }
  }

resume:
  // Phase 1: Handle quoted/delimited patterns.  Single quotes, double
  // quotes, and forward slashes all delimit literal pattern strings that
  // become TERM tokens.  Backslash escapes are supported within patterns.
  switch (*arg_i) {
  case '\0':
    assert(false);
    break;

  case '\'':
  case '"':
  case '/': {
    string pat;
    char closing = *arg_i;
    bool found_closing = false;
    for (++arg_i; arg_i != arg_end; ++arg_i) {
      if (*arg_i == '\\') {
        if (++arg_i == arg_end)
          throw_(parse_error, _("Unexpected '\\' at end of pattern"));
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
  }

  // Phase 2: When consume_next_arg is set (by the `expr` keyword), consume
  // the entire remainder of the current argument as a single TERM.
  if (multiple_args && consume_next_arg) {
    consume_next_arg = false;
    token_t tok(token_t::TERM, string(arg_i, arg_end));
    prev_arg_i = arg_i;
    arg_i = arg_end;
    return tok;
  }

  // Phase 3: Recognize single-character operators and then fall through
  // to identifier/keyword scanning for multi-character tokens.
  bool consume_next = false;
  switch (*arg_i) {
  case '\0':
    assert(false);
    break;

  case ' ':
  case '\t':
  case '\r':
  case '\n':
    if (++arg_i == arg_end)
      return next_token(tok_context);
    goto resume;

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
  default: {
    // Phase 4: Accumulate an identifier (bare word) and then test whether
    // it matches a keyword.  If not, it becomes a TERM token.
    string ident;
    string::const_iterator ident_start = arg_i;
    for (; arg_i != arg_end; ++arg_i) {
      switch (*arg_i) {
      case '\0':
        assert(false);
        break;

      case ' ':
      case '\t':
      case '\n':
      case '\r':
        if (!multiple_args && !consume_whitespace && !consume_next_arg) {
          // When parsing a single predicate string (not multiple command-line
          // args), spaces normally terminate the current token.  However,
          // account names can contain embedded spaces (e.g. "income:b c").
          // To support unquoted multi-word account names, peek ahead: if the
          // next non-whitespace content is a query keyword or a special
          // operator character, stop the token as usual.  If it is a plain
          // word, include this space in the token so the whole thing becomes
          // one TERM (equivalent to quoting the name).
          //
          // This means "= income:b c" matches "income:b c" exactly, while
          // "= income:b and income:c" still parses as two AND-ed patterns.
          auto is_query_kw = [](const string& w) -> bool {
            return w == "and" || w == "or"  || w == "not"   ||
                   w == "expr"                               ||
                   w == "code" || w == "payee" || w == "desc"  ||
                   w == "note" || w == "tag"   || w == "meta"  || w == "data" ||
                   w == "show" || w == "only"  || w == "bold"  ||
                   w == "for"  || w == "since" || w == "until";
          };
          // If the token accumulated so far is itself a keyword, stop now.
          if (is_query_kw(ident))
            goto test_ident;
          // Peek past this whitespace run.
          auto peek = arg_i;
          ++peek;
          while (peek != arg_end &&
                 (*peek == ' ' || *peek == '\t' || *peek == '\n' || *peek == '\r'))
            ++peek;
          if (peek == arg_end) {
            // Trailing whitespace only — stop the token.
            goto test_ident;
          }
          // A special operator character always stops the token.  Also stop
          // for '^' since it begins a regex anchor (e.g. "~ ^A" should stay
          // as two separate TERM tokens, not collapse into "~ ^A").
          char ahead = *peek;
          if (ahead == '&' || ahead == '|' || ahead == '!' || ahead == '@' ||
              ahead == '#' || ahead == '%' || ahead == '(' || ahead == ')' ||
              ahead == '/' || ahead == '\'' || ahead == '"' || ahead == '=' ||
              ahead == '^') {
            goto test_ident;
          }
          // Read ahead to find the boundary of the next word and test it.
          auto word_end = peek;
          while (word_end != arg_end &&
                 *word_end != ' ' && *word_end != '\t' &&
                 *word_end != '\n' && *word_end != '\r' &&
                 *word_end != '&' && *word_end != '|' && *word_end != '!' &&
                 *word_end != '(' && *word_end != ')' && *word_end != '@' &&
                 *word_end != '#' && *word_end != '%' && *word_end != '=' &&
                 *word_end != '/' && *word_end != '\'' && *word_end != '"')
            ++word_end;
          if (is_query_kw(string(peek, word_end)))
            goto test_ident;
          // The following content is a plain word: absorb this whitespace run
          // as a single space and advance arg_i so the loop's ++arg_i lands
          // on peek (first char of that plain word).
          ident.push_back(' ');
          arg_i = peek - 1;
        } else
          ident.push_back(*arg_i);
        break;

      case ')':
        if (unbalanced_braces(ident))
          consume_next = true;
        if (!consume_next && tok_context == token_t::TOK_EXPR)
          goto test_ident;
        [[fallthrough]];
      case '(':
      case '&':
      case '|':
      case '!':
      case '@':
      case '#':
      case '%':
      case '=':
        if (!consume_next && tok_context != token_t::TOK_EXPR)
          goto test_ident;
        [[fallthrough]];
      default:
        ident.push_back(*arg_i);
        break;
      }
    }
    consume_whitespace = false;

  test_ident:
    // Strip trailing whitespace from the parsed identifier (can accumulate
    // when multiple_args is true and whitespace precedes an operator)
    while (!ident.empty() && (ident.back() == ' ' || ident.back() == '\t' || ident.back() == '\r' ||
                              ident.back() == '\n'))
      ident.pop_back();
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
      // The expr keyword takes the whole of the next string as its argument.
      consume_next_arg = true;
      return token_t(token_t::TOK_EXPR);
    } else
      return token_t(token_t::TERM, ident);
    // NOLINTEND(bugprone-branch-clone)
  }
  }

  return token_t(token_t::UNKNOWN);
}

void query_t::lexer_t::token_t::expected(char wanted) {
  throw_(parse_error, _f("Missing '%1%'") % wanted);
}

/*--- Parser: Recursive-Descent Expression Construction ---*/

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

    case lexer_t::token_t::TOK_META: {
      // Metadata matching: build a call to has_tag(pattern), optionally
      // with a second argument for value matching: has_tag(pattern, value_pattern).
      node = new expr_t::op_t(expr_t::op_t::O_CALL);

      expr_t::ptr_op_t ident = new expr_t::op_t(expr_t::op_t::IDENT);
      ident->set_ident("has_tag");
      node->set_left(ident);

      expr_t::ptr_op_t arg1 = new expr_t::op_t(expr_t::op_t::VALUE);
      arg1->set_value(mask_t(*tok.value));

      tok = lexer.peek_token(tok_context);
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
      break;
    }

    default: {
      // Check if this term looks like a comparison expression rather than an
      // account name pattern.  This supports "d>=[2014/01/01]" syntax in
      // automated transaction predicates.
      //
      // Case 1: term ends with > or < (e.g., "d>" from "d>=[date]")
      //   followed by TOK_EQ and another term → parse as expression "d>=[date]"
      // Case 2: term contains ">["  or "<[" pattern (e.g., "d>[date]")
      //   → parse the whole term as an expression
      assert(tok.value);
      {
        const string& term = *tok.value;
        bool ends_with_cmp = !term.empty() && (term.back() == '>' || term.back() == '<');

        if (ends_with_cmp && lexer.peek_token(tok_context).kind == lexer_t::token_t::TOK_EQ) {
          lexer.next_token(tok_context); // consume the '='
          lexer_t::token_t rhs = lexer.next_token(tok_context);
          if (rhs.kind == lexer_t::token_t::TERM && rhs.value) {
            try {
              string expr_str = term + "=" + *rhs.value;
              node = expr_t(expr_str).get_op();
            } catch (...) {
              node = nullptr;
            }
          }
        } else if (!ends_with_cmp &&
                   (term.find(">[") != string::npos || term.find("<[") != string::npos)) {
          try {
            node = expr_t(term).get_op();
          } catch (...) {
            node = nullptr;
          }
        }
      }

      // Default case: build an O_MATCH node that regex-matches the term
      // against the appropriate field (account, payee, code, or note).
      // E.g., "food" in account context becomes: account =~ /food/
      if (!node) {
        node = new expr_t::op_t(expr_t::op_t::O_MATCH);

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
        DEBUG("query.mask", "Mask from string: " << *tok.value);
        mask->set_value(mask_t(*tok.value));
        DEBUG("query.mask", "Mask is: " << mask->as_value().as_mask().str());

        node->set_left(ident);
        node->set_right(mask);
      }
    }
    }
    break;

  case lexer_t::token_t::LPAREN:
    node = parse_query_expr(tok_context, true);
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
    lexer_t::token_t tok = lexer.peek_token(tok_context);
    while (tok.kind != lexer_t::token_t::END_REACHED) {
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
          assert(tok.kind == lexer_t::token_t::TERM);

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
        goto done;
      }

      tok = lexer.peek_token(tok_context);
    }
  done:;
  }

  return limiter;
}

} // namespace ledger
