/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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

#include "query.h"
#include "op.h"

namespace ledger {

query_t::lexer_t::token_t
query_t::lexer_t::next_token(query_t::lexer_t::token_t::kind_t tok_context)
{
  if (token_cache.kind != token_t::UNKNOWN) {
    token_t tok = token_cache;
    token_cache = token_t();
    return tok;
  }

  if (arg_i == arg_end) {
    if (begin == end || ++begin == end) {
      return token_t(token_t::END_REACHED);
    } else {
      arg_i   = (*begin).as_string().begin();
      arg_end = (*begin).as_string().end();
    }
  }

 resume:
  switch (*arg_i) {
  case '\0':
    assert(false);
    break;

  case '\'':
  case '"':
  case '/': {
    string pat;
    char   closing       = *arg_i;
    bool   found_closing = false;
    for (++arg_i; arg_i != arg_end; ++arg_i) {
      if (*arg_i == '\\') {
        if (++arg_i == arg_end)
          throw_(parse_error, _("Unexpected '\\' at end of pattern"));
      }
      else if (*arg_i == closing) {
        ++arg_i;
        found_closing = true;
        break;
      }
      pat.push_back(*arg_i);
    }
    if (! found_closing)
      throw_(parse_error, _f("Expected '%1%' at end of pattern") % closing);
    if (pat.empty())
      throw_(parse_error, _("Match pattern is empty"));

    return token_t(token_t::TERM, pat);
  }
  }

  if (multiple_args && consume_next_arg) {
    consume_next_arg = false;
    token_t tok(token_t::TERM, string(arg_i, arg_end));
    prev_arg_i = arg_i;
    arg_i = arg_end;
    return tok;
  }

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
  case '&': ++arg_i; return token_t(token_t::TOK_AND);
  case '|': ++arg_i; return token_t(token_t::TOK_OR);
  case '!': ++arg_i; return token_t(token_t::TOK_NOT);
  case '@': ++arg_i; return token_t(token_t::TOK_PAYEE);
  case '#': ++arg_i; return token_t(token_t::TOK_CODE);
  case '%': ++arg_i; return token_t(token_t::TOK_META);
  case '=':
    ++arg_i;
    consume_next = true;
    return token_t(token_t::TOK_EQ);

  case '\\':
    consume_next = true;
    ++arg_i;
    // fall through...
  default: {
    string ident;
    for (; arg_i != arg_end; ++arg_i) {
      switch (*arg_i) {
      case '\0':
        assert(false);
        break;

      case ' ':
      case '\t':
      case '\n':
      case '\r':
        if (! multiple_args && ! consume_whitespace && ! consume_next_arg)
          goto test_ident;
        else
          ident.push_back(*arg_i);
        break;

      case ')':
        if (! consume_next && tok_context == token_t::TOK_EXPR)
          goto test_ident;
      case '(':
      case '&':
      case '|':
      case '!':
      case '@':
      case '#':
      case '%':
      case '=':
        if (! consume_next && tok_context != token_t::TOK_EXPR)
          goto test_ident;
      // fall through...
      default:
        ident.push_back(*arg_i);
        break;
      }
    }
    consume_whitespace = false;

test_ident:
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
    }
    else
      return token_t(token_t::TERM, ident);
  }
  }

  return token_t(token_t::UNKNOWN);
}

void query_t::lexer_t::token_t::unexpected()
{
  kind_t prev_kind = kind;

  kind = UNKNOWN;

  switch (prev_kind) {
  case END_REACHED:
    throw_(parse_error, _("Unexpected end of expression"));
  case TERM:
    throw_(parse_error, _f("Unexpected string '%1%'") % *value);
  default:
    throw_(parse_error, _f("Unexpected token '%1%'") % symbol());
  }
}

void query_t::lexer_t::token_t::expected(char wanted, char c)
{
  kind = UNKNOWN;

  if (c == '\0' || c == -1) {
    if (wanted == '\0' || wanted == -1)
      throw_(parse_error, _("Unexpected end"));
    else
      throw_(parse_error, _f("Missing '%1%'") % wanted);
  } else {
    if (wanted == '\0' || wanted == -1)
      throw_(parse_error, _f("Invalid char '%1%'") % c);
    else
      throw_(parse_error, _f("Invalid char '%1%' (wanted '%2%')") % c % wanted);
  }
}

expr_t::ptr_op_t
query_t::parser_t::parse_query_term(query_t::lexer_t::token_t::kind_t tok_context)
{
  expr_t::ptr_op_t node;

  lexer_t::token_t tok = lexer.next_token(tok_context);
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

  case lexer_t::token_t::TOK_CODE:
  case lexer_t::token_t::TOK_PAYEE:
  case lexer_t::token_t::TOK_NOTE:
  case lexer_t::token_t::TOK_ACCOUNT:
  case lexer_t::token_t::TOK_META:
  case lexer_t::token_t::TOK_EXPR:
    node = parse_query_term(tok.kind);
    if (! node)
      throw_(parse_error,
             _f("%1% operator not followed by argument") % tok.symbol());
    break;

  case lexer_t::token_t::TERM:
    assert(tok.value);
    switch (tok_context) {
    case lexer_t::token_t::TOK_EXPR:
      node = expr_t(*tok.value).get_op();
      break;

    case lexer_t::token_t::TOK_META: {
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
          throw_(parse_error,
                 _("Metadata equality operator not followed by term"));

        expr_t::ptr_op_t arg2 = new expr_t::op_t(expr_t::op_t::VALUE);
        assert(tok.value);
        arg2->set_value(mask_t(*tok.value));

        node->set_right(expr_t::op_t::new_node
                        (expr_t::op_t::O_SEQ,
                         expr_t::op_t::new_node
                         (expr_t::op_t::O_CONS, arg1, arg2)));
      } else {
        node->set_right(arg1);
      }
      break;
    }

    default: {
      node = new expr_t::op_t(expr_t::op_t::O_MATCH);

      expr_t::ptr_op_t ident = new expr_t::op_t(expr_t::op_t::IDENT);
      switch (tok_context) {
      case lexer_t::token_t::TOK_ACCOUNT:
        ident->set_ident("account"); break;
      case lexer_t::token_t::TOK_PAYEE:
        ident->set_ident("payee"); break;
      case lexer_t::token_t::TOK_CODE:
        ident->set_ident("code"); break;
      case lexer_t::token_t::TOK_NOTE:
        ident->set_ident("note"); break;
      default:
        assert(false); break;
      }

      expr_t::ptr_op_t mask = new expr_t::op_t(expr_t::op_t::VALUE);
      DEBUG("query.mask", "Mask from string: " << *tok.value);
      mask->set_value(mask_t(*tok.value));
      DEBUG("query.mask", "Mask is: " << mask->as_value().as_mask().str());

      node->set_left(ident);
      node->set_right(mask);
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

  return node;
}

expr_t::ptr_op_t
query_t::parser_t::parse_unary_expr(lexer_t::token_t::kind_t tok_context)
{
  expr_t::ptr_op_t node;

  lexer_t::token_t tok = lexer.next_token(tok_context);
  switch (tok.kind) {
  case lexer_t::token_t::TOK_NOT: {
    expr_t::ptr_op_t term(parse_query_term(tok_context));
    if (! term)
      throw_(parse_error,
             _f("%1% operator not followed by argument") % tok.symbol());

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

expr_t::ptr_op_t
query_t::parser_t::parse_and_expr(lexer_t::token_t::kind_t tok_context)
{
  if (expr_t::ptr_op_t node = parse_unary_expr(tok_context)) {
    while (true) {
      lexer_t::token_t tok = lexer.next_token(tok_context);
      if (tok.kind == lexer_t::token_t::TOK_AND) {
        expr_t::ptr_op_t prev(node);
        node = new expr_t::op_t(expr_t::op_t::O_AND);
        node->set_left(prev);
        node->set_right(parse_unary_expr(tok_context));
        if (! node->right())
          throw_(parse_error,
                 _f("%1% operator not followed by argument") % tok.symbol());
      } else {
        lexer.push_token(tok);
        break;
      }
    }
    return node;
  }
  return expr_t::ptr_op_t();
}

expr_t::ptr_op_t
query_t::parser_t::parse_or_expr(lexer_t::token_t::kind_t tok_context)
{
  if (expr_t::ptr_op_t node = parse_and_expr(tok_context)) {
    while (true) {
      lexer_t::token_t tok = lexer.next_token(tok_context);
      if (tok.kind == lexer_t::token_t::TOK_OR) {
        expr_t::ptr_op_t prev(node);
        node = new expr_t::op_t(expr_t::op_t::O_OR);
        node->set_left(prev);
        node->set_right(parse_and_expr(tok_context));
        if (! node->right())
          throw_(parse_error,
                 _f("%1% operator not followed by argument") % tok.symbol());
      } else {
        lexer.push_token(tok);
        break;
      }
    }
    return node;
  }
  return expr_t::ptr_op_t();
}

expr_t::ptr_op_t
query_t::parser_t::parse_query_expr(lexer_t::token_t::kind_t tok_context,
                                    bool                     subexpression)
{
  expr_t::ptr_op_t limiter;

  while (expr_t::ptr_op_t next = parse_or_expr(tok_context)) {
    if (! limiter) {
      limiter = next;
    } else {
      expr_t::ptr_op_t prev(limiter);
      limiter = new expr_t::op_t(expr_t::op_t::O_OR);
      limiter->set_left(prev);
      limiter->set_right(next);
    }
  }

  if (! subexpression) {
    if (limiter)
      query_map.insert
        (query_map_t::value_type
         (QUERY_LIMIT, predicate_t(limiter, what_to_keep).print_to_str()));

    lexer_t::token_t tok = lexer.peek_token(tok_context);
    while (tok.kind != lexer_t::token_t::END_REACHED) {
      switch (tok.kind) {
      case lexer_t::token_t::TOK_SHOW:
      case lexer_t::token_t::TOK_ONLY:
      case lexer_t::token_t::TOK_BOLD: {
        lexer.next_token(tok_context);

        kind_t kind;
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
          if (! node) {
            node = next;
          } else {
            expr_t::ptr_op_t prev(node);
            node = new expr_t::op_t(expr_t::op_t::O_OR);
            node->set_left(prev);
            node->set_right(next);
          }
        }

        if (node)
          query_map.insert
            (query_map_t::value_type
             (kind, predicate_t(node, what_to_keep).print_to_str()));
        break;
      }

      case lexer_t::token_t::TOK_FOR:
      case lexer_t::token_t::TOK_SINCE:
      case lexer_t::token_t::TOK_UNTIL: {
        tok = lexer.next_token(tok_context);

        string                 for_string;

        if (tok.kind == lexer_t::token_t::TOK_SINCE)
          for_string = "since";
        else if (tok.kind == lexer_t::token_t::TOK_UNTIL)
          for_string = "until";

        lexer.consume_next_arg = true;
        tok = lexer.peek_token(tok_context);

        while (tok.kind != lexer_t::token_t::END_REACHED) {
          tok = lexer.next_token(tok_context);
          assert(tok.kind == lexer_t::token_t::TERM);

          if (*tok.value == "show" || *tok.value == "bold" ||
              *tok.value == "for" || *tok.value == "since" ||
              *tok.value == "until") {
            lexer.token_cache = lexer_t::token_t();
            lexer.arg_i = lexer.prev_arg_i;
            lexer.consume_next_arg = false;
            break;
          }

          if (! for_string.empty())
            for_string += " ";
          for_string += *tok.value;

          lexer.consume_next_arg = true;
          tok = lexer.peek_token(tok_context);
        }

        if (! for_string.empty())
          query_map.insert(query_map_t::value_type(QUERY_FOR, for_string));
        break;
      }

      default:
        goto done;
      }

      tok = lexer.peek_token(tok_context);
    }
  done:
    ;
  }

  return limiter;
}

} // namespace ledger
