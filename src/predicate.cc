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

#include <system.hh>

#include "predicate.h"
#include "op.h"

namespace ledger {

query_lexer_t::token_t query_lexer_t::next_token()
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
  bool consume_next = false;
  switch (*arg_i) {
  case ' ':
  case '\t':
  case '\r':
  case '\n':
    if (++arg_i == arg_end)
      return next_token();
  goto resume;

  case '/': {
    string pat;
    bool found_end_slash = false;
    for (++arg_i; arg_i != arg_end; ++arg_i) {
      if (*arg_i == '\\') {
	if (++arg_i == arg_end)
	  throw_(parse_error, _("Unexpected '\\' at end of pattern"));
      }
      else if (*arg_i == '/') {
	++arg_i;
	found_end_slash = true;
	break;
      }
      pat.push_back(*arg_i);
    }
    if (! found_end_slash)
      throw_(parse_error, _("Expected '/' at end of pattern"));
    if (pat.empty())
      throw_(parse_error, _("Match pattern is empty"));

    return token_t(token_t::TERM, pat);
  }

  case '(': ++arg_i; return token_t(token_t::LPAREN);
  case ')': ++arg_i; return token_t(token_t::RPAREN);
  case '&': ++arg_i; return token_t(token_t::TOK_AND);
  case '|': ++arg_i; return token_t(token_t::TOK_OR);
  case '!': ++arg_i; return token_t(token_t::TOK_NOT);
  case '@': ++arg_i; return token_t(token_t::TOK_PAYEE);
  case '#': ++arg_i; return token_t(token_t::TOK_CODE);
  case '%': ++arg_i; return token_t(token_t::TOK_META);
  case '=':
    // The '=' keyword at the beginning of a string causes the entire string
    // to be taken as an expression.
    if (arg_i == (*begin).as_string().begin())
      consume_whitespace = true;
    ++arg_i;
    return token_t(token_t::TOK_EQ);

  case '\\':
    consume_next = true;
    ++arg_i;
    // fall through...
  default: {
    string ident;
    string::const_iterator beg = arg_i;
    for (; arg_i != arg_end; ++arg_i) {
      switch (*arg_i) {
      case ' ':
      case '\t':
      case '\n':
      case '\r':
	if (! consume_whitespace)
	  goto test_ident;
	else
	  ident.push_back(*arg_i);
	break;
      case '(':
      case ')':
      case '&':
      case '|':
      case '!':
      case '@':
      case '#':
      case '%':
      case '=':
	if (! consume_next)
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
    else if (ident == "account")
      return token_t(token_t::TOK_ACCOUNT);
    else if (ident == "tag")
      return token_t(token_t::TOK_META);
    else if (ident == "meta")
      return token_t(token_t::TOK_META);
    else if (ident == "data")
      return token_t(token_t::TOK_META);
    else if (ident == "show") {
      // The "show" keyword is special, and separates a limiting predicate
      // from a display predicate.
      DEBUG("pred.show", "string = " << (*begin).as_string());
      return token_t(token_t::END_REACHED);
    }
#if 0
    // jww (2009-11-06): This is disabled for the time being.
    else if (ident == "date") {
      // The date keyword takes the whole of the next string as its argument.
      consume_whitespace = true;
      return token_t(token_t::TOK_DATE);
    }
#endif
    else if (ident == "expr") {
      // The expr keyword takes the whole of the next string as its argument.
      consume_whitespace = true;
      return token_t(token_t::TOK_EXPR);
    }
    else
      return token_t(token_t::TERM, ident);
    break;
  }
  }

  return token_t(token_t::UNKNOWN);
}

void query_lexer_t::token_t::unexpected()
{
  kind_t prev_kind = kind;

  kind = UNKNOWN;

  switch (prev_kind) {
  case END_REACHED:
    throw_(parse_error, _("Unexpected end of expression"));
  case TERM:
    throw_(parse_error, _("Unexpected string '%1'") << *value);
  default:
    throw_(parse_error, _("Unexpected token '%1'") << symbol());
  }
}

void query_lexer_t::token_t::expected(char wanted, char c)
{
  kind = UNKNOWN;

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

expr_t::ptr_op_t
query_parser_t::parse_query_term(query_lexer_t::token_t::kind_t tok_context)
{
  expr_t::ptr_op_t node;

  query_lexer_t::token_t tok = lexer.next_token();
  switch (tok.kind) {
  case query_lexer_t::token_t::END_REACHED:
    break;

  case query_lexer_t::token_t::TOK_DATE:
  case query_lexer_t::token_t::TOK_CODE:
  case query_lexer_t::token_t::TOK_PAYEE:
  case query_lexer_t::token_t::TOK_NOTE:
  case query_lexer_t::token_t::TOK_ACCOUNT:
  case query_lexer_t::token_t::TOK_META:
  case query_lexer_t::token_t::TOK_EXPR:
    node = parse_query_term(tok.kind);
    if (! node)
      throw_(parse_error,
	     _("%1 operator not followed by argument") << tok.symbol());
    break;

  case query_lexer_t::token_t::TERM:
    assert(tok.value);
    switch (tok_context) {
    case query_lexer_t::token_t::TOK_DATE: {
      expr_t::ptr_op_t ident = new expr_t::op_t(expr_t::op_t::IDENT);
      ident->set_ident("date");

      date_interval_t interval(*tok.value);

      if (interval.start) {
	node = new expr_t::op_t(expr_t::op_t::O_GTE);
	node->set_left(ident);

	expr_t::ptr_op_t arg1 = new expr_t::op_t(expr_t::op_t::VALUE);
	arg1->set_value(*interval.start);
	node->set_right(arg1);
      }

      if (interval.end) {
	expr_t::ptr_op_t lt = new expr_t::op_t(expr_t::op_t::O_LT);
	lt->set_left(ident);

	expr_t::ptr_op_t arg1 = new expr_t::op_t(expr_t::op_t::VALUE);
	arg1->set_value(*interval.end);
	lt->set_right(arg1);

	if (node) {
	  expr_t::ptr_op_t prev(node);
	  node = new expr_t::op_t(expr_t::op_t::O_AND);
	  node->set_left(prev);
	  node->set_right(lt);
	} else {
	  node = lt;
	}
      }
      break;
    }

    case query_lexer_t::token_t::TOK_EXPR:
      node = expr_t(*tok.value).get_op();
      break;

    case query_lexer_t::token_t::TOK_META: {
      node = new expr_t::op_t(expr_t::op_t::O_CALL);

      expr_t::ptr_op_t ident = new expr_t::op_t(expr_t::op_t::IDENT);
      ident->set_ident("has_tag");
      node->set_left(ident);

      expr_t::ptr_op_t arg1 = new expr_t::op_t(expr_t::op_t::VALUE);
      arg1->set_value(mask_t(*tok.value));

      tok = lexer.peek_token();
      if (tok.kind == query_lexer_t::token_t::TOK_EQ) {
	tok = lexer.next_token();
	tok = lexer.next_token();
	if (tok.kind != query_lexer_t::token_t::TERM)
	  throw_(parse_error,
		 _("Metadata equality operator not followed by term"));
	
	expr_t::ptr_op_t cons = new expr_t::op_t(expr_t::op_t::O_CONS);

	expr_t::ptr_op_t arg2 = new expr_t::op_t(expr_t::op_t::VALUE);
	assert(tok.value);
	arg2->set_value(mask_t(*tok.value));

	cons->set_left(arg1);
	cons->set_right(arg2);
	node->set_right(cons);
      } else {
	node->set_right(arg1);
      }
      break;
    }
      
    default: {
      node = new expr_t::op_t(expr_t::op_t::O_MATCH);

      expr_t::ptr_op_t ident = new expr_t::op_t(expr_t::op_t::IDENT);
      switch (tok_context) {
      case query_lexer_t::token_t::TOK_ACCOUNT:
	ident->set_ident("account"); break;
      case query_lexer_t::token_t::TOK_PAYEE:
	ident->set_ident("payee"); break;
      case query_lexer_t::token_t::TOK_CODE:
	ident->set_ident("code"); break;
      case query_lexer_t::token_t::TOK_NOTE:
	ident->set_ident("note"); break;
      default:
	assert(0); break;
      }

      expr_t::ptr_op_t mask = new expr_t::op_t(expr_t::op_t::VALUE);
      mask->set_value(mask_t(*tok.value));

      node->set_left(ident);
      node->set_right(mask);
    }
    }
    break;

  case query_lexer_t::token_t::LPAREN:
    node = parse_query_expr(tok_context);
    tok = lexer.next_token();
    if (tok.kind != query_lexer_t::token_t::RPAREN)
      tok.expected(')');
    break;

  default:
    lexer.push_token(tok);
    break;
  }

  return node;
}

expr_t::ptr_op_t
query_parser_t::parse_unary_expr(query_lexer_t::token_t::kind_t tok_context)
{
  expr_t::ptr_op_t node;

  query_lexer_t::token_t tok = lexer.next_token();
  switch (tok.kind) {
  case query_lexer_t::token_t::TOK_NOT: {
    expr_t::ptr_op_t term(parse_query_term(tok_context));
    if (! term)
      throw_(parse_error,
	     _("%1 operator not followed by argument") << tok.symbol());

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
query_parser_t::parse_and_expr(query_lexer_t::token_t::kind_t tok_context)
{
  if (expr_t::ptr_op_t node = parse_unary_expr(tok_context)) {
    while (true) {
      query_lexer_t::token_t tok = lexer.next_token();
      if (tok.kind == query_lexer_t::token_t::TOK_AND) {
	expr_t::ptr_op_t prev(node);
	node = new expr_t::op_t(expr_t::op_t::O_AND);
	node->set_left(prev);
	node->set_right(parse_unary_expr(tok_context));
	if (! node->right())
	  throw_(parse_error,
		 _("%1 operator not followed by argument") << tok.symbol());
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
query_parser_t::parse_or_expr(query_lexer_t::token_t::kind_t tok_context)
{
  if (expr_t::ptr_op_t node = parse_and_expr(tok_context)) {
    while (true) {
      query_lexer_t::token_t tok = lexer.next_token();
      if (tok.kind == query_lexer_t::token_t::TOK_OR) {
	expr_t::ptr_op_t prev(node);
	node = new expr_t::op_t(expr_t::op_t::O_OR);
	node->set_left(prev);
	node->set_right(parse_and_expr(tok_context));
	if (! node->right())
	  throw_(parse_error,
		 _("%1 operator not followed by argument") << tok.symbol());
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
query_parser_t::parse_query_expr(query_lexer_t::token_t::kind_t tok_context)
{
  if (expr_t::ptr_op_t node = parse_or_expr(tok_context)) {
    if (expr_t::ptr_op_t next = parse_query_expr(tok_context)) {
      expr_t::ptr_op_t prev(node);
      node = new expr_t::op_t(expr_t::op_t::O_OR);
      node->set_left(prev);
      node->set_right(next);
    }
    return node;
  }
  return expr_t::ptr_op_t();
}

expr_t::ptr_op_t query_parser_t::parse()
{
  return parse_query_expr(query_lexer_t::token_t::TOK_ACCOUNT);
}

std::pair<expr_t, query_parser_t>
args_to_predicate(value_t::sequence_t::const_iterator begin,
		  value_t::sequence_t::const_iterator end)
{
  query_parser_t parser(begin, end);
  expr_t expr(parser.parse());
  return std::pair<expr_t, query_parser_t>(expr, parser);
}

std::pair<expr_t, query_parser_t> args_to_predicate(query_parser_t parser)
{
  expr_t expr(parser.parse());
  return std::pair<expr_t, query_parser_t>(expr, parser);
}

} // namespace ledger
