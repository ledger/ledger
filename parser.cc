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

#include "parser.h"

namespace ledger {

expr_t::ptr_op_t
expr_t::parser_t::parse_value_term(std::istream& in,
				   const flags_t tflags) const
{
  ptr_op_t node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::VALUE:
    node = new op_t(op_t::VALUE);
    node->set_value(tok.value);
    break;

  case token_t::MASK: {
    // A /mask/ is just a shorthand for calling match().
    node = new op_t(op_t::O_CALL);

    ptr_op_t ident = new op_t(op_t::IDENT);
    ident->set_ident("match");
    node->set_left(ident);

    ptr_op_t args = new op_t(op_t::O_COMMA);
    node->set_right(args);

    ptr_op_t mask = new op_t(op_t::MASK);
    mask->set_mask(tok.value.as_string());

    ident = new op_t(op_t::IDENT);

    args->set_left(mask);
    args->set_right(ident);

    switch (tok.flags()) {
    case TOKEN_SHORT_ACCOUNT_MASK:
      ident->set_ident("account_base");
      break;
    case TOKEN_CODE_MASK:
      ident->set_ident("code");
      break;
#if 0
    case TOKEN_COMMODITY_MASK:
      ident->set_ident("commodity");
      break;
#endif
    case TOKEN_PAYEE_MASK:
      ident->set_ident("payee");
      break;
    case TOKEN_NOTE_MASK:
      ident->set_ident("note");
      break;
    case TOKEN_ACCOUNT_MASK:
      ident->set_ident("account");
      break;
    }
    break;
  }

  case token_t::IDENT: {
#if 0
#ifdef USE_BOOST_PYTHON
    if (tok.value->as_string() == "lambda") // special
      try {
	char c, buf[4096];

	std::strcpy(buf, "lambda ");
	READ_INTO(in, &buf[7], 4000, c, true);

	ptr_op_t eval = new op_t(op_t::O_EVAL);
	ptr_op_t lambda = new op_t(op_t::FUNCTION);
	lambda->functor = new python_functor_t(python_eval(buf));
	eval->set_left(lambda);
	ptr_op_t sym = new op_t(op_t::SYMBOL);
	sym->name = new string("__ptr");
	eval->set_right(sym);

	node = eval;

	goto done;
      }
      catch(const boost::python::error_already_set&) {
	throw_(parse_error, "Error parsing lambda expression");
      }
#endif /* USE_BOOST_PYTHON */
#endif

    string ident = tok.value.as_string();

    // An identifier followed by ( represents a function call
    tok = next_token(in, tflags);

    if (tok.kind == token_t::LPAREN) {
      node = new op_t(op_t::IDENT);
      node->set_ident(ident);

      ptr_op_t call_node(new op_t(op_t::O_CALL));
      call_node->set_left(node);
      call_node->set_right(parse_value_expr(in, tflags | EXPR_PARSE_PARTIAL));

      tok = next_token(in, tflags);
      if (tok.kind != token_t::RPAREN)
	tok.expected(')');

      node = call_node;
    } else {
      if (std::isdigit(ident[0])) {
	node = new op_t(op_t::INDEX);
	node->set_index(lexical_cast<unsigned int>(ident.c_str()));
      } else {
	node = new op_t(op_t::IDENT);
	node->set_ident(ident);
      }
      push_token(tok);
    }
    break;
  }

  case token_t::LPAREN:
    node = parse_value_expr(in, tflags | EXPR_PARSE_PARTIAL);
    if (! node)
      throw_(parse_error, tok.symbol << " operator not followed by expression");

    tok = next_token(in, tflags);
    if (tok.kind != token_t::RPAREN)
      tok.expected(')');
    break;

  default:
    push_token(tok);
    break;
  }

#if 0
#ifdef USE_BOOST_PYTHON
 done:
#endif
#endif
  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_unary_expr(std::istream& in,
				   const flags_t tflags) const
{
  ptr_op_t node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::EXCLAM: {
    ptr_op_t term(parse_value_term(in, tflags));
    if (! term)
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");

    // A very quick optimization
    if (term->kind == op_t::VALUE) {
      term->as_value_lval().in_place_negate();
      node = term;
    } else {
      node = new op_t(op_t::O_NOT);
      node->set_left(term);
    }
    break;
  }

  case token_t::MINUS: {
    ptr_op_t term(parse_value_term(in, tflags));
    if (! term)
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");

    // A very quick optimization
    if (term->kind == op_t::VALUE) {
      term->as_value_lval().in_place_negate();
      node = term;
    } else {
      node = new op_t(op_t::O_NEG);
      node->set_left(term);
    }
    break;
  }

  default:
    push_token(tok);
    node = parse_value_term(in, tflags);
    break;
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_mul_expr(std::istream& in,
				 const flags_t tflags) const
{
  ptr_op_t node(parse_unary_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);

    if (tok.kind == token_t::STAR || tok.kind == token_t::KW_DIV) {
      ptr_op_t prev(node);
      node = new op_t(tok.kind == token_t::STAR ?
		      op_t::O_MUL : op_t::O_DIV);
      node->set_left(prev);
      node->set_right(parse_mul_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");

      tok = next_token(in, tflags);
    }
    push_token(tok);
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_add_expr(std::istream& in,
				 const flags_t tflags) const
{
  ptr_op_t node(parse_mul_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);

    if (tok.kind == token_t::PLUS ||
	tok.kind == token_t::MINUS) {
      ptr_op_t prev(node);
      node = new op_t(tok.kind == token_t::PLUS ?
		      op_t::O_ADD : op_t::O_SUB);
      node->set_left(prev);
      node->set_right(parse_add_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");

      tok = next_token(in, tflags);
    }
    push_token(tok);
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_logic_expr(std::istream& in,
				   const flags_t tflags) const
{
  ptr_op_t node(parse_add_expr(in, tflags));

  if (node) {
    op_t::kind_t kind	= op_t::LAST;
    flags_t	 _flags = tflags;
    token_t&	 tok	= next_token(in, tflags);

    switch (tok.kind) {
    case token_t::EQUAL:
      if (tflags & EXPR_PARSE_NO_ASSIGN)
	tok.rewind(in);
      else
	kind = op_t::O_EQ;
      break;
    case token_t::NEQUAL:
      kind = op_t::O_NEQ;
      break;
    case token_t::LESS:
      kind = op_t::O_LT;
      break;
    case token_t::LESSEQ:
      kind = op_t::O_LTE;
      break;
    case token_t::GREATER:
      kind = op_t::O_GT;
      break;
    case token_t::GREATEREQ:
      kind = op_t::O_GTE;
      break;
    default:
      push_token(tok);
      break;
    }

    if (kind != op_t::LAST) {
      ptr_op_t prev(node);
      node = new op_t(kind);
      node->set_left(prev);
      node->set_right(parse_add_expr(in, _flags));

      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    }
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_and_expr(std::istream& in,
				 const flags_t tflags) const
{
  ptr_op_t node(parse_logic_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);

    if (tok.kind == token_t::KW_AND) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_AND);
      node->set_left(prev);
      node->set_right(parse_and_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_or_expr(std::istream& in,
				const flags_t tflags) const
{
  ptr_op_t node(parse_and_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);

    if (tok.kind == token_t::KW_OR) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_OR);
      node->set_left(prev);
      node->set_right(parse_or_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_querycolon_expr(std::istream& in,
					const flags_t tflags) const
{
  ptr_op_t node(parse_or_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);

    if (tok.kind == token_t::QUERY) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_AND);
      node->set_left(prev);
      node->set_right(parse_or_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");

      token_t& next_tok = next_token(in, tflags);
      if (next_tok.kind != token_t::COLON)
	next_tok.expected(':');

      prev = node;
      node = new op_t(op_t::O_OR);
      node->set_left(prev);
      node->set_right(parse_or_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_value_expr(std::istream& in,
				   const flags_t tflags) const
{
  ptr_op_t node(parse_querycolon_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);

    if (tok.kind == token_t::COMMA) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_COMMA);
      node->set_left(prev);
      node->set_right(parse_value_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
      tok = next_token(in, tflags);
    }

    if (tok.kind != token_t::TOK_EOF) {
      if (tflags & EXPR_PARSE_PARTIAL)
	push_token(tok);
      else
	tok.unexpected();
    }
  }
  else if (! (tflags & EXPR_PARSE_PARTIAL)) {
    throw_(parse_error, "Failed to parse value expression");
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse(std::istream& in, const flags_t flags)
{
  try {
    ptr_op_t top_node = parse_value_expr(in, flags);

    if (use_lookahead) {
      use_lookahead = false;
      lookahead.rewind(in);
    }
    lookahead.clear();

    return top_node;
  }
  catch (const std::exception& err) {
    add_error_context("While parsing value expression:\n");
#if 0
    add_error_context(line_context(str, (long)in.tellg() - 1));
#endif
    throw err;
  }
}

} // namespace ledger
