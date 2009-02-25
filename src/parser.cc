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

#include "parser.h"

namespace ledger {

expr_t::ptr_op_t
expr_t::parser_t::parse_value_term(std::istream&        in,
				   const parse_flags_t& tflags) const
{
  ptr_op_t node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::VALUE:
    node = new op_t(op_t::VALUE);
    node->set_value(tok.value);
    break;

  case token_t::IDENT: {
    string ident = tok.value.as_string();

    node = new op_t(op_t::IDENT);
    node->set_ident(ident);

    // An identifier followed by ( represents a function call
    tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
    if (tok.kind == token_t::LPAREN) {
      ptr_op_t call_node(new op_t(op_t::O_CALL));
      call_node->set_left(node);
      node = call_node;

      push_token(tok);		// let the parser see it again
      node->set_right(parse_value_expr(in, tflags.plus_flags(PARSE_SINGLE)));
    } else {
      push_token(tok);
    }
    break;
  }

  case token_t::LPAREN:
    node = parse_value_expr(in, tflags.plus_flags(PARSE_PARTIAL)
			    .minus_flags(PARSE_SINGLE));
    tok = next_token(in, tflags);
    if (tok.kind != token_t::RPAREN)
      tok.expected(')');
    break;

  default:
    push_token(tok);
    break;
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_dot_expr(std::istream& in,
				 const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_value_term(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      if (tok.kind == token_t::DOT) {
	ptr_op_t prev(node);
	node = new op_t(op_t::O_LOOKUP);
	node->set_left(prev);
	node->set_right(parse_value_term(in, tflags));
	if (! node->right())
	  throw_(parse_error,
		 _("%1 operator not followed by argument") << tok.symbol);
      } else {
	push_token(tok);
	break;
      }
    }
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_unary_expr(std::istream& in,
				   const parse_flags_t& tflags) const
{
  ptr_op_t node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::EXCLAM: {
    ptr_op_t term(parse_dot_expr(in, tflags));
    if (! term)
      throw_(parse_error,
	     _("%1 operator not followed by argument") << tok.symbol);

    // A very quick optimization
    if (term->kind == op_t::VALUE) {
      term->as_value_lval().in_place_not();
      node = term;
    } else {
      node = new op_t(op_t::O_NOT);
      node->set_left(term);
    }
    break;
  }

  case token_t::MINUS: {
    ptr_op_t term(parse_dot_expr(in, tflags));
    if (! term)
      throw_(parse_error,
	     _("%1 operator not followed by argument") << tok.symbol);

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
    node = parse_dot_expr(in, tflags);
    break;
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_mul_expr(std::istream& in,
				 const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_unary_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

      if (tok.kind == token_t::STAR || tok.kind == token_t::SLASH ||
	  tok.kind == token_t::KW_DIV) {
	ptr_op_t prev(node);
	node = new op_t(tok.kind == token_t::STAR ?
			op_t::O_MUL : op_t::O_DIV);
	node->set_left(prev);
	node->set_right(parse_unary_expr(in, tflags));
	if (! node->right())
	  throw_(parse_error,
		 _("%1 operator not followed by argument") << tok.symbol);
      } else {
	push_token(tok);
	break;
      }
    }
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_add_expr(std::istream& in,
				 const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_mul_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

      if (tok.kind == token_t::PLUS ||
	  tok.kind == token_t::MINUS) {
	ptr_op_t prev(node);
	node = new op_t(tok.kind == token_t::PLUS ?
			op_t::O_ADD : op_t::O_SUB);
	node->set_left(prev);
	node->set_right(parse_mul_expr(in, tflags));
	if (! node->right())
	  throw_(parse_error,
		 _("%1 operator not followed by argument") << tok.symbol);
      } else {
	push_token(tok);
	break;
      }
    }
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_logic_expr(std::istream& in,
				   const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_add_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    while (true) {
      op_t::kind_t  kind	 = op_t::LAST;
      parse_flags_t _flags = tflags;
      token_t&	  tok	 = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      bool	  negate = false;

      switch (tok.kind) {
      case token_t::DEFINE:
	kind = op_t::O_DEFINE;
	break;
      case token_t::EQUAL:
	if (tflags.has_flags(PARSE_NO_ASSIGN))
	  tok.rewind(in);
	else
	  kind = op_t::O_EQ;
	break;
      case token_t::NEQUAL:
	kind = op_t::O_EQ;
	negate = true;
	break;
      case token_t::MATCH:
	kind = op_t::O_MATCH;
	break;
      case token_t::NMATCH:
	kind = op_t::O_MATCH;
	negate = true;
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
	goto exit_loop;
      }

      if (kind != op_t::LAST) {
	ptr_op_t prev(node);
	node = new op_t(kind);
	node->set_left(prev);
	node->set_right(parse_add_expr(in, _flags));

	if (! node->right())
	  throw_(parse_error,
		 _("%1 operator not followed by argument") << tok.symbol);

	if (negate) {
	  prev = node;
	  node = new op_t(op_t::O_NOT);
	  node->set_left(prev);
	}
      }
    }
  }

 exit_loop:
  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_and_expr(std::istream& in,
				 const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_logic_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

      if (tok.kind == token_t::KW_AND) {
	ptr_op_t prev(node);
	node = new op_t(op_t::O_AND);
	node->set_left(prev);
	node->set_right(parse_logic_expr(in, tflags));
	if (! node->right())
	  throw_(parse_error,
		 _("%1 operator not followed by argument") << tok.symbol);
      } else {
	push_token(tok);
	break;
      }
    }
  }
  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_or_expr(std::istream& in,
				const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_and_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

      if (tok.kind == token_t::KW_OR) {
	ptr_op_t prev(node);
	node = new op_t(op_t::O_OR);
	node->set_left(prev);
	node->set_right(parse_and_expr(in, tflags));
	if (! node->right())
	  throw_(parse_error,
		 _("%1 operator not followed by argument") << tok.symbol);
      } else {
	push_token(tok);
	break;
      }
    }
  }
  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_querycolon_expr(std::istream& in,
					const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_or_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

    if (tok.kind == token_t::QUERY) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_QUERY);
      node->set_left(prev);
      node->set_right(parse_or_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       _("%1 operator not followed by argument") << tok.symbol);

      token_t& next_tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      if (next_tok.kind != token_t::COLON)
	next_tok.expected(':');

      prev = node->right();
      ptr_op_t subnode = new op_t(op_t::O_COLON);
      subnode->set_left(prev);
      subnode->set_right(parse_or_expr(in, tflags));
      if (! subnode->right())
	throw_(parse_error,
	       _("%1 operator not followed by argument") << tok.symbol);

      node->set_right(subnode);
    } else {
      push_token(tok);
    }
  }
  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_value_expr(std::istream& in,
				   const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_querycolon_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

    if (tok.kind == token_t::COMMA) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_CONS);
      node->set_left(prev);
      node->set_right(parse_value_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       _("%1 operator not followed by argument") << tok.symbol);
      tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
    }

    if (tok.kind != token_t::TOK_EOF) {
      if (tflags.has_flags(PARSE_PARTIAL))
	push_token(tok);
      else
	tok.unexpected();
    }
  }
  else if (! tflags.has_flags(PARSE_PARTIAL) &&
	   ! tflags.has_flags(PARSE_SINGLE)) {
    throw_(parse_error, _("Failed to parse value expression"));
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse(std::istream& in, const parse_flags_t& flags,
			const string * original_string)
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
    if (original_string) {
      add_error_context(_("While parsing value expression:"));

      istream_pos_type end_pos = in.tellg();
      istream_pos_type pos     = end_pos;

      pos -= lookahead.length;

      DEBUG("parser.error", "original_string = '" << *original_string << "'");
      DEBUG("parser.error", "            pos = " << pos);
      DEBUG("parser.error", "        end_pos = " << end_pos);
      DEBUG("parser.error", "     token kind = " << int(lookahead.kind));
      DEBUG("parser.error", "   token length = " << lookahead.length);

      add_error_context(line_context(*original_string, pos, end_pos));
    }
    throw;
  }
}

} // namespace ledger
