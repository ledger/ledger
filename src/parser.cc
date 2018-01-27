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
    break;
  }

  case token_t::LPAREN:
    node = parse_value_expr(in, tflags.plus_flags(PARSE_PARTIAL)
                            .minus_flags(PARSE_SINGLE));
    tok = next_token(in, tflags, token_t::RPAREN);
    break;

  default:
    push_token(tok);
    break;
  }

  return node;
}


expr_t::ptr_op_t
expr_t::parser_t::parse_call_expr(std::istream& in,
                                 const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_value_term(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      if (tok.kind == token_t::LPAREN) {
        ptr_op_t prev(node);
        node = new op_t(op_t::O_CALL);
        node->set_left(prev);
        push_token(tok);        // let the parser see the '(' again
        node->set_right(parse_value_expr(in, tflags.plus_flags(PARSE_SINGLE)));
      } else {
        push_token(tok);
        break;
      }
    }
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_dot_expr(std::istream& in,
                                 const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_call_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      if (tok.kind == token_t::DOT) {
        ptr_op_t prev(node);
        node = new op_t(op_t::O_LOOKUP);
        node->set_left(prev);
        node->set_right(parse_call_expr(in, tflags));
        if (! node->right())
          throw_(parse_error,
                 _f("%1% operator not followed by argument") % tok.symbol);
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
             _f("%1% operator not followed by argument") % tok.symbol);

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
             _f("%1% operator not followed by argument") % tok.symbol);

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
                 _f("%1% operator not followed by argument") % tok.symbol);
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
                 _f("%1% operator not followed by argument") % tok.symbol);
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
      op_t::kind_t  kind   = op_t::LAST;
      parse_flags_t _flags = tflags;
      token_t&      tok    = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      bool          negate = false;

      switch (tok.kind) {
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
                 _f("%1% operator not followed by argument") % tok.symbol);

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
                 _f("%1% operator not followed by argument") % tok.symbol);
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
                 _f("%1% operator not followed by argument") % tok.symbol);
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
               _f("%1% operator not followed by argument") % tok.symbol);

      next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT), token_t::COLON);
      prev = node->right();
      ptr_op_t subnode = new op_t(op_t::O_COLON);
      subnode->set_left(prev);
      subnode->set_right(parse_or_expr(in, tflags));
      if (! subnode->right())
        throw_(parse_error,
               _f("%1% operator not followed by argument") % tok.symbol);

      node->set_right(subnode);
    }
    else if (tok.kind == token_t::KW_IF) {
      ptr_op_t if_op(parse_or_expr(in, tflags));
      if (! if_op)
        throw_(parse_error, _("'if' keyword not followed by argument"));

      tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      if (tok.kind == token_t::KW_ELSE) {
        ptr_op_t else_op(parse_or_expr(in, tflags));
        if (! else_op)
          throw_(parse_error, _("'else' keyword not followed by argument"));

        ptr_op_t subnode = new op_t(op_t::O_COLON);
        subnode->set_left(node);
        subnode->set_right(else_op);

        node = new op_t(op_t::O_QUERY);
        node->set_left(if_op);
        node->set_right(subnode);
      } else {
        ptr_op_t null_node = new op_t(op_t::VALUE);
        null_node->set_value(NULL_VALUE);

        ptr_op_t subnode = new op_t(op_t::O_COLON);
        subnode->set_left(node);
        subnode->set_right(null_node);

        node = new op_t(op_t::O_QUERY);
        node->set_left(if_op);
        node->set_right(subnode);

        push_token(tok);
      }
    }
    else {
      push_token(tok);
    }
  }
  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_comma_expr(std::istream& in,
                                   const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_querycolon_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    ptr_op_t next;
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

      if (tok.kind == token_t::COMMA) {
        if (! next) {
          ptr_op_t prev(node);
          node = new op_t(op_t::O_CONS);
          node->set_left(prev);
          next = node;
        }

        token_t& ntok = next_token(in, tflags);
        push_token(ntok);
        if (ntok.kind == token_t::RPAREN)
          break;

        ptr_op_t chain(new op_t(op_t::O_CONS));
        chain->set_left(parse_querycolon_expr(in, tflags));

        next->set_right(chain);
        next = chain;
      } else {
        push_token(tok);
        break;
      }
    }
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_lambda_expr(std::istream& in,
                                    const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_comma_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

    if (tok.kind == token_t::ARROW) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_LAMBDA);
      node->set_left(prev);
      ptr_op_t scope(new op_t(op_t::SCOPE));
      scope->set_left(parse_querycolon_expr(in, tflags));
      node->set_right(scope);
    } else {
      push_token(tok);
    }
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse_assign_expr(std::istream& in,
                                    const parse_flags_t& tflags) const
{
  ptr_op_t node(parse_lambda_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

    if (tok.kind == token_t::ASSIGN) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_DEFINE);
      node->set_left(prev);
      ptr_op_t scope(new op_t(op_t::SCOPE));
      scope->set_left(parse_lambda_expr(in, tflags));
      node->set_right(scope);
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
  ptr_op_t node(parse_assign_expr(in, tflags));

  if (node && ! tflags.has_flags(PARSE_SINGLE)) {
    ptr_op_t chain;
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      if (tok.kind == token_t::SEMI) {
        ptr_op_t seq(new op_t(op_t::O_SEQ));
        if (! chain) {
          seq->set_left(node);
          node = seq;
        } else {
          seq->set_left(chain->right());
          chain->set_right(seq);
        }
        seq->set_right(parse_assign_expr(in, tflags));
        chain = seq;
      } else {
        push_token(tok);
        break;
      }
    }
  }

  return node;
}

expr_t::ptr_op_t
expr_t::parser_t::parse(std::istream&           in,
                        const parse_flags_t&    flags,
                        const optional<string>& original_string)
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
  catch (const std::exception&) {
    if (original_string) {
      add_error_context(_("While parsing value expression:"));

      std::streamoff end_pos = 0;
      if (in.good())
        end_pos = in.tellg();
      std::streamoff pos = end_pos;

      if (pos > 0)
        pos -= lookahead.length;

      DEBUG("parser.error", "original_string = '" << *original_string << "'");
      DEBUG("parser.error", "            pos = " << pos);
      DEBUG("parser.error", "        end_pos = " << end_pos);
      DEBUG("parser.error", "     token kind = " << int(lookahead.kind));
      DEBUG("parser.error", "   token length = " << lookahead.length);

      add_error_context(line_context(*original_string,
                                     static_cast<string::size_type>(pos),
                                     static_cast<string::size_type>(end_pos)));
    }
    throw;
  }
}

} // namespace ledger
