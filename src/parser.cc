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
 * @file   parser.cc
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Implementation of the recursive-descent expression parser.
 *
 * This file implements the recursive-descent parser declared in
 * parser.h.  Each `parse_*` function handles one precedence level of
 * Ledger's expression grammar; its operands are parsed at tighter levels,
 * except inside parentheses, which restart at parse_value_expr.
 *
 * The general pattern for binary operator levels is:
 *   1. Call the next-higher-precedence function to get the left operand.
 *   2. Loop (parse_binary()): read a token; if it is an operator at this
 *      level, create an op_t node, set the left operand, parse the right
 *      operand by calling the higher-precedence function again, and continue
 *      the loop.  Each binary level passes its operand parser and its
 *      token-to-op map to parse_binary().
 *   3. If the token is not at this level, push it back and return.
 *
 * Constant folding is performed eagerly for unary `!` and `-` when the
 * operand is a literal VALUE node, avoiding runtime AST walks for simple
 * cases like `!true` or `-100`.
 *
 * The ternary operator (`? :`) and postfix conditional (`expr if cond else alt`)
 * are both normalized into the same O_QUERY / O_COLON AST shape, so the
 * evaluator only needs one code path for conditionals.
 */

#include <system.hh>

#include "parser.h"
#include "scope.h"

namespace ledger {

/*--- Primary Value Terms ---*/

int expr_t::parser_t::checked_depth(int depth) const {
  if (depth > MAX_EXPR_DEPTH)
    throw_(parse_error, _("Expression tree depth limit exceeded"));
  return depth;
}

expr_t::ptr_op_t expr_t::parser_t::parse_value_term(std::istream& in,
                                                    const parse_flags_t& tflags) const {
  ptr_op_t node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::VALUE:
    node = new op_t(op_t::VALUE);
    node->set_value(tok.value);
    subtree_depth = 0;
    break;

  case token_t::IDENT: {
    string ident = tok.value.as_string();

    node = new op_t(op_t::IDENT);
    node->set_ident(ident);
    subtree_depth = 0;
    break;
  }

  case token_t::LPAREN:
    if (++parse_depth > MAX_PARSE_DEPTH)
      throw_(parse_error, _("Expression nesting depth limit exceeded"));
    node = parse_value_expr(in, tflags.minus_flags(PARSE_SINGLE));
    --parse_depth;
    next_token(in, tflags, token_t::RPAREN);
    break;

  default:
    push_token(tok);
    subtree_depth = 0;
    break;
  }

  return node;
}

/*--- Function Call and Member Access ---*/

expr_t::ptr_op_t expr_t::parser_t::parse_call_expr(std::istream& in,
                                                   const parse_flags_t& tflags) const {
  ptr_op_t node(parse_value_term(in, tflags));

  if (node && !tflags.has_flags(PARSE_SINGLE)) {
    int d = subtree_depth;
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      if (tok.kind == token_t::LPAREN) {
        ptr_op_t prev(node);
        node = new op_t(op_t::O_CALL);
        node->set_left(prev);
        push_token(tok); // let the parser see the '(' again
        node->set_right(parse_value_expr(in, tflags.plus_flags(PARSE_SINGLE)));
        d = checked_depth(1 + std::max(d, subtree_depth));
      } else {
        push_token(tok);
        break;
      }
    }
    subtree_depth = d;
  }

  return node;
}

expr_t::ptr_op_t expr_t::parser_t::parse_dot_expr(std::istream& in,
                                                  const parse_flags_t& tflags) const {
  ptr_op_t node(parse_call_expr(in, tflags));

  if (node && !tflags.has_flags(PARSE_SINGLE)) {
    int d = subtree_depth;
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      if (tok.kind == token_t::DOT) {
        ptr_op_t prev(node);
        node = new op_t(op_t::O_LOOKUP);
        node->set_left(prev);
        node->set_right(parse_call_expr(in, tflags));
        if (!node->right())
          throw_(parse_error, _f("%1% operator not followed by argument") % tok.symbol);
        d = checked_depth(1 + std::max(d, subtree_depth));
      } else {
        push_token(tok);
        break;
      }
    }
    subtree_depth = d;
  }

  return node;
}

/*--- Unary Prefix Operators ---*/

expr_t::ptr_op_t expr_t::parser_t::parse_unary_expr(std::istream& in,
                                                    const parse_flags_t& tflags) const {
  ptr_op_t node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::EXCLAM: {
    ptr_op_t term(parse_dot_expr(in, tflags));
    if (!term)
      throw_(parse_error, _f("%1% operator not followed by argument") % tok.symbol);

    // A very quick optimization
    if (term->kind == op_t::VALUE) {
      term->as_value_lval().in_place_not();
      node = term;
    } else {
      node = new op_t(op_t::O_NOT);
      node->set_left(term);
      subtree_depth = checked_depth(subtree_depth + 1);
    }
    break;
  }

  case token_t::MINUS: {
    ptr_op_t term(parse_dot_expr(in, tflags));
    if (!term)
      throw_(parse_error, _f("%1% operator not followed by argument") % tok.symbol);

    // A very quick optimization
    if (term->kind == op_t::VALUE) {
      term->as_value_lval().in_place_negate();
      node = term;
    } else {
      node = new op_t(op_t::O_NEG);
      node->set_left(term);
      subtree_depth = checked_depth(subtree_depth + 1);
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

/*--- Binary Operators ---*/

namespace {

// Each map returns the op_t kind its level builds for a token, or
// op_t::LAST when the token ends the level.

expr_t::op_t::kind_t mul_kind(expr_t::token_t::kind_t tok, bool& /*negate*/) {
  switch (tok) {
  case expr_t::token_t::STAR:
    return expr_t::op_t::O_MUL;
  case expr_t::token_t::SLASH:
  case expr_t::token_t::KW_DIV:
    return expr_t::op_t::O_DIV;
  default:
    return expr_t::op_t::LAST;
  }
}

expr_t::op_t::kind_t add_kind(expr_t::token_t::kind_t tok, bool& /*negate*/) {
  switch (tok) {
  case expr_t::token_t::PLUS:
    return expr_t::op_t::O_ADD;
  case expr_t::token_t::MINUS:
    return expr_t::op_t::O_SUB;
  default:
    return expr_t::op_t::LAST;
  }
}

expr_t::op_t::kind_t logic_kind(expr_t::token_t::kind_t tok, bool& negate) {
  switch (tok) {
  case expr_t::token_t::EQUAL:
    return expr_t::op_t::O_EQ;
  // != and !~ are desugared as O_NOT(O_EQ(...)) and O_NOT(O_MATCH(...))
  // rather than having their own op_t kinds, reducing evaluator complexity.
  case expr_t::token_t::NEQUAL:
    negate = true;
    return expr_t::op_t::O_EQ;
  case expr_t::token_t::MATCH:
    return expr_t::op_t::O_MATCH;
  case expr_t::token_t::NMATCH:
    negate = true;
    return expr_t::op_t::O_MATCH;
  case expr_t::token_t::EMATCH:
    return expr_t::op_t::O_EMATCH;
  case expr_t::token_t::LESS:
    return expr_t::op_t::O_LT;
  case expr_t::token_t::LESSEQ:
    return expr_t::op_t::O_LTE;
  case expr_t::token_t::GREATER:
    return expr_t::op_t::O_GT;
  case expr_t::token_t::GREATEREQ:
    return expr_t::op_t::O_GTE;
  default:
    return expr_t::op_t::LAST;
  }
}

expr_t::op_t::kind_t and_kind(expr_t::token_t::kind_t tok, bool& /*negate*/) {
  return tok == expr_t::token_t::KW_AND ? expr_t::op_t::O_AND : expr_t::op_t::LAST;
}

expr_t::op_t::kind_t or_kind(expr_t::token_t::kind_t tok, bool& /*negate*/) {
  return tok == expr_t::token_t::KW_OR ? expr_t::op_t::O_OR : expr_t::op_t::LAST;
}

} // namespace

expr_t::ptr_op_t expr_t::parser_t::parse_binary(std::istream& in, const parse_flags_t& tflags,
                                                operand_fn operand, op_kind_fn op_kind) const {
  ptr_op_t node((this->*operand)(in, tflags));

  if (node && !tflags.has_flags(PARSE_SINGLE)) {
    int d = subtree_depth;
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      bool negate = false;
      op_t::kind_t kind = op_kind(tok.kind, negate);
      if (kind == op_t::LAST) {
        push_token(tok);
        break;
      }

      ptr_op_t prev(node);
      node = new op_t(kind);
      node->set_left(prev);
      node->set_right((this->*operand)(in, tflags));
      if (!node->right())
        throw_(parse_error, _f("%1% operator not followed by argument") % tok.symbol);
      d = checked_depth(1 + std::max(d, subtree_depth));

      if (negate) {
        prev = node;
        node = new op_t(op_t::O_NOT);
        node->set_left(prev);
        d = checked_depth(d + 1);
      }
    }
    subtree_depth = d;
  }
  return node;
}

expr_t::ptr_op_t expr_t::parser_t::parse_mul_expr(std::istream& in,
                                                  const parse_flags_t& tflags) const {
  return parse_binary(in, tflags, &parser_t::parse_unary_expr, mul_kind);
}

expr_t::ptr_op_t expr_t::parser_t::parse_add_expr(std::istream& in,
                                                  const parse_flags_t& tflags) const {
  return parse_binary(in, tflags, &parser_t::parse_mul_expr, add_kind);
}

expr_t::ptr_op_t expr_t::parser_t::parse_logic_expr(std::istream& in,
                                                    const parse_flags_t& tflags) const {
  return parse_binary(in, tflags, &parser_t::parse_add_expr, logic_kind);
}

expr_t::ptr_op_t expr_t::parser_t::parse_and_expr(std::istream& in,
                                                  const parse_flags_t& tflags) const {
  return parse_binary(in, tflags, &parser_t::parse_logic_expr, and_kind);
}

expr_t::ptr_op_t expr_t::parser_t::parse_or_expr(std::istream& in,
                                                 const parse_flags_t& tflags) const {
  return parse_binary(in, tflags, &parser_t::parse_and_expr, or_kind);
}

/*--- Ternary and Conditional Expressions ---*/

expr_t::ptr_op_t expr_t::parser_t::parse_querycolon_expr(std::istream& in,
                                                         const parse_flags_t& tflags) const {
  ptr_op_t node(parse_or_expr(in, tflags));

  if (node && !tflags.has_flags(PARSE_SINGLE)) {
    int d = subtree_depth;
    token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

    if (tok.kind == token_t::QUERY) {
      // Traditional ternary: cond ? then_expr : else_expr
      // AST shape: O_QUERY(cond, O_COLON(then_expr, else_expr))
      ptr_op_t prev(node);
      node = new op_t(op_t::O_QUERY);
      node->set_left(prev);
      node->set_right(parse_or_expr(in, tflags));
      if (!node->right())
        throw_(parse_error, _f("%1% operator not followed by argument") % tok.symbol);
      int branches = subtree_depth;

      next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT), token_t::COLON);
      prev = node->right();
      ptr_op_t subnode = new op_t(op_t::O_COLON);
      subnode->set_left(prev);
      subnode->set_right(parse_or_expr(in, tflags));
      if (!subnode->right())
        throw_(parse_error, _f("%1% operator not followed by argument") % tok.symbol);
      branches = std::max(branches, subtree_depth);

      node->set_right(subnode);
      d = checked_depth(1 + std::max(d, 1 + branches));
    } else if (tok.kind == token_t::KW_IF) {
      // Postfix conditional: value_expr if cond [else alt_expr]
      // Normalized to the same AST shape: O_QUERY(cond, O_COLON(value, alt))
      ptr_op_t if_op(parse_or_expr(in, tflags));
      if (!if_op)
        throw_(parse_error, _("'if' keyword not followed by argument"));
      int cond = subtree_depth;
      int alt = 0; // the null VALUE when there is no else

      next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      if (tok.kind == token_t::KW_ELSE) {
        ptr_op_t else_op(parse_or_expr(in, tflags));
        if (!else_op)
          throw_(parse_error, _("'else' keyword not followed by argument"));
        alt = subtree_depth;

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
      d = checked_depth(1 + std::max(cond, 1 + std::max(d, alt)));
    } else {
      push_token(tok);
    }
    subtree_depth = d;
  }
  return node;
}

/*--- Comma-Separated Lists ---*/

expr_t::ptr_op_t expr_t::parser_t::parse_comma_expr(std::istream& in,
                                                    const parse_flags_t& tflags) const {
  ptr_op_t node(parse_querycolon_expr(in, tflags));

  if (node && !tflags.has_flags(PARSE_SINGLE)) {
    // Element i (1-based) hangs under i O_CONS nodes.
    int d = subtree_depth; // first element
    int conses = 0;
    ptr_op_t next;
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

      if (tok.kind == token_t::COMMA) {
        // Build a right-leaning cons list: O_CONS(a, O_CONS(b, O_CONS(c, ...)))
        // The 'next' pointer tracks the tail for efficient appending.
        if (!next) {
          ptr_op_t prev(node);
          node = new op_t(op_t::O_CONS);
          node->set_left(prev);
          next = node;
          conses = 1;
          d = checked_depth(1 + d);
        }

        token_t& ntok = next_token(in, tflags);
        push_token(ntok);
        if (ntok.kind == token_t::RPAREN)
          break;

        ptr_op_t chain(new op_t(op_t::O_CONS));
        chain->set_left(parse_querycolon_expr(in, tflags));
        ++conses;

        next->set_right(chain);
        next = chain;
        d = checked_depth(std::max(d, conses + subtree_depth));
      } else {
        push_token(tok);
        break;
      }
    }
    subtree_depth = d;
  }

  return node;
}

/*--- Lambda Expressions ---*/

expr_t::ptr_op_t expr_t::parser_t::parse_lambda_expr(std::istream& in,
                                                     const parse_flags_t& tflags) const {
  ptr_op_t node(parse_comma_expr(in, tflags));

  if (node && !tflags.has_flags(PARSE_SINGLE)) {
    int d = subtree_depth;
    token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

    if (tok.kind == token_t::ARROW) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_LAMBDA);
      node->set_left(prev);
      ptr_op_t scope(new op_t(op_t::SCOPE));
      scope->set_scope(std::make_shared<symbol_scope_t>(*scope_t::empty_scope));
      scope->set_left(parse_querycolon_expr(in, tflags));
      node->set_right(scope);
      d = checked_depth(1 + std::max(d, 1 + subtree_depth));
    } else {
      push_token(tok);
    }
    subtree_depth = d;
  }

  return node;
}

/*--- Assignment Expressions ---*/

expr_t::ptr_op_t expr_t::parser_t::parse_assign_expr(std::istream& in,
                                                     const parse_flags_t& tflags) const {
  ptr_op_t node(parse_lambda_expr(in, tflags));

  if (node && !tflags.has_flags(PARSE_SINGLE)) {
    int d = subtree_depth;
    token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));

    if (tok.kind == token_t::ASSIGN) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_DEFINE);
      node->set_left(prev);
      ptr_op_t scope(new op_t(op_t::SCOPE));
      scope->set_scope(std::make_shared<symbol_scope_t>(*scope_t::empty_scope));
      scope->set_left(parse_lambda_expr(in, tflags));
      node->set_right(scope);
      d = checked_depth(1 + std::max(d, 1 + subtree_depth));
    } else {
      push_token(tok);
    }
    subtree_depth = d;
  }

  return node;
}

/*--- Statement Sequences ---*/

expr_t::ptr_op_t expr_t::parser_t::parse_value_expr(std::istream& in,
                                                    const parse_flags_t& tflags) const {
  ptr_op_t node(parse_assign_expr(in, tflags));

  if (node && !tflags.has_flags(PARSE_SINGLE)) {
    // The k-th O_SEQ (1-based) holds statements k-1 and k, both at depth k,
    // so the previous statement moves one level down.
    int d = subtree_depth;    // statement 0
    int last = subtree_depth; // depth of the newest statement
    int seqs = 0;
    ptr_op_t chain;
    while (true) {
      token_t& tok = next_token(in, tflags.plus_flags(PARSE_OP_CONTEXT));
      if (tok.kind == token_t::SEMI) {
        ptr_op_t seq(new op_t(op_t::O_SEQ));
        if (!chain) {
          seq->set_left(node);
          node = seq;
        } else {
          seq->set_left(chain->right());
          chain->set_right(seq);
        }
        seq->set_right(parse_assign_expr(in, tflags));
        chain = seq;
        ++seqs;
        d = checked_depth(std::max(d, seqs + std::max(last, subtree_depth)));
        last = subtree_depth;
      } else {
        push_token(tok);
        break;
      }
    }
    subtree_depth = d;
  }

  return node;
}

/*--- Public Entry Point ---*/

expr_t::ptr_op_t expr_t::parser_t::parse(std::istream& in, const parse_flags_t& flags,
                                         const optional<string>& original_string) {
  try {
    ptr_op_t top_node = parse_value_expr(in, flags);

    // A caller that reads on past the expression says so with PARSE_PARTIAL
    // or PARSE_SINGLE; for every other caller the expression is the whole
    // input, and anything left over is a mistake to report, not to drop.
    if (!flags.has_flags(PARSE_PARTIAL) && !flags.has_flags(PARSE_SINGLE)) {
      token_t& tok = next_token(in, flags.plus_flags(PARSE_OP_CONTEXT));
      if (tok.kind != token_t::TOK_EOF)
        tok.unexpected();
      push_token(tok);
    }

    if (use_lookahead) {
      use_lookahead = false;
      lookahead.rewind(in);
    }
    lookahead.clear();

    return top_node;
  } catch (const std::exception&) {
    // Enrich the error with positional context showing where in the
    // original expression text the parse failure occurred.
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

      add_error_context(line_context(*original_string, static_cast<string::size_type>(pos),
                                     static_cast<string::size_type>(end_pos)));
    }
    throw;
  }
}

} // namespace ledger
