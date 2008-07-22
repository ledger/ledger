/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#ifndef _PARSEXP_H
#define _PARSEXP_H

#include "valexpr.h"

namespace ledger {
namespace expr {

DECLARE_EXCEPTION(error, parse_error);

class parser_t
{
#define EXPR_PARSE_NORMAL     0x00
#define EXPR_PARSE_PARTIAL    0x01
#define EXPR_PARSE_RELAXED    0x02
#define EXPR_PARSE_NO_MIGRATE 0x04
#define EXPR_PARSE_NO_REDUCE  0x08
#define EXPR_PARSE_ALLOW_DATE 0x10

public:
  typedef uint_least8_t flags_t;

private:
  struct token_t
  {
    enum kind_t {
      VALUE,			// any kind of literal value

      IDENT,			// [A-Za-z_][-A-Za-z0-9_:]*
      DOLLAR,			// $
      AT_SYM,			// @

      DOT,			// .
      DOTDOT,			// ..
      SLASH,			// /

      LPAREN,			// (
      RPAREN,			// )
      LBRACKET,			// [
      RBRACKET,			// ]

      EQUAL,			// =
      NEQUAL,			// !=
      LESS,			// <
      LESSEQ,			// <=
      GREATER,			// >
      GREATEREQ,		// >=

      MINUS,			// -
      PLUS,			// +
      STAR,			// *
      KW_DIV,

      EXCLAM,			// !
      KW_AND,
      KW_OR,
      KW_MOD,

#if 0
      PIPE,			// |
      KW_UNION,
#endif

      COMMA,			// ,

      TOK_EOF,
      UNKNOWN
    } kind;

    char	      symbol[3];
    value_t     value;
    std::size_t length;

    explicit token_t() : kind(UNKNOWN), length(0) {
      TRACE_CTOR(token_t, "");
    }
    token_t(const token_t& other) {
      assert(false);
      TRACE_CTOR(token_t, "copy");
      *this = other;
    }
    ~token_t() {
      TRACE_DTOR(token_t);
    }

    token_t& operator=(const token_t& other) {
      if (&other == this)
	return *this;
      assert(false);
      return *this;
    }

    void clear() {
      kind   = UNKNOWN;
      length = 0;
      value  = NULL_VALUE;

      symbol[0] = '\0';
      symbol[1] = '\0';
      symbol[2] = '\0';
    }

    void parse_ident(std::istream& in);
    void next(std::istream& in, flags_t flags);
    void rewind(std::istream& in);
    void unexpected();

    static void unexpected(char c, char wanted = '\0');
  };

  mutable token_t lookahead;
  mutable bool	  use_lookahead;

  token_t& next_token(std::istream& in, flags_t tflags) const
  {
    if (use_lookahead)
      use_lookahead = false;
    else
      lookahead.next(in, tflags);
    return lookahead;
  }

  void push_token(const token_t& tok) const
  {
    assert(&tok == &lookahead);
    use_lookahead = true;
  }
  void push_token() const
  {
    use_lookahead = true;
  }

public:
  value_expr expr;

private:
  ptr_op_t parse_value_term(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
#if 0
  ptr_op_t parse_predicate_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
  ptr_op_t parse_path_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
#endif
  ptr_op_t parse_unary_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
#if 0
  ptr_op_t parse_union_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
#endif
  ptr_op_t parse_mul_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
  ptr_op_t parse_add_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
  ptr_op_t parse_logic_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
  ptr_op_t parse_and_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
  ptr_op_t parse_or_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
  ptr_op_t parse_querycolon_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;
  ptr_op_t parse_value_expr(std::istream& in, scope_t& scope,
			    const flags_t flags) const;

  value_expr& parse_expr(std::istream& in, string& str,
			 scope_t& scope, const flags_t flags) {
    try {
      ptr_op_t top_node = parse_value_expr(in, scope, flags);
      expr = value_expr(top_node, str);

      if (use_lookahead) {
	use_lookahead = false;
	lookahead.rewind(in);
      }
      lookahead.clear();
    }
    catch (error * err) {
      err->context.push_back
	(new line_context(str, (long)in.tellg() - 1,
			  "While parsing value expression:"));
      throw err;
    }

    return expr;
  }

public:
  parser_t() : use_lookahead(false) {}

  value_expr& parse(std::istream& in,
		    const flags_t flags = EXPR_PARSE_RELAXED)
  {
    return parse_expr(in, empty_string, *global_scope, flags);
  }

  value_expr& parse(std::istream& in,
		    const flags_t flags = EXPR_PARSE_RELAXED,
		    scope_t& scope)
  {
    return parse_expr(in, empty_string, scope, flags);
  }

  value_expr& parse(string& str, const flags_t flags = EXPR_PARSE_RELAXED)
  {
    std::istringstream stream(str);
    return parse_expr(stream, str, *global_scope, flags);
  }

  value_expr& parse(string& str, const flags_t flags = EXPR_PARSE_RELAXED,
		    scope_t& scope)
  {
    std::istringstream stream(str);
    return parse_expr(stream, str, scope, flags);
  }
};

void dump(std::ostream& out, const ptr_op_t node, const int depth = 0);

bool print(std::ostream&   out,
	   const ptr_op_t  node,
	   const bool      relaxed      = true,
	   const ptr_op_t  node_to_find = NULL,
	   unsigned long * start_pos    = NULL,
	   unsigned long * end_pos      = NULL);

} // namespace expr
} // namespace ledger

#endif // _PARESXP_H
