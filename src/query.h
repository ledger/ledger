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

/**
 * @addtogroup expr
 */

/**
 * @file   predicate.h
 * @author John Wiegley
 *
 * @ingroup expr
 */
#ifndef _QUERY_H
#define _QUERY_H

#include "predicate.h"

namespace ledger {

class query_t
{
protected:
  class parser_t;

public:
  class lexer_t
  {
    friend class query_t;
    friend class parser_t;

    value_t::sequence_t::const_iterator begin;
    value_t::sequence_t::const_iterator end;

    string::const_iterator prev_arg_i;
    string::const_iterator arg_i;
    string::const_iterator arg_end;

    bool consume_whitespace;
    bool consume_next_arg;
    bool multiple_args;

  public:
    struct token_t
    {
      enum kind_t {
        UNKNOWN,

        LPAREN,
        RPAREN,

        TOK_NOT,
        TOK_AND,
        TOK_OR,
        TOK_EQ,

        TOK_CODE,
        TOK_PAYEE,
        TOK_NOTE,
        TOK_ACCOUNT,
        TOK_META,
        TOK_EXPR,

        TOK_SHOW,
        TOK_ONLY,
        TOK_BOLD,
        TOK_FOR,
        TOK_SINCE,
        TOK_UNTIL,

        TERM,

        END_REACHED

      } kind;

      optional<string> value;

      explicit token_t(kind_t _kind = UNKNOWN,
                       const optional<string>& _value = none)
        : kind(_kind), value(_value) {
        TRACE_CTOR(query_t::lexer_t::token_t, "");
      }
      token_t(const token_t& tok)
        : kind(tok.kind), value(tok.value) {
        TRACE_CTOR(query_t::lexer_t::token_t, "copy");
      }
      ~token_t() throw() {
        TRACE_DTOR(query_t::lexer_t::token_t);
      }

      token_t& operator=(const token_t& tok) {
        if (this != &tok) {
          kind  = tok.kind;
          value = tok.value;
        }
        return *this;
      }

      operator bool() const {
        return kind != END_REACHED;
      }

      string to_string() const {
        switch (kind) {
        case UNKNOWN:     return "UNKNOWN";
        case LPAREN:      return "LPAREN";
        case RPAREN:      return "RPAREN";
        case TOK_NOT:     return "TOK_NOT";
        case TOK_AND:     return "TOK_AND";
        case TOK_OR:      return "TOK_OR";
        case TOK_EQ:      return "TOK_EQ";
        case TOK_CODE:    return "TOK_CODE";
        case TOK_PAYEE:   return "TOK_PAYEE";
        case TOK_NOTE:    return "TOK_NOTE";
        case TOK_ACCOUNT: return "TOK_ACCOUNT";
        case TOK_META:    return "TOK_META";
        case TOK_EXPR:    return "TOK_EXPR";
        case TOK_SHOW:    return "TOK_SHOW";
        case TOK_ONLY:    return "TOK_ONLY";
        case TOK_BOLD:    return "TOK_BOLD";
        case TOK_FOR:     return "TOK_FOR";
        case TOK_SINCE:   return "TOK_SINCE";
        case TOK_UNTIL:   return "TOK_UNTIL";
        case TERM:        return string("TERM(") + *value + ")";
        case END_REACHED: return "END_REACHED";
        }
      }

      string symbol() const {
        switch (kind) {
        case LPAREN:      return "(";
        case RPAREN:      return ")";
        case TOK_NOT:     return "not";
        case TOK_AND:     return "and";
        case TOK_OR:      return "or";
        case TOK_EQ:      return "=";
        case TOK_CODE:    return "code";
        case TOK_PAYEE:   return "payee";
        case TOK_NOTE:    return "note";
        case TOK_ACCOUNT: return "account";
        case TOK_META:    return "meta";
        case TOK_EXPR:    return "expr";
        case TOK_SHOW:    return "show";
        case TOK_ONLY:    return "only";
        case TOK_BOLD:    return "bold";
        case TOK_FOR:     return "for";
        case TOK_SINCE:   return "since";
        case TOK_UNTIL:   return "until";

        case END_REACHED: return "<EOF>";

        case TERM:
          assert(false);
          return "<TERM>";

        case UNKNOWN:
          assert(false);
          return "<UNKNOWN>";
        }
#if !defined(__clang__)
        return "<ERROR>";
#endif
      }

      void unexpected();
      void expected(char wanted, char c = '\0');
    };

    token_t token_cache;

    lexer_t(value_t::sequence_t::const_iterator _begin,
            value_t::sequence_t::const_iterator _end,
            bool _multiple_args = true)
      : begin(_begin), end(_end),
        consume_whitespace(false), consume_next_arg(false),
        multiple_args(_multiple_args)
    {
      assert(begin != end);
      arg_i   = (*begin).as_string().begin();
      arg_end = (*begin).as_string().end();

      TRACE_CTOR(query_t::lexer_t, "");
    }
    lexer_t(const lexer_t& lexer)
      : begin(lexer.begin), end(lexer.end),
        arg_i(lexer.arg_i), arg_end(lexer.arg_end),
        consume_whitespace(lexer.consume_whitespace),
        consume_next_arg(lexer.consume_next_arg),
        multiple_args(lexer.multiple_args), token_cache(lexer.token_cache)
    {
      TRACE_CTOR(query_t::lexer_t, "copy");
    }
    ~lexer_t() throw() {
      TRACE_DTOR(query_t::lexer_t);
    }

    token_t next_token(token_t::kind_t tok_context = token_t::UNKNOWN);
    void    push_token(token_t tok) {
      assert(token_cache.kind == token_t::UNKNOWN);
      token_cache = tok;
    }
    token_t peek_token(token_t::kind_t tok_context = token_t::UNKNOWN) {
      if (token_cache.kind == token_t::UNKNOWN)
        token_cache = next_token(tok_context);
      return token_cache;
    }
  };

  enum kind_t {
    QUERY_LIMIT,
    QUERY_SHOW,
    QUERY_ONLY,
    QUERY_BOLD,
    QUERY_FOR
  };

  typedef std::map<kind_t, string> query_map_t;

protected:
  class parser_t
  {
    friend class query_t;

    value_t        args;
    lexer_t        lexer;
    keep_details_t what_to_keep;
    query_map_t    query_map;

    expr_t::ptr_op_t parse_query_term(lexer_t::token_t::kind_t tok_context);
    expr_t::ptr_op_t parse_unary_expr(lexer_t::token_t::kind_t tok_context);
    expr_t::ptr_op_t parse_and_expr(lexer_t::token_t::kind_t tok_context);
    expr_t::ptr_op_t parse_or_expr(lexer_t::token_t::kind_t tok_context);
    expr_t::ptr_op_t parse_query_expr(lexer_t::token_t::kind_t tok_context,
                                      bool subexpression = false);

  public:
    parser_t(const value_t&        _args,
             const keep_details_t& _what_to_keep = keep_details_t(),
             bool                  multiple_args = true)
      : args(_args), lexer(args.begin(), args.end(), multiple_args),
        what_to_keep(_what_to_keep) {
      TRACE_CTOR(query_t::parser_t, "value_t, keep_details_t, bool");
    }
    parser_t(const parser_t& other)
      : args(other.args), lexer(other.lexer) {
      TRACE_CTOR(query_t::parser_t, "copy");
    }
    ~parser_t() throw() {
      TRACE_DTOR(query_t::parser_t);
    }

    expr_t::ptr_op_t parse(bool subexpression = false) {
      return parse_query_expr(lexer_t::token_t::TOK_ACCOUNT, subexpression);
    }

    bool tokens_remaining() {
      lexer_t::token_t tok = lexer.peek_token();
      assert(tok.kind != lexer_t::token_t::UNKNOWN);
      return tok.kind != lexer_t::token_t::END_REACHED;
    }
  };

  optional<parser_t> parser;
  query_map_t        predicates;

public:
  query_t() {
    TRACE_CTOR(query_t, "");
  }
  query_t(const query_t& other)
    : parser(other.parser), predicates(other.predicates) {
    TRACE_CTOR(query_t, "copy");
  }
  query_t(const string&         arg,
          const keep_details_t& what_to_keep  = keep_details_t(),
          bool                  multiple_args = true) {
    if (! arg.empty()) {
      value_t temp(string_value(arg));
      parse_args(temp.to_sequence(), what_to_keep, multiple_args);
    }
    TRACE_CTOR(query_t, "string, keep_details_t, bool");
  }
  query_t(const value_t&        args,
          const keep_details_t& what_to_keep  = keep_details_t(),
          bool                  multiple_args = true) {
    if (! args.empty())
      parse_args(args, what_to_keep, multiple_args);

    TRACE_CTOR(query_t, "value_t, keep_details_t, bool");
  }
  virtual ~query_t() {
    TRACE_DTOR(query_t);
  }

  expr_t::ptr_op_t
  parse_args(const value_t&        args,
             const keep_details_t& what_to_keep  = keep_details_t(),
             bool                  multiple_args = true,
             bool                  subexpression = false) {
    if (! parser)
      parser = parser_t(args, what_to_keep, multiple_args);
    return parser->parse(subexpression);
  }

  bool has_query(const kind_t& id) const {
    return parser && parser->query_map.find(id) != parser->query_map.end();
  }
  string get_query(const kind_t& id) const {
    if (parser) {
      query_map_t::const_iterator i = parser->query_map.find(id);
      if (i != parser->query_map.end())
        return (*i).second;
    }
    return empty_string;
  }

  bool tokens_remaining() {
    return parser && parser->tokens_remaining();
  }
};

} // namespace ledger

#endif // _QUERY_H
