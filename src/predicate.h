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

/**
 * @addtogroup expr
 */

/**
 * @file   predicate.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _PREDICATE_H
#define _PREDICATE_H

#include "expr.h"
#include "commodity.h"
#include "annotate.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class item_predicate
{
public:
  expr_t	 predicate;
  keep_details_t what_to_keep;

  item_predicate() {
    TRACE_CTOR(item_predicate, "");
  }
  item_predicate(const item_predicate& other)
    : predicate(other.predicate), what_to_keep(other.what_to_keep) {
    TRACE_CTOR(item_predicate, "copy");
  }
  item_predicate(const expr_t&	       _predicate,
		 const keep_details_t& _what_to_keep)
    : predicate(_predicate), what_to_keep(_what_to_keep) {
    TRACE_CTOR(item_predicate, "const expr_t&, const keep_details_t&");
  }
  item_predicate(const string&	       _predicate,
		 const keep_details_t& _what_to_keep)
    : predicate(expr_t(_predicate)), what_to_keep(_what_to_keep) {
    TRACE_CTOR(item_predicate, "const string&, const keep_details_t&");
  }
  ~item_predicate() throw() {
    TRACE_DTOR(item_predicate);
  }

  bool operator()(scope_t& item) {
    try {
      return ! predicate || predicate.calc(item).strip_annotations(what_to_keep);
    }
    catch (const std::exception& err) {
      add_error_context(_("While determining truth of predicate expression:"));
      add_error_context(expr_context(predicate));
      throw;
    }
  }

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive & ar, const unsigned int /* version */) {
    ar & predicate;
    ar & what_to_keep;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

class query_lexer_t
{
  friend class query_parser_t;

  value_t::sequence_t::const_iterator begin;
  value_t::sequence_t::const_iterator end;

  string::const_iterator arg_i;
  string::const_iterator arg_end;

  bool	  consume_whitespace;

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

      TOK_ACCOUNT,
      TOK_PAYEE,
      TOK_CODE,
      TOK_NOTE,
      TOK_META,
      TOK_EXPR,

      TERM,

      END_REACHED

    } kind;

    optional<string> value;

    explicit token_t(kind_t _kind = UNKNOWN,
		     const optional<string>& _value = none)
      : kind(_kind), value(_value) {
      TRACE_CTOR(query_lexer_t::token_t, "");
    }
    token_t(const token_t& tok)
      : kind(tok.kind), value(tok.value) {
      TRACE_CTOR(query_lexer_t::token_t, "copy");
    }
    ~token_t() throw() {
      TRACE_DTOR(query_lexer_t::token_t);
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
      case UNKNOWN:	return "UNKNOWN";
      case LPAREN:	return "LPAREN";
      case RPAREN:	return "RPAREN";
      case TOK_NOT:	return "TOK_NOT";
      case TOK_AND:	return "TOK_AND";
      case TOK_OR:	return "TOK_OR";
      case TOK_EQ:	return "TOK_EQ";
      case TOK_ACCOUNT: return "TOK_ACCOUNT";
      case TOK_PAYEE:	return "TOK_PAYEE";
      case TOK_CODE:	return "TOK_CODE";
      case TOK_NOTE:	return "TOK_NOTE";
      case TOK_META:	return "TOK_META";
      case TOK_EXPR:	return "TOK_EXPR";
      case TERM:	return string("TERM(") + *value + ")";
      case END_REACHED: return "END_REACHED";
      }
    }

    string symbol() const {
      switch (kind) {
      case LPAREN:	return "(";
      case RPAREN:	return ")";
      case TOK_NOT:	return "not";
      case TOK_AND:	return "and";
      case TOK_OR:	return "or";
      case TOK_EQ:	return "=";
      case TOK_ACCOUNT: return "account";
      case TOK_PAYEE:	return "payee";
      case TOK_CODE:	return "code";
      case TOK_NOTE:	return "note";
      case TOK_META:	return "meta";
      case TOK_EXPR:	return "expr";

      case END_REACHED: return "<EOF>";

      case TERM:
	assert(0);
	return "<TERM>";

      case UNKNOWN:
      default:
	assert(0);
	return "<UNKNOWN>";
      }
    }

    void unexpected();
    void expected(char wanted, char c = '\0');
  };

  token_t token_cache;

  query_lexer_t(value_t::sequence_t::const_iterator _begin,
		value_t::sequence_t::const_iterator _end)
    : begin(_begin), end(_end), consume_whitespace(false)
  {
    TRACE_CTOR(query_lexer_t, "");
    assert(begin != end);
    arg_i   = (*begin).as_string().begin();
    arg_end = (*begin).as_string().end();
  }
  query_lexer_t(const query_lexer_t& lexer)
    : begin(lexer.begin), end(lexer.end),
      arg_i(lexer.arg_i), arg_end(lexer.arg_end),
      consume_whitespace(lexer.consume_whitespace),
      token_cache(lexer.token_cache)
  {
    TRACE_CTOR(query_lexer_t, "copy");
  }
  ~query_lexer_t() throw() {
    TRACE_DTOR(query_lexer_t);
  }

  token_t next_token();
  void    push_token(token_t tok) {
    assert(token_cache.kind == token_t::UNKNOWN);
    token_cache = tok;
  }
  token_t peek_token() {
    if (token_cache.kind == token_t::UNKNOWN)
      token_cache = next_token();
    return token_cache;
  }
};

class query_parser_t
{
  query_lexer_t lexer;

  expr_t::ptr_op_t parse_query_term(query_lexer_t::token_t::kind_t tok_context);
  expr_t::ptr_op_t parse_unary_expr(query_lexer_t::token_t::kind_t tok_context);
  expr_t::ptr_op_t parse_and_expr(query_lexer_t::token_t::kind_t tok_context);
  expr_t::ptr_op_t parse_or_expr(query_lexer_t::token_t::kind_t tok_context);
  expr_t::ptr_op_t parse_query_expr(query_lexer_t::token_t::kind_t tok_context);

public:
  query_parser_t(value_t::sequence_t::const_iterator begin,
		 value_t::sequence_t::const_iterator end)
    : lexer(begin, end) {
    TRACE_CTOR(query_parser_t, "");
  }
  query_parser_t(const query_parser_t& parser)
    : lexer(parser.lexer) {
    TRACE_CTOR(query_parser_t, "copy");
  }
  ~query_parser_t() throw() {
    TRACE_DTOR(query_parser_t);
  }

  expr_t::ptr_op_t parse();

  bool tokens_remaining() {
    query_lexer_t::token_t tok = lexer.peek_token();
    assert(tok.kind != query_lexer_t::token_t::UNKNOWN);
    return tok.kind != query_lexer_t::token_t::END_REACHED;
  }
};

std::pair<expr_t, query_parser_t>
args_to_predicate(value_t::sequence_t::const_iterator begin,
		  value_t::sequence_t::const_iterator end);

std::pair<expr_t, query_parser_t>
args_to_predicate(query_parser_t parser);

} // namespace ledger

#endif // _PREDICATE_H
