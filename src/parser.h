/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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
 * @file   parser.h
 * @author John Wiegley
 *
 * @ingroup expr
 */
#pragma once

#include "token.h"
#include "op.h"

namespace ledger {

class expr_t::parser_t : public noncopyable
{
  mutable token_t lookahead;
  mutable bool    use_lookahead;

  token_t& next_token(std::istream& in, const parse_flags_t& tflags,
                      const optional<token_t::kind_t>& expecting = none) const {
    if (use_lookahead)
      use_lookahead = false;
    else
      lookahead.next(in, tflags);

    if (expecting && lookahead.kind != *expecting)
      lookahead.expected(*expecting);

    return lookahead;
  }

  void push_token(const token_t& tok) const {
    assert(&tok == &lookahead);
    use_lookahead = true;
  }

  void push_token() const {
    use_lookahead = true;
  }

  ptr_op_t parse_value_term(std::istream& in,
                            const parse_flags_t& flags) const;
  ptr_op_t parse_call_expr(std::istream& in,
                           const parse_flags_t& flags) const;
  ptr_op_t parse_dot_expr(std::istream& in,
                          const parse_flags_t& flags) const;
  ptr_op_t parse_unary_expr(std::istream& in,
                            const parse_flags_t& flags) const;
  ptr_op_t parse_mul_expr(std::istream& in,
                          const parse_flags_t& flags) const;
  ptr_op_t parse_add_expr(std::istream& in,
                          const parse_flags_t& flags) const;
  ptr_op_t parse_logic_expr(std::istream& in,
                            const parse_flags_t& flags) const;
  ptr_op_t parse_and_expr(std::istream& in,
                          const parse_flags_t& flags) const;
  ptr_op_t parse_or_expr(std::istream& in,
                         const parse_flags_t& flags) const;
  ptr_op_t parse_querycolon_expr(std::istream& in,
                                 const parse_flags_t& flags) const;
  ptr_op_t parse_comma_expr(std::istream& in,
                            const parse_flags_t& flags) const;
  ptr_op_t parse_lambda_expr(std::istream& in,
                             const parse_flags_t& flags) const;
  ptr_op_t parse_assign_expr(std::istream& in,
                             const parse_flags_t& flags) const;
  ptr_op_t parse_value_expr(std::istream& in,
                            const parse_flags_t& flags) const;

public:
  parser_t() : use_lookahead(false) {
    TRACE_CTOR(parser_t, "");
  }
  ~parser_t() throw() {
    TRACE_DTOR(parser_t);
  }

  ptr_op_t parse(std::istream&           in,
                 const parse_flags_t&    flags           = PARSE_DEFAULT,
                 const optional<string>& original_string = boost::none);
};

} // namespace ledger
