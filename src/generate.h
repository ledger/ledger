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
 * @addtogroup generate
 */

/**
 * @file   generate.h
 * @author John Wiegley
 *
 * @ingroup report
 */
#ifndef _GENERATE_H
#define _GENERATE_H

#include "iterators.h"

namespace ledger {

class session_t;

class generate_posts_iterator
  : public iterator_facade_base<generate_posts_iterator, post_t *,
                                boost::forward_traversal_tag>
{
  session_t&   session;
  unsigned int seed;
  std::size_t  quantity;
  date_t       next_date;
  date_t       next_aux_date;

  mt19937 rnd_gen;

  typedef variate_generator<mt19937&, uniform_int<> >  int_generator_t;
  typedef variate_generator<mt19937&, uniform_real<> > real_generator_t;

  uniform_int<>   year_range;
  int_generator_t year_gen;
  uniform_int<>   mon_range;
  int_generator_t mon_gen;
  uniform_int<>   day_range;
  int_generator_t day_gen;

  uniform_int<>   upchar_range;
  int_generator_t upchar_gen;
  uniform_int<>   downchar_range;
  int_generator_t downchar_gen;
  uniform_int<>   numchar_range;
  int_generator_t numchar_gen;

  uniform_int<>   truth_range;
  int_generator_t truth_gen;
  uniform_int<>   three_range;
  int_generator_t three_gen;
  uniform_int<>   six_range;
  int_generator_t six_gen;
  uniform_int<>   two_six_range;
  int_generator_t two_six_gen;

  uniform_int<>   strlen_range;
  int_generator_t strlen_gen;

  uniform_real<>   neg_number_range;
  real_generator_t neg_number_gen;
  uniform_real<>   pos_number_range;
  real_generator_t pos_number_gen;

  xact_posts_iterator posts;

public:
  generate_posts_iterator(session_t&   _session,
                          unsigned int _seed         = 0,
                          std::size_t  _quantity     = 100);

  virtual ~generate_posts_iterator() throw() {
    TRACE_DTOR(generate_posts_iterator);
  }

  virtual void increment();

protected:
  void   generate_string(std::ostream& out, int len, bool only_alpha = false);
  bool   generate_account(std::ostream& out, bool no_virtual = false);
  void   generate_commodity(std::ostream& out, const string& exclude = "");
  string generate_amount(std::ostream& out,
                         value_t       not_this_amount = NULL_VALUE,
                         bool          no_negative     = false,
                         const string& exclude         = "");
  bool   generate_post(std::ostream& out, bool no_amount = false);
  void   generate_cost(std::ostream& out, value_t amount);
  void   generate_date(std::ostream& out);
  void   generate_state(std::ostream& out);
  void   generate_code(std::ostream& out);
  void   generate_payee(std::ostream& out);
  void   generate_note(std::ostream& out);
  void   generate_xact(std::ostream& out);
};

} // namespace ledger

#endif // _GENERATE_H
