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

#include "generate.h"
#include "session.h"

namespace ledger {

generate_posts_iterator::generate_posts_iterator
  (session_t&   _session,
   unsigned int _seed,
   std::size_t  _quantity)
  : session(_session), seed(_seed), quantity(_quantity),

    rnd_gen(seed == 0 ? static_cast<unsigned int>(std::time(0)) : seed),

    year_range(1900, 2300), year_gen(rnd_gen, year_range),
    mon_range(1, 12), mon_gen(rnd_gen, mon_range),
    day_range(1, 28), day_gen(rnd_gen, day_range),

    upchar_range('A', 'Z'), upchar_gen(rnd_gen, upchar_range),
    downchar_range('a', 'z'), downchar_gen(rnd_gen, downchar_range),
    numchar_range('0', '9'), numchar_gen(rnd_gen, numchar_range),

    truth_range(0, 1), truth_gen(rnd_gen, truth_range),
    three_range(1, 3), three_gen(rnd_gen, three_range),
    six_range(1, 6), six_gen(rnd_gen, six_range),
    two_six_range(2, 6), two_six_gen(rnd_gen, two_six_range),
    strlen_range(1, 40), strlen_gen(rnd_gen, strlen_range),

    neg_number_range(-10000, -1), neg_number_gen(rnd_gen, neg_number_range),
    pos_number_range(1, 10000), pos_number_gen(rnd_gen, pos_number_range)
{
  std::ostringstream next_date_buf;
  generate_date(next_date_buf);
  next_date = parse_date(next_date_buf.str());

  std::ostringstream next_aux_date_buf;
  generate_date(next_aux_date_buf);
  next_aux_date = parse_date(next_aux_date_buf.str());

  TRACE_CTOR(generate_posts_iterator, "bool");
}

void generate_posts_iterator::generate_string(std::ostream& out, int len,
                                              bool only_alpha)
{
  DEBUG("generate.post.string",
        "Generating string of length " << len << ", only alpha " << only_alpha);

  int last = -1;
  bool first = true;
  for (int i = 0; i < len; i++) {
    int next = only_alpha ? 3 : three_gen();
    bool output = true;
    switch (next) {
    case 1:                     // colon
      if (! first && last == 3 && strlen_gen() % 10 == 0 && i + 1 != len)
        out << ':';
      else {
        i--;
        output = false;
      }
      break;
    case 2:                     // space
      if (! first && last == 3 && strlen_gen() % 20 == 0 && i + 1 != len)
        out << ' ';
      else {
        i--;
        output = false;
      }
      break;
    case 3:                     // character
      switch (three_gen()) {
      case 1:                   // uppercase
        out << char(upchar_gen());
        break;
      case 2:                   // lowercase
        out << char(downchar_gen());
        break;
      case 3:                   // number
        if (! only_alpha && ! first)
          out << char(numchar_gen());
        else {
          i--;
          output = false;
        }
        break;
      }
      break;
    }
    if (output) {
      last = next;
      first = false;
    }
  }
}

bool generate_posts_iterator::generate_account(std::ostream& out,
                                               bool no_virtual)
{
  bool must_balance = true;
  bool is_virtual   = false;

  if (! no_virtual) {
    switch (three_gen()) {
    case 1:
      out << '[';
      is_virtual = true;
      break;
    case 2:
      out << '(';
      must_balance = false;
      is_virtual = true;
      break;
    case 3:
      break;
    }
  }

  generate_string(out, strlen_gen());

  if (is_virtual) {
    if (must_balance)
      out << ']';
    else
      out << ')';
  }

  return must_balance;
}

void generate_posts_iterator::generate_commodity(std::ostream& out,
                                                 const string& exclude)
{
  string comm;
  do {
    std::ostringstream buf;
    generate_string(buf, six_gen(), true);
    comm = buf.str();
  }
  while (comm == exclude || comm == "h" || comm == "m" || comm == "s" ||
         comm == "and" || comm == "any" || comm == "all" || comm == "div" ||
         comm == "false" || comm == "or" || comm == "not" ||
         comm == "true" || comm == "if" || comm == "else");

  out << comm;
}

string generate_posts_iterator::generate_amount(std::ostream& out,
                                                value_t       not_this_amount,
                                                bool          no_negative,
                                                const string& exclude)
{
  std::ostringstream buf;

  if (truth_gen()) {            // commodity goes in front
    generate_commodity(buf, exclude);
    if (truth_gen())
      buf << ' ';
    if (no_negative || truth_gen())
      buf << pos_number_gen();
    else
      buf << neg_number_gen();
  } else {
    if (no_negative || truth_gen())
      buf << pos_number_gen();
    else
      buf << neg_number_gen();
    if (truth_gen())
      buf << ' ';
    generate_commodity(buf, exclude);
  }

  // Possibly generate an annotized commodity, but make it rarer
  if (! no_negative && three_gen() == 1) {
    if (three_gen() == 1) {
      buf << " {";
      generate_amount(buf, value_t(), true);
      buf << '}';
    }
    if (six_gen() == 1) {
      buf << " [";
      generate_date(buf);
      buf << ']';
    }
    if (six_gen() == 1) {
      buf << " (";
      generate_string(buf, six_gen());
      buf << ')';
    }
  }

  if (! not_this_amount.is_null() &&
      value_t(buf.str()).as_amount().commodity() ==
      not_this_amount.as_amount().commodity())
    return "";

  out << buf.str();

  return buf.str();
}

bool generate_posts_iterator::generate_post(std::ostream& out, bool no_amount)
{
  out << "    ";
  bool must_balance = generate_account(out, no_amount);
  out << "  ";

  if (! no_amount) {
    value_t amount(generate_amount(out));
    if (truth_gen())
      generate_cost(out, amount);
  }
  if (truth_gen())
    generate_note(out);
  out << '\n';

  return must_balance;
}

void generate_posts_iterator::generate_cost(std::ostream& out, value_t amount)
{
  std::ostringstream buf;

  if (truth_gen())
    buf << " @ ";
  else
    buf << " @@ ";

  if (! generate_amount(buf, amount, true,
                        amount.as_amount().commodity().symbol()).empty())
    out << buf.str();
}

void generate_posts_iterator::generate_date(std::ostream& out)
{
  out.width(4);
  out.fill('0');
  out << year_gen();

  out.width(1);
  out << '/';

  out.width(2);
  out.fill('0');
  out << mon_gen();

  out.width(1);
  out << '/';

  out.width(2);
  out.fill('0');
  out << day_gen();
}

void generate_posts_iterator::generate_state(std::ostream& out)
{
  switch (three_gen()) {
  case 1:
    out << "* ";
    break;
  case 2:
    out << "! ";
    break;
  case 3:
    out << "";
    break;
  }
}

void generate_posts_iterator::generate_code(std::ostream& out)
{
  out << '(';
  generate_string(out, six_gen());
  out << ") ";
}

void generate_posts_iterator::generate_payee(std::ostream& out)
{
  generate_string(out, strlen_gen());
}

void generate_posts_iterator::generate_note(std::ostream& out)
{
  out << "\n    ; ";
  generate_string(out, strlen_gen());
}

void generate_posts_iterator::generate_xact(std::ostream& out)
{
  out << format_date(next_date, FMT_WRITTEN);
  next_date += gregorian::days(six_gen());
  if (truth_gen()) {
    out << '=';
    out << format_date(next_aux_date, FMT_WRITTEN);
    next_aux_date += gregorian::days(six_gen());
  }
  out << ' ';

  generate_state(out);
  generate_code(out);
  generate_payee(out);
  if (truth_gen())
    generate_note(out);
  out << '\n';

  int  count = three_gen() * 2;
  bool has_must_balance = false;
  for (int i = 0; i < count; i++) {
    if (generate_post(out))
      has_must_balance = true;
  }
  if (has_must_balance)
    generate_post(out, true);

  out << '\n';
}

void generate_posts_iterator::increment()
{
  post_t * post = *posts++;

  if (post == NULL && quantity > 0) {
    std::ostringstream buf;
    generate_xact(buf);

    DEBUG("generate.post", "The post we intend to parse:\n" << buf.str());

    try {
      shared_ptr<std::istringstream> in(new std::istringstream(buf.str()));

      parse_context_stack_t parsing_context;
      parsing_context.push(in);
      parsing_context.get_current().journal = session.journal.get();
      parsing_context.get_current().scope   = &session;

      if (session.journal->read(parsing_context) != 0) {
        VERIFY(session.journal->xacts.back()->valid());
        posts.reset(*session.journal->xacts.back());
        post = *posts++;
      }
    }
    catch (std::exception&) {
      add_error_context(_f("While parsing generated transaction (seed %1%):")
                        % seed);
      add_error_context(buf.str());
      throw;
    }
    catch (int) {
      add_error_context(_f("While parsing generated transaction (seed %1%):")
                        % seed);
      add_error_context(buf.str());
      throw;
    }

    quantity--;
  }

  m_node = post;
}

} // namespace ledger
