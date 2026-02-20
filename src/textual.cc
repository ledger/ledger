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

#include "textual_internal.h"

namespace ledger {

using detail::instance_t;
using detail::application_t;

void instance_t::parse() {
  INFO("Parsing file " << context.pathname);

  TRACE_START(instance_parse, 1, "Done parsing file " << context.pathname);

  if (!in.good() || in.eof())
    return;

  context.linenum = 0;
  context.curr_pos = in.tellg();

  bool error_flag = false;
  xact_t* previous_xact = NULL;

  while (in.good() && !in.eof() && in.peek() != '^' && in.good()) {
    try {
      if (xact_t* xact = read_next_directive(error_flag, previous_xact)) {
        previous_xact = xact;
      }
    } catch (const std::exception& err) {
      error_flag = true;

      string current_context = error_context();

      if (parent) {
        std::list<instance_t*> instances;

        for (instance_t* instance = parent; instance; instance = instance->parent)
          instances.push_front(instance);

        for (instance_t* instance : instances)
          add_error_context(_f("In file included from %1%") % instance->context.location());
      }
      add_error_context(_f("While parsing file %1%") % context.location());

      if (caught_signal != NONE_CAUGHT)
        throw;

      string err_context = error_context();
      if (!err_context.empty())
        std::cerr << err_context << std::endl;

      if (!current_context.empty())
        std::cerr << current_context << std::endl;

      std::cerr << _("Error: ") << err.what() << std::endl;
      context.errors++;
      if (!current_context.empty())
        context.last = current_context + "\n" + err.what();
      else
        context.last = err.what();
    }
  }

  if (std::holds_alternative<boost::optional<datetime_t>>(apply_stack.front().value)) {
    epoch = std::get<boost::optional<datetime_t>>(apply_stack.front().value);
    year_directive_year = apply_stack.front().saved_year_directive;
  }

  apply_stack.pop_front();

#if TIMELOG_SUPPORT
  timelog.close();
#endif // TIMELOG_SUPPORT

  TRACE_STOP(instance_parse, 1);
}

std::streamsize instance_t::read_line(char*& line) {
  assert(in.good());
  assert(!in.eof()); // no one should call us in that case

  context.line_beg_pos = context.curr_pos;

  check_for_signal();

  const size_t maxLine = parse_context_t::MAX_LINE;
  in.getline(context.linebuf, maxLine);
  std::streamsize len = in.gcount();

  if (in.fail() && len == (parse_context_t::MAX_LINE - 1)) {
    throw_(parse_error, _f("Line exceeds %1% characters") % maxLine);
  }

  if (len > 0) {
    context.linenum++;

    context.curr_pos = context.line_beg_pos;
    context.curr_pos += len;

    if (context.linenum == 0 &&
        utf8::starts_with_bom(context.linebuf, context.linebuf + sizeof(context.linebuf))) {
      line = &context.linebuf[3];
      len -= 3;
    } else {
      line = context.linebuf;
    }

    if (!in.eof()) {
      // if we are not at the end of the file, len includes the new line character,
      // even through it does not appear in linebuf
      --len;
    }

    // strip trailing whitespace
    while (len > 0 && std::isspace(static_cast<unsigned char>(line[len - 1])))
      line[--len] = '\0';

    return len;
  }
  return 0;
}

xact_t* instance_t::read_next_directive(bool& error_flag, xact_t* previous_xact) {
  char* line;
  std::streamsize len = read_line(line);
  if (len == 0 || line == NULL)
    return NULL;

  if (!std::isspace(static_cast<unsigned char>(line[0])))
    error_flag = false;

  switch (line[0]) {
  case '\0':
    assert(false); // shouldn't ever reach here
    break;

  case ' ':
  case '\t':
    if (!error_flag)
      throw parse_error(_("Unexpected whitespace at beginning of line"));
    break;

  case ';': // comments
  case '#':
  case '*':
  case '|':
    break;

  case '-': // option setting
    option_directive(line);
    break;

  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    return xact_directive(line, len, previous_xact);
  case '=': // automated xact
    automated_xact_directive(line);
    break;
  case '~': // period xact
    period_xact_directive(line);
    break;

  case '@':
  case '!':
    line++;
    // fall through...
  default: // some other directive
    if (!general_directive(line)) {
      switch (line[0]) {
#if TIMELOG_SUPPORT
      case 'i':
        clock_in_directive(line, false);
        break;
      case 'I':
        clock_in_directive(line, true);
        break;

      case 'o':
        clock_out_directive(line, false);
        break;
      case 'O':
        clock_out_directive(line, true);
        break;

      case 'h':
      case 'b':
        break;
#endif // TIMELOG_SUPPORT

      case 'A': // a default account for unbalanced posts
        default_account_directive(line + 1);
        break;
      case 'C': // a set of conversions
        price_conversion_directive(line);
        break;
      case 'D': // a default commodity for "xact"
        default_commodity_directive(line);
        break;
      case 'N': // don't download prices
        nomarket_directive(line);
        break;
      case 'P': // a pricing xact
        price_xact_directive(line);
        break;
      case 'Y': // set the current year
        if (std::strlen(line + 1) == 0)
          throw_(parse_error, _f("Directive '%1%' requires an argument") % line[0]);
        apply_year_directive(line + 1);
        break;
      }
    }
    break;
  }

  return NULL;
}

expr_t::ptr_op_t instance_t::lookup(const symbol_t::kind_t kind, const string& name) {
  return context.scope->lookup(kind, name);
}

std::size_t journal_t::read_textual(parse_context_stack_t& context_stack, hash_type_t hash_type) {
  TRACE_START(parsing_total, 1, "Total time spent parsing text:");
  {
    instance_t instance(context_stack, context_stack.get_current(), NULL,
                        checking_style == journal_t::CHECK_PERMISSIVE, hash_type);
    instance.apply_stack.push_front(application_t("account", context_stack.get_current().master));
    instance.parse();
  }
  TRACE_STOP(parsing_total, 1);

  // Apply any deferred postings at this time
  master->apply_deferred_posts();

  // These tracers were started in textual.cc
  TRACE_FINISH(xact_text, 1);
  TRACE_FINISH(xact_details, 1);
  TRACE_FINISH(xact_posts, 1);
  TRACE_FINISH(xacts, 1);
  TRACE_FINISH(instance_parse, 1); // report per-instance timers
  TRACE_FINISH(parsing_total, 1);

  if (context_stack.get_current().errors > 0)
    throw error_count(context_stack.get_current().errors, context_stack.get_current().last);

  return context_stack.get_current().count;
}

} // namespace ledger
