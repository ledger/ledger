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

#include "textual.h"

namespace ledger {

using namespace xml;

#define MAX_LINE 1024

typedef builder_t::position_t position_t;

void parse_transaction(builder_t&  builder,
		       char *	   line,
		       position_t& end_of_line)
{
  // First cut up the input line into its various parts.

  char * state	      = NULL;
  char * account_path = NULL;
  char * amount	      = NULL;
  char * note	      = NULL;

  char * p = line;

  if (*p == '*' || *p == '!')
    state = p++;

  account_path = skip_ws(p);

  amount = next_element(account_path, true);
  if (amount) {
    char * p = amount;
    while (*p && *p != ';')
      p++;

    if (*p == ';') {
      *p++ = '\0';
      note = skip_ws(p);
    }

    p = amount + (std::strlen(amount) - 1);
    while (p > amount && std::isspace(*p))
      p--;

    if (std::isspace(*(p + 1)))
      *++p = '\0';
  }

  // Setup the details for this node

  if (state) {
    switch (*state) {
    case '*':
      builder.push_attr(CLEARED_ATTR, "yes");
      break;
    case '!':
      builder.push_attr(PENDING_ATTR, "yes");
      break;
    }
  }

  builder.begin_node(TRANSACTION_NODE);

  // Parse the account name

  char * b = &account_path[0];
  char * e = &account_path[std::strlen(account_path) - 1];
  if ((*b == '[' && *e == ']') ||
      (*b == '(' && *e == ')')) {
    builder.push_attr(VIRTUAL_ATTR, "yes");
    if (*b == '[')
      builder.push_attr(BALANCE_ATTR, "yes");
    *account_path++ = '\0';
    *e = '\0';
  }

  builder.begin_node(ACCOUNT_PATH_NODE, true);
  builder.append_text(account_path);
  builder.end_node(ACCOUNT_PATH_NODE);

  // Parse the optional amount

  if (amount) {
    builder.begin_node(AMOUNT_EXPR_NODE, true);
    builder.append_text(amount);
    builder.end_node(AMOUNT_EXPR_NODE);
  }

  // Parse the optional note

  if (note) {
    builder.begin_node(NOTE_NODE, true);
    builder.append_text(note);
    builder.end_node(NOTE_NODE);
  }

  builder.end_node(TRANSACTION_NODE, end_of_line);
}

bool parse_transactions(std::istream& in, builder_t& builder)
{
  TRACE_START(entry_xacts, 1, "Time spent parsing transactions:");

  bool added = false;

  while (! in.eof() && (in.peek() == ' ' || in.peek() == '\t')) {
    static char line[MAX_LINE + 1];
    line[0] = '\0';
    in.getline(line, MAX_LINE);
    if (in.eof() || line[0] == '\0')
      break;

    position_t end_of_line(builder.position());
    end_of_line.offset += std::strlen(line) + 1;
    end_of_line.linenum++;

    char * p = skip_ws(line);
    if (! *p || *p == '\r' || *p == '\n')
      break;

    parse_transaction(builder, line, end_of_line);
    added = true;
  }

  TRACE_STOP(entry_xacts, 1);

  return added;
}

void parse_entry(std::istream& in,
		 builder_t&    builder,
		 char *	       line,
		 position_t&   end_of_line)
{
  TRACE_START(entry_text, 1, "Time spent preparing entry text:");

  // First cut up the input line into its various parts

  char * date	  = NULL;
  char * date_eff = NULL;
  char * statep   = NULL;
  char * code	  = NULL;
  char * payee	  = NULL;

  date = line;

  char * p = line;

  while (*p && (std::isdigit(*p) || *p == '/' || *p == '.' || *p == '-'))
    p++;
  assert(*p);

  if (*p == '=') {
    *p++ = '\0';
    date_eff = p;

    while (*p && (std::isdigit(*p) || *p == '/' || *p == '.' || *p == '-'))
      p++;
    assert(*p);
  } else {
    *p++ = '\0';
  }

  p = skip_ws(p);

  if (*p == '*' || *p == '!') {
    statep = p;
    p++; *p++ = '\0';

    p = skip_ws(p);
  }

  if (*p == '(') {
    code = ++p;
    while (*p && *p != ')')
      p++;
    assert(*p);
    *p++ = '\0';

    p = skip_ws(p);
  }

  payee = p;

  p = payee + (std::strlen(payee) - 1);
  while (p > payee && std::isspace(*p))
    p--;

  if (std::isspace(*(p + 1)))
    *++p = '\0';

  TRACE_STOP(entry_text, 1);

  // Setup the details for this node

  TRACE_START(entry_details, 1, "Time spent parsing entry details:");

  builder.push_attr(DATE_ATTR, date);

  if (date_eff)
    builder.push_attr(EFF_DATE_ATTR, date_eff);

  if (statep) {
    switch (*statep) {
    case '*':
      builder.push_attr(CLEARED_ATTR, "yes");
      break;
    case '!':
      builder.push_attr(PENDING_ATTR, "yes");
      break;
    }
  }

  if (code)
    builder.push_attr(CODE_ATTR, code);

  builder.begin_node(ENTRY_NODE);

  builder.begin_node(PAYEE_NODE, true);
  assert(payee);
  builder.append_text(*payee != '\0' ? payee : "<Unspecified payee>");
  builder.end_node(PAYEE_NODE, end_of_line);

  TRACE_STOP(entry_details, 1);

  // Parse all the transactions associated with this entry

  if (! parse_transactions(in, builder))
    throw_(parse_error, "Entry has no transactions");

  builder.end_node(ENTRY_NODE);
}

bool textual_parser_t::test(std::istream& in) const
{
  char buf[5];

  in.read(buf, 5);
  if (std::strncmp(buf, "<?xml", 5) == 0)
    throw_(parse_error, "Ledger file contains XML data, but format was not recognized");

  in.clear();
  in.seekg(0, std::ios::beg);
  assert(in.good());
  return true;
}

std::size_t textual_parser_t::parse(std::istream& in,
				    const path&   pathname,
				    builder_t&	  builder)
{
  TRACE_START(parsing_total, 1, "Total time spent parsing text:");

  INFO("Parsing file '" << pathname.string() << "'");

  builder.begin_node(JOURNAL_NODE);

  std::size_t count = 0;

  while (in.good() && ! in.eof()) {
    static char line[MAX_LINE + 1];
    in.getline(line, MAX_LINE);
    if (in.eof())
      break;

    position_t end_of_line(builder.position());
    end_of_line.offset += std::strlen(line) + 1;
    end_of_line.linenum++;

    //PUSH_CONTEXT();

    switch (line[0]) {
    case '\0':
    case '\r':
      break;

    case ' ':
    case '\t': {
      char * p = skip_ws(line);
      if (*p && *p != '\r')
	throw_(parse_error, "Line begins with whitespace");
      break;
    }

    case 'i':
    case 'I': {
      string date(line, 2, 19);

      char * p = skip_ws(line + 22);
      char * n = next_element(p, true);

      builder.push_attr(TIME_ATTR, date);
      builder.push_attr(ACCOUNT_ATTR, p);
      builder.begin_node(CHECKIN_NODE, true);
      builder.append_text(n);
      builder.end_node(CHECKIN_NODE, end_of_line);
      break;
    }

    case 'o':
    case 'O': {
      string date(line, 2, 19);

      char * p = skip_ws(line + 22);
      char * n = next_element(p, true);

      builder.push_attr(TIME_ATTR, date);
      builder.push_attr(ACCOUNT_ATTR, p);
      builder.begin_node(CHECKIN_NODE, true);
      builder.append_text(n);
      builder.end_node(CHECKIN_NODE, end_of_line);
      break;
    }

    case 'D':	{		// specifies default commodity flags
      builder.push_attr(TEMPLATE_ATTR, skip_ws(line + 1));
      builder.push_node(COMMODITY_TEMPLATE_NODE, end_of_line);
      break;
    }

    case 'A':		        // a default account for unbalanced xacts
      builder.push_attr(NAME_ATTR, skip_ws(line + 1));
      builder.push_node(DEFAULT_ACCOUNT_NODE, end_of_line);
      break;

    case 'C':			// a set of conversions
      if (char * p = std::strchr(line + 1, '=')) {
	*p++ = '\0';
	builder.push_attr(FROM_ATTR, skip_ws(line + 1));
	builder.push_attr(TO_ATTR, p);
	builder.push_node(COMMODITY_CONVERSION_NODE, end_of_line);
      } else {
	throw_(parse_error, "Conversion entry (code C) must follow the format X=Y");
      }
      break;

    case 'P': {		// a pricing entry
      char * date_field_ptr = skip_ws(line + 1);
      char * time_field_ptr = next_element(date_field_ptr);
      if (! time_field_ptr)
	throw_(parse_error, "Pricing entry (code P) is missing arguments");
      string date_field = date_field_ptr;

      char * symbol_and_price;
      moment_t  datetime;

      if (std::isdigit(time_field_ptr[0])) {
	symbol_and_price = next_element(time_field_ptr);
	if (! symbol_and_price)
	  throw_(parse_error, "Pricing entry (code P) is missing a symbol name");
      } else {
	symbol_and_price = time_field_ptr;
      }

      builder.push_attr(DATE_ATTR, date_field_ptr);
      builder.push_attr(TIME_ATTR, time_field_ptr);

      string symbol;
      commodity_t::parse_symbol(symbol_and_price, symbol);

      builder.push_attr(SYMBOL_ATTR, symbol);
      builder.push_attr(PRICE_ATTR, skip_ws(symbol_and_price));
      builder.push_node(PRICE_HISTORY_NODE, end_of_line);
      break;
    }

    case 'N': {			// don't download prices
      char * p = skip_ws(line + 1);

      string symbol;
      commodity_t::parse_symbol(p, symbol);

      builder.push_attr(SYMBOL_ATTR, symbol);
      builder.push_node(COMMODITY_NOMARKET_NODE, end_of_line);
      break;
    }

    case 'Y':			// set current year
      builder.push_attr(YEAR_ATTR, skip_ws(line + 1));
      builder.push_node(CURRENT_YEAR_NODE, end_of_line);
      break;

    case 'h':
    case 'b':
    case ';':			// comment
      // jww (2007-05-12): Read in the comment and save it
      break;

    case '@':
    case '!': {                 // directive
      char * p = next_element(line);
      string word(line + 1);

      builder.push_attr(NAME_ATTR, word);
      builder.push_attr(ARG_ATTR, p);
      builder.push_node(DIRECTIVE_NODE, end_of_line);
      break;
    }

    case '-':			// option setting
      throw_(parse_error, "Option settings are not allowed in journal files");

    case '=': {		// automated entry
      builder.begin_node(AUTO_ENTRY_NODE);
      builder.begin_node(RULE_NODE, true);
      builder.append_text(skip_ws(line + 1));
      builder.end_node(RULE_NODE);

      builder.set_position(end_of_line);

      if (! parse_transactions(in, builder))
	throw_(parse_error, "Automated entry has no transactions");

      builder.end_node(AUTO_ENTRY_NODE);
      break;
    }

    case '~':			// period entry
      builder.begin_node(PERIOD_ENTRY_NODE);
      builder.begin_node(PERIOD_NODE, true);
      builder.append_text(skip_ws(line + 1));
      builder.end_node(PERIOD_NODE);

      builder.set_position(end_of_line);

      if (! parse_transactions(in, builder))
	throw_(parse_error, "Repeating entry has no transactions");

      builder.end_node(PERIOD_ENTRY_NODE);
      break;

    default:
      TRACE_START(entries, 1, "Time spent handling entries:");
      parse_entry(in, builder, line, end_of_line);
      count++;
      TRACE_STOP(entries, 1);
      break;
    }

    //POP_CONTEXT(builder_context(builder));
  }

  builder.end_node(JOURNAL_NODE);

  TRACE_STOP(parsing_total, 1);

  return count;
}

} // namespace ledger
