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

#include "xml.h"
#include "journal.h"
#include "utils.h"

namespace ledger {

#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

static XML_Parser    current_parser;
static unsigned int  count;

static journal_t *   curr_journal;
static entry_t *     curr_entry;
static commodity_t * curr_comm;
static string	     comm_flags;

static xact_t::state_t curr_state;

static string data;
static bool   ignore;
static string have_error;

static void startElement(void *userData, const char *name, const char **attrs)
{
  if (ignore)
    return;

  if (std::strcmp(name, "entry") == 0) {
    assert(! curr_entry);
    curr_entry = new entry_t;
    curr_state = xact_t::UNCLEARED;
  }
  else if (std::strcmp(name, "xact") == 0) {
    assert(curr_entry);
    curr_entry->add_xact(new xact_t);
    if (curr_state != item_t::UNCLEARED)
      curr_entry->xacts.back()->set_state(curr_state);
  }
  else if (std::strcmp(name, "commodity") == 0) {
    if (string(attrs[0]) == "flags")
      comm_flags = attrs[1];
  }
  else if (std::strcmp(name, "total") == 0) {
    ignore = true;
  }
}

static void endElement(void *userData, const char *name)
{
  if (ignore) {
    if (std::strcmp(name, "total") == 0)
      ignore = false;
    return;
  }

  if (std::strcmp(name, "entry") == 0) {
    assert(curr_entry);
    if (curr_journal->add_entry(curr_entry)) {
      count++;
    } else {
      account_t * acct = curr_journal->find_account("<Unknown>");
      curr_entry->add_xact(new xact_t(acct));
      if (curr_journal->add_entry(curr_entry)) {
	count++;
      } else {
	checked_delete(curr_entry);
	have_error = "Entry cannot be balanced";
      }
    }
    curr_entry = NULL;
  }
  else if (std::strcmp(name, "en:date") == 0) {
    curr_entry->_date = parse_date(data);
  }
  else if (std::strcmp(name, "en:date_eff") == 0) {
    curr_entry->_date_eff = parse_date(data);
  }
  else if (std::strcmp(name, "en:code") == 0) {
    curr_entry->code = data;
  }
  else if (std::strcmp(name, "en:cleared") == 0) {
    curr_state = xact_t::CLEARED;
  }
  else if (std::strcmp(name, "en:pending") == 0) {
    curr_state = xact_t::PENDING;
  }
  else if (std::strcmp(name, "en:payee") == 0) {
    curr_entry->payee = data;
  }
  else if (std::strcmp(name, "tr:account") == 0) {
    curr_entry->xacts.back()->account = curr_journal->find_account(data);
  }
  else if (std::strcmp(name, "tr:cleared") == 0) {
    curr_entry->xacts.back()->set_state(item_t::CLEARED);
  }
  else if (std::strcmp(name, "tr:pending") == 0) {
    curr_entry->xacts.back()->set_state(item_t::PENDING);
  }
  else if (std::strcmp(name, "tr:virtual") == 0) {
    curr_entry->xacts.back()->add_flags(XACT_VIRTUAL);
  }
  else if (std::strcmp(name, "tr:generated") == 0) {
    curr_entry->xacts.back()->add_flags(XACT_AUTO);
  }
  else if (std::strcmp(name, "symbol") == 0) {
    assert(! curr_comm);
    curr_comm = amount_t::current_pool->find_or_create(data);
    assert(curr_comm);
    curr_comm->add_flags(COMMODITY_STYLE_SUFFIXED);
    if (! comm_flags.empty()) {
      for (string::size_type i = 0, l = comm_flags.length(); i < l; i++) {
	switch (comm_flags[i]) {
	case 'P': curr_comm->drop_flags(COMMODITY_STYLE_SUFFIXED); break;
	case 'S': curr_comm->add_flags(COMMODITY_STYLE_SEPARATED); break;
	case 'T': curr_comm->add_flags(COMMODITY_STYLE_THOUSANDS); break;
	case 'E': curr_comm->add_flags(COMMODITY_STYLE_EUROPEAN); break;
	}
      }
    }
  }
#if 0
  // jww (2006-03-02): !!!
  else if (std::strcmp(name, "price") == 0) {
    assert(curr_comm);
    amount_t * price = new amount_t(data);
    std::ostringstream symstr;
    symstr << curr_comm->symbol << " {" << *price << "}";
    commodity_t * priced_comm =
      commodity_t::find_commodity(symstr.str(), true);
    priced_comm->price = price;
    priced_comm->base = curr_comm;
    curr_comm = priced_comm;
  }
#endif
  else if (std::strcmp(name, "quantity") == 0) {
    curr_entry->xacts.back()->amount.parse(data);
    if (curr_comm) {
      string::size_type i = data.find('.');
      if (i != string::npos) {
	int precision = data.length() - i - 1;
	if (precision > curr_comm->precision())
	  curr_comm->set_precision(precision);
      }
      curr_entry->xacts.back()->amount.set_commodity(*curr_comm);
      curr_comm = NULL;
    }
  }
  else if (std::strcmp(name, "tr:amount") == 0) {
    curr_comm = NULL;
  }
}

static void dataHandler(void *userData, const char *s, int len)
{
  if (! ignore)
    data = string(s, len);
}

bool xml_parser_t::test(std::istream& in) const
{
  char buf[80];

  in.getline(buf, 79);
  if (std::strncmp(buf, "<?xml", 5) != 0) {
    in.clear();
    in.seekg(0, std::ios::beg);
    return false;
  }

  in.getline(buf, 79);
  if (! std::strstr(buf, "<ledger")) {
    in.clear();
    in.seekg(0, std::ios::beg);
    return false;
  }

  in.clear();
  in.seekg(0, std::ios::beg);
  return true;
}

unsigned int xml_parser_t::parse(std::istream& in,
				 session_t&     session,
				 journal_t&   journal,
				 account_t *   master,
				 const path *  original_file)
{
  char buf[BUFSIZ];

  count        = 0;
  curr_journal = &journal;
  curr_entry   = NULL;
  curr_comm    = NULL;
  ignore       = false;

  XML_Parser   parser = XML_ParserCreate(NULL);
  current_parser = parser;

  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  while (! in.eof()) {
    in.getline(buf, BUFSIZ - 1);
    std::strcat(buf, "\n");
    bool result;
    try {
      result = XML_Parse(parser, buf, std::strlen(buf), in.eof());
    }
    catch (const std::exception& err) {
      //unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      XML_ParserFree(parser);
      throw parse_error(err.what());
    }

    if (! have_error.empty()) {
      //unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      parse_error err(have_error);
      std::cerr << "Error: " << err.what() << std::endl;
      have_error = "";
    }

    if (! result) {
      //unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      const char *  err  = XML_ErrorString(XML_GetErrorCode(parser));
      XML_ParserFree(parser);
      throw parse_error(err);
    }
  }

  XML_ParserFree(parser);

  return count;
}

#endif // defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

void xml_write_amount(std::ostream& out, const amount_t& amount,
		      const int depth = 0)
{
  for (int i = 0; i < depth; i++) out << ' ';
  out << "<amount>\n";

  commodity_t& c = amount.commodity();
  for (int i = 0; i < depth + 2; i++) out << ' ';
  out << "<commodity flags=\"";
  if (! (c.flags() & COMMODITY_STYLE_SUFFIXED)) out << 'P';
  if (c.flags() & COMMODITY_STYLE_SEPARATED)    out << 'S';
  if (c.flags() & COMMODITY_STYLE_THOUSANDS)    out << 'T';
  if (c.flags() & COMMODITY_STYLE_EUROPEAN)     out << 'E';
  out << "\">\n";
  for (int i = 0; i < depth + 4; i++) out << ' ';
#if 0
  // jww (2006-03-02): !!!
  if (c.price) {
    out << "<symbol>" << c.base->symbol << "</symbol>\n";
    for (int i = 0; i < depth + 4; i++) out << ' ';
    out << "<price>\n";
    xml_write_amount(out, *c.price, depth + 6);
    for (int i = 0; i < depth + 4; i++) out << ' ';
    out << "</price>\n";
  } else {
    out << "<symbol>" << c.symbol << "</symbol>\n";
  }
#endif
  for (int i = 0; i < depth + 2; i++) out << ' ';
  out << "</commodity>\n";

  for (int i = 0; i < depth + 2; i++) out << ' ';
  out << "<quantity>";
  out << amount.quantity_string() << "</quantity>\n";

  for (int i = 0; i < depth; i++) out << ' ';
  out << "</amount>\n";
}

void xml_write_value(std::ostream& out, const value_t& value,
		     const int depth = 0)
{
  const balance_t * bal = NULL;

  for (int i = 0; i < depth; i++) out << ' ';
  out << "<value type=\"";
  switch (value.type()) {
  case value_t::BOOLEAN: out << "boolean"; break;
  case value_t::INTEGER: out << "integer"; break;
  case value_t::AMOUNT: out << "amount"; break;
  case value_t::BALANCE:
  case value_t::BALANCE_PAIR: out << "balance"; break;
  default:
    assert(false);
    break;
  }
  out << "\">\n";

  switch (value.type()) {
  case value_t::BOOLEAN:
    for (int i = 0; i < depth + 2; i++) out << ' ';
    out << "<boolean>" << value.as_boolean() << "</boolean>\n";
    break;

  case value_t::INTEGER:
    for (int i = 0; i < depth + 2; i++) out << ' ';
    out << "<integer>" << value.as_long() << "</integer>\n";
    break;

  case value_t::AMOUNT:
    xml_write_amount(out, value.as_amount(), depth + 2);
    break;

  case value_t::BALANCE:
    bal = &(value.as_balance());
    // fall through...

  case value_t::BALANCE_PAIR:
    if (! bal)
      bal = &(value.as_balance_pair().quantity());

    for (int i = 0; i < depth + 2; i++) out << ' ';
    out << "<balance>\n";

    foreach (const balance_t::amounts_map::value_type& pair, bal->amounts)
      xml_write_amount(out, pair.second, depth + 4);

    for (int i = 0; i < depth + 2; i++) out << ' ';
    out << "</balance>\n";
    break;

  default:
    assert(false);
    break;
  }

  for (int i = 0; i < depth; i++) out << ' ';
  out << "</value>\n";
}

void output_xml_string(std::ostream& out, const string& str)
{
  for (const char * s = str.c_str(); *s; s++) {
    switch (*s) {
    case '<':
      out << "&lt;";
      break;
    case '>':
      out << "&rt;";
      break;
    case '&':
      out << "&amp;";
      break;
    default:
      out << *s;
      break;
    }
  }
}

void format_xml_entries::format_last_entry()
{
  std::ostream& out(*report.output_stream);

#if 0
  // jww (2008-05-08): Need to format these dates
  out << "  <entry>\n"
      << "    <en:date>" << last_entry->_date.to_string("%Y/%m/%d")
      << "</en:date>\n";

  if (is_valid(last_entry->_date_eff))
    out << "    <en:date_eff>"
	<< last_entry->_date_eff.to_string("%Y/%m/%d")
	<< "</en:date_eff>\n";
#endif

  if (last_entry->code) {
    out << "    <en:code>";
    output_xml_string(out, *last_entry->code);
    out << "</en:code>\n";
  }

  if (! last_entry->payee.empty()) {
    out << "    <en:payee>";
    output_xml_string(out, last_entry->payee);
    out << "</en:payee>\n";
  }

  bool first = true;
  foreach (xact_t * xact, last_entry->xacts) {
    if (xact->has_xdata() &&
	xact->xdata().has_flags(XACT_EXT_TO_DISPLAY)) {
      if (first) {
	out << "    <en:xacts>\n";
	first = false;
      }

      out << "      <xact>\n";

#if 0
      // jww (2008-05-08): Need to format these
      if (xact->_date)
	out << "        <tr:date>"
	    << xact->_date.to_string("%Y/%m/%d")
	    << "</tr:date>\n";

      if (is_valid(xact->_date_eff))
	out << "        <tr:date_eff>"
	    << xact->_date_eff.to_string("%Y/%m/%d")
	    << "</tr:date_eff>\n";
#endif

      if (xact->state() == item_t::CLEARED)
	out << "        <tr:cleared/>\n";
      else if (xact->state() == xact_t::PENDING)
	out << "        <tr:pending/>\n";

      if (xact->has_flags(XACT_VIRTUAL))
	out << "        <tr:virtual/>\n";
      if (xact->has_flags(XACT_AUTO))
	out << "        <tr:generated/>\n";

      if (xact->account) {
	string name = xact->account->fullname();
	if (name == "<Total>")
	  name = "[TOTAL]";
	else if (name == "<Unknown>")
	  name = "[UNKNOWN]";

	out << "        <tr:account>";
	output_xml_string(out, name);
	out << "</tr:account>\n";
      }

      out << "        <tr:amount>\n";
      if (xact->xdata().has_flags(XACT_EXT_COMPOUND))
	xml_write_value(out, xact->xdata().value, 10);
      else
	xml_write_value(out, value_t(xact->amount), 10);
      out << "        </tr:amount>\n";

      if (xact->cost) {
	out << "        <tr:cost>\n";
	xml_write_value(out, value_t(*xact->cost), 10);
	out << "        </tr:cost>\n";
      }

      if (xact->note) {
	out << "        <tr:note>";
	output_xml_string(out, *xact->note);
	out << "</tr:note>\n";
      }

      if (show_totals) {
	out << "        <total>\n";
	xml_write_value(out, xact->xdata().total, 10);
	out << "        </total>\n";
      }

      out << "      </xact>\n";

      xact->xdata().add_flags(XACT_EXT_DISPLAYED);
    }
  }

  if (! first)
    out << "    </en:xacts>\n";

  out << "  </entry>\n";
}

} // namespace ledger
