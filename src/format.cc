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

#include "format.h"
#include "scope.h"

namespace ledger {

void format_t::element_t::dump(std::ostream& out) const
{
  out << "Element: ";

  switch (type) {
  case STRING: out << " STRING"; break;
  case EXPR:   out << "   EXPR"; break;
  }

  out << "  flags: 0x" << std::hex << int(flags());
  out << "  min: ";
  out << std::right;
  out.width(2);
  out << std::dec << int(min_width);
  out << "  max: ";
  out << std::right;
  out.width(2);
  out << std::dec << int(max_width);

  switch (type) {
  case STRING: out << "   str: '" << chars << "'" << std::endl; break;
  case EXPR:   out << "  expr: "   << expr << std::endl; break;
  }
}

format_t::element_t * format_t::parse_elements(const string& fmt)
{
  std::auto_ptr<element_t> result;

  element_t * current = NULL;

  char   buf[1024];
  char * q = buf;

  // The following format codes need to be implemented as functions:
  //
  //   d: COMPLETE_DATE_STRING
  //   D: DATE_STRING
  //   S: SOURCE; break
  //   B: XACT_BEG_POS
  //   b: XACT_BEG_LINE
  //   E: XACT_END_POS
  //   e: XACT_END_LINE
  //   X: CLEARED
  //   Y: XACT_CLEARED
  //   C: CODE
  //   P: PAYEE
  //   W: OPT_ACCOUNT
  //   a: ACCOUNT_NAME
  //   A: ACCOUNT_FULLNAME
  //   t: AMOUNT
  //   o: OPT_AMOUNT
  //   T: TOTAL
  //   N: NOTE
  //   n: OPT_NOTE
  //   _: DEPTH_SPACER
  //   
  //   xB: POST_BEG_POS
  //   xb: POST_BEG_LINE
  //   xE: POST_END_POS
  //   xe: POST_END_LINE

  for (const char * p = fmt.c_str(); *p; p++) {
    if (*p != '%' && *p != '\\') {
      *q++ = *p;
      continue;
    }

    if (! result.get()) {
      result.reset(new element_t);
      current = result.get();
    } else {
      current->next.reset(new element_t);
      current = current->next.get();
    }

    if (q != buf) {
      current->type  = element_t::STRING;
      current->chars = string(buf, q);
      q = buf;

      current->next.reset(new element_t);
      current = current->next.get();
    }

    if (*p == '\\') {
      p++;
      current->type = element_t::STRING;
      switch (*p) {
      case 'b': current->chars = "\b"; break;
      case 'f': current->chars = "\f"; break;
      case 'n': current->chars = "\n"; break;
      case 'r': current->chars = "\r"; break;
      case 't': current->chars = "\t"; break;
      case 'v': current->chars = "\v"; break;
      case '\\': current->chars = "\\"; break;
      default: current->chars = string(1, *p); break;
      }
      continue;
    }

    ++p;
    while (*p == '-') {
      switch (*p) {
      case '-':
	current->add_flags(ELEMENT_ALIGN_LEFT);
	break;
      }
      ++p;
    }

    int num = 0;
    while (*p && std::isdigit(*p)) {
      num *= 10;
      num += *p++ - '0';
    }
    current->min_width = num;

    if (*p == '.') {
      ++p;
      num = 0;
      while (*p && std::isdigit(*p)) {
	num *= 10;
	num += *p++ - '0';
      }
      current->max_width = num;
      if (current->min_width == 0)
	current->min_width = current->max_width;
    }

    switch (*p) {
    case '%':
      current->type  = element_t::STRING;
      current->chars = "%";
      break;

    case '(':
    case '[': {
      std::istringstream str(p);
      current->type = element_t::EXPR;
      string temp(p);
      current->expr.parse(str, expr_t::PARSE_SINGLE, &temp);
      if (str.eof()) {
	current->expr.set_text(p);
	p += std::strlen(p);
      } else {
	assert(str.good());
	istream_pos_type pos = str.tellg();
	current->expr.set_text(string(p, p + long(pos)));
	p += long(pos) - 1;

	// Don't gobble up any whitespace
	const char * base = p;
	while (p >= base && std::isspace(*p))
	  p--;
      }
      break;
    }

    default:
      current->type  = element_t::EXPR;
      current->chars = string(FMT_PREFIX) + *p;
      current->expr.parse(current->chars);
      break;
    }
  }

  if (q != buf) {
    if (! result.get()) {
      result.reset(new element_t);
      current = result.get();
    } else {
      current->next.reset(new element_t);
      current = current->next.get();
    }
    current->type  = element_t::STRING;
    current->chars = string(buf, q);
  }

  return result.release();
}

void format_t::format(std::ostream& out_str, scope_t& scope)
{
  for (element_t * elem = elements.get(); elem; elem = elem->next.get()) {
    std::ostringstream out;
    string name;

    if (elem->has_flags(ELEMENT_ALIGN_LEFT))
      out << std::left;
    else
      out << std::right;

    switch (elem->type) {
    case element_t::STRING:
      if (elem->min_width > 0)
	out.width(elem->min_width);
      out << elem->chars;
      break;

    case element_t::EXPR:
      try {
	elem->expr.compile(scope);

	value_t value;
	if (elem->expr.is_function()) {
	  call_scope_t args(scope);
	  args.push_back(long(elem->max_width));
	  value = elem->expr.get_function()(args);
	} else {
	  value = elem->expr.calc(scope);
	}
	DEBUG("format.expr", "value = (" << value << ")");

	value.print(out, elem->min_width);
      }
      catch (const calc_error&) {
	add_error_context(_("While calculating format expression:"));
	add_error_context(expr_context(elem->expr));
	throw;
      }
      break;

    default:
      assert(false);
      break;
    }

    if (elem->max_width > 0 || elem->min_width > 0) {
      unistring temp(out.str());

      string result;
      if (elem->max_width > 0 && elem->max_width < temp.length()) {
	result = truncate(temp, elem->max_width);
      } else {
	result = temp.extract();
	for (int i = 0; i < (int)elem->min_width - (int)temp.length(); i++)
	  result += " ";
      }
      out_str << result;
    } else {
      out_str << out.str();
    }
  }
}

string format_t::truncate(const unistring& ustr, std::size_t width,
			  const int account_abbrev_length)
{
  assert(width < 4095);

  const std::size_t len = ustr.length();
  if (width == 0 || len <= width)
    return ustr.extract();

  std::ostringstream buf;

  elision_style_t style = TRUNCATE_TRAILING;
  if (account_abbrev_length > 0)
    style = ABBREVIATE;

  switch (style) {
  case TRUNCATE_LEADING:
    // This method truncates at the beginning.
    buf << ".." << ustr.extract(len - width, width);
    break;

  case TRUNCATE_MIDDLE:
    // This method truncates in the middle.
    buf << ustr.extract(0, width / 2)
	<< ".."
	<< ustr.extract(len - (width / 2 + width % 2),
			width / 2 + width % 2);
    break;

  case ABBREVIATE:
    if (account_abbrev_length > 0) {
      std::list<string> parts;
      string::size_type beg = 0;
      string strcopy(ustr.extract());
      for (string::size_type pos = strcopy.find(':');
	   pos != string::npos;
	   beg = pos + 1, pos = strcopy.find(':', beg))
	parts.push_back(string(strcopy, beg, pos - beg));
      parts.push_back(string(strcopy, beg));

      std::ostringstream result;

      std::size_t newlen = len;
      for (std::list<string>::iterator i = parts.begin();
	   i != parts.end();
	   i++) {
	// Don't contract the last element
	std::list<string>::iterator x = i;
	if (++x == parts.end()) {
	  result << *i;
	  break;
	}

	if (newlen > width) {
	  unistring temp(*i);
	  if (temp.length() > static_cast<std::size_t>(account_abbrev_length)) {
	    result << temp.extract(0, account_abbrev_length) << ":";
	    newlen -= temp.length() - account_abbrev_length;
	  } else {
	    result << temp.extract() << ":";
	    newlen -= temp.length();
	  }
	} else {
	  result << *i << ":";
	}
      }

      if (newlen > width) {
	// Even abbreviated its too big to show the last account, so
	// abbreviate all but the last and truncate at the beginning.
	unistring temp(result.str());
	assert(temp.length() > width - 2);
	buf << ".." << temp.extract(temp.length() - (width - 2), width - 2);
      } else {
	buf << result.str();
      }
      break;
    }
    // fall through...

  case TRUNCATE_TRAILING:
    // This method truncates at the end (the default).
    buf << ustr.extract(0, width - 2) << "..";
    break;
  }

  return buf.str();
}

} // namespace ledger
