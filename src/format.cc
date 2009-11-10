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

#include <system.hh>

#include "format.h"
#include "scope.h"
#include "unistring.h"
#include "pstream.h"

namespace ledger {

format_t::elision_style_t format_t::default_style	  = TRUNCATE_TRAILING;
bool			  format_t::default_style_changed = false;

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
  case STRING:
    out << "   str: '" << boost::get<string>(data) << "'" << std::endl;
    break;
  case EXPR:
    out << "  expr: "   << boost::get<expr_t>(data) << std::endl;
    break;
  }
}

namespace {
  expr_t parse_single_expression(const char *& p, bool single_expr = true)
  {
    string temp(p);
    ptristream str(const_cast<char *&>(p));
    expr_t expr;
    expr.parse(str, single_expr ? PARSE_SINGLE : PARSE_PARTIAL, temp);
    if (str.eof()) {
      expr.set_text(p);
      p += std::strlen(p);
    } else {
      assert(str.good());
      istream_pos_type pos = str.tellg();
      expr.set_text(string(p, p + long(pos)));
      p += long(pos) - 1;

      // Don't gobble up any whitespace
      const char * base = p;
      while (p >= base && std::isspace(*p))
	p--;
    }
    return expr;
  }
}

format_t::element_t * format_t::parse_elements(const string& fmt,
					       const optional<format_t&>& tmpl)
{
  std::auto_ptr<element_t> result;

  element_t * current = NULL;

  char   buf[1024];
  char * q = buf;

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
      current->data = string(buf, q);
      q = buf;

      current->next.reset(new element_t);
      current = current->next.get();
    }

    if (*p == '\\') {
      p++;
      current->type = element_t::STRING;
      switch (*p) {
      case 'b':  current->data = string("\b");  break;
      case 'f':  current->data = string("\f");  break;
      case 'n':  current->data = string("\n");  break;
      case 'r':  current->data = string("\r");  break;
      case 't':  current->data = string("\t");  break;
      case 'v':  current->data = string("\v");  break;
      case '\\': current->data = string("\\");  break;
      default:   current->data = string(1, *p); break;
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

    std::size_t num = 0;
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
      current->type = element_t::STRING;
      current->data = string("%");
      break;

    case '$': {
      if (! tmpl)
	throw_(format_error, _("Prior field reference, but no template"));

      p++;
      if (*p == '0' || (! std::isdigit(*p) &&
			*p != 'A' && *p != 'B' && *p != 'C' &&
			*p != 'D' && *p != 'E' && *p != 'F'))
	throw_(format_error, _("%$ field reference must be a digit from 1-9"));

      unsigned int index     = std::isdigit(*p) ? *p - '0' : (*p - 'A' + 10);
      element_t *  tmpl_elem = tmpl->elements.get();

      for (unsigned int i = 1; i < index && tmpl_elem; i++) {
	tmpl_elem = tmpl_elem->next.get();
	while (tmpl_elem && tmpl_elem->type != element_t::EXPR)
	  tmpl_elem = tmpl_elem->next.get();
      }

      if (! tmpl_elem)
	throw_(format_error, _("%$ reference to a non-existent prior field"));

      *current = *tmpl_elem;
      break;
    }

    case '(':
    case '{': {
      bool format_amount = *p == '{';
      if (format_amount) p++;

      current->type = element_t::EXPR;
      current->data = parse_single_expression(p, ! format_amount);

      // Wrap the subexpression in calls to justify and scrub
      if (format_amount) {
	if (! *p || *(p + 1) != '}')
	  throw_(format_error, _("Expected closing brace"));
	else
	  p++;

	expr_t::ptr_op_t op = boost::get<expr_t>(current->data).get_op();

	expr_t::ptr_op_t amount_op;
	expr_t::ptr_op_t colorize_op;
	if (op->kind == expr_t::op_t::O_CONS) {
	  amount_op   = op->left();
	  colorize_op = op->right();
	} else {
	  amount_op = op;
	}

	expr_t::ptr_op_t scrub_node(new expr_t::op_t(expr_t::op_t::IDENT));
	scrub_node->set_ident("scrub");

	expr_t::ptr_op_t call1_node(new expr_t::op_t(expr_t::op_t::O_CALL));
	call1_node->set_left(scrub_node);
	call1_node->set_right(amount_op);

	expr_t::ptr_op_t arg1_node(new expr_t::op_t(expr_t::op_t::VALUE));
	expr_t::ptr_op_t arg2_node(new expr_t::op_t(expr_t::op_t::VALUE));
	expr_t::ptr_op_t arg3_node(new expr_t::op_t(expr_t::op_t::VALUE));

	arg1_node->set_value(current->min_width > 0 ?
			     long(current->min_width) : -1);
	arg2_node->set_value(current->max_width > 0 ?
			     long(current->max_width) : -1);
	arg3_node->set_value(! current->has_flags(ELEMENT_ALIGN_LEFT));

	current->min_width = 0;
	current->max_width = 0;

	expr_t::ptr_op_t args1_node(new expr_t::op_t(expr_t::op_t::O_CONS));
	args1_node->set_left(arg2_node);
	args1_node->set_right(arg3_node);

	expr_t::ptr_op_t args2_node(new expr_t::op_t(expr_t::op_t::O_CONS));
	args2_node->set_left(arg1_node);
	args2_node->set_right(args1_node);

	expr_t::ptr_op_t args3_node(new expr_t::op_t(expr_t::op_t::O_CONS));
	args3_node->set_left(call1_node);
	args3_node->set_right(args2_node);

	expr_t::ptr_op_t seq1_node(new expr_t::op_t(expr_t::op_t::O_SEQ));
	seq1_node->set_left(args3_node);

	expr_t::ptr_op_t justify_node(new expr_t::op_t(expr_t::op_t::IDENT));
	justify_node->set_ident("justify");

	expr_t::ptr_op_t call2_node(new expr_t::op_t(expr_t::op_t::O_CALL));
	call2_node->set_left(justify_node);
	call2_node->set_right(seq1_node);

	string prev_expr = boost::get<expr_t>(current->data).text();

	if (colorize_op) {
	  expr_t::ptr_op_t ansify_if_node(new expr_t::op_t(expr_t::op_t::IDENT));
	  ansify_if_node->set_ident("ansify_if");

	  expr_t::ptr_op_t args4_node(new expr_t::op_t(expr_t::op_t::O_CONS));
	  args4_node->set_left(call2_node);
	  args4_node->set_right(colorize_op);

	  expr_t::ptr_op_t seq2_node(new expr_t::op_t(expr_t::op_t::O_SEQ));
	  seq2_node->set_left(args4_node);

	  expr_t::ptr_op_t call3_node(new expr_t::op_t(expr_t::op_t::O_CALL));
	  call3_node->set_left(ansify_if_node);
	  call3_node->set_right(seq2_node);

	  current->data = expr_t(call3_node);
	} else {
	  current->data = expr_t(call2_node);
	}

	boost::get<expr_t>(current->data).set_text(prev_expr);
      }
      break;
    }

    default:
      throw_(format_error, _("Unrecognized formatting character: %1") << *p);
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
    current->type = element_t::STRING;
    current->data = string(buf, q);
  }

  return result.release();
}

string format_t::real_calc(scope_t& scope)
{
  std::ostringstream out_str;

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
      out << boost::get<string>(elem->data);
      break;

    case element_t::EXPR: {
      expr_t& expr(boost::get<expr_t>(elem->data));
      try {

	expr.compile(scope);

	value_t value;
	if (expr.is_function()) {
	  call_scope_t args(scope);
	  args.push_back(long(elem->max_width));
	  value = expr.get_function()(args);
	} else {
	  value = expr.calc(scope);
	}
	DEBUG("format.expr", "value = (" << value << ")");

	value.print(out, static_cast<int>(elem->min_width), -1,
		    ! elem->has_flags(ELEMENT_ALIGN_LEFT));
      }
      catch (const calc_error&) {
	add_error_context(_("While calculating format expression:"));
	add_error_context(expr.context_to_str());
	throw;
      }
      break;
    }

    default:
      assert(false);
      break;
    }

    if (elem->max_width > 0 || elem->min_width > 0) {
      unistring temp(out.str());
      string	result;

      if (elem->max_width > 0 && elem->max_width < temp.length()) {
	result = truncate(temp, elem->max_width);
      } else {
	result = temp.extract();
	if (elem->min_width > temp.length())
	  for (std::size_t i = 0; i < elem->min_width - temp.length(); i++)
	    result += " ";
      }
      out_str << result;
    } else {
      out_str << out.str();
    }
  }

  return out_str.str();
}

string format_t::truncate(const unistring&  ustr,
			  const std::size_t width,
			  const std::size_t account_abbrev_length)
{
  assert(width < 4095);

  const std::size_t len = ustr.length();
  if (width == 0 || len <= width)
    return ustr.extract();

  std::ostringstream buf;

  elision_style_t style = default_style;
  if (account_abbrev_length > 0 && ! default_style_changed)
    style = ABBREVIATE;

  switch (style) {
  case TRUNCATE_LEADING:
    // This method truncates at the beginning.
    buf << ".." << ustr.extract(len - (width - 2), width - 2);
    break;

  case TRUNCATE_MIDDLE:
    // This method truncates in the middle.
    buf << ustr.extract(0, (width - 2) / 2)
	<< ".."
	<< ustr.extract(len - ((width - 2) / 2 + (width - 2) % 2),
			(width - 2) / 2 + (width - 2) % 2);
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
	  if (temp.length() > account_abbrev_length) {
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
