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

#include "predicate.h"

namespace ledger {

string args_to_predicate_expr(value_t::sequence_t::const_iterator begin,
			      value_t::sequence_t::const_iterator end)
{
  std::ostringstream expr;
  bool		     append_or = false;
  bool		     only_parenthesis;

  while (begin != end) {
    string arg = (*begin).as_string();
    string prefix;

    bool parse_argument		 = true;
    bool only_closed_parenthesis = false;;

    if (arg == "not" || arg == "NOT") {
      if (append_or)
	prefix = " | ! ";
      else
	prefix = " ! ";
      parse_argument = false;
      append_or = false;
    }
    else if (arg == "and" || arg == "AND") {
      prefix = " & ";
      parse_argument = false;
      append_or = false;
    }
    else if (arg == "or" || arg == "OR") {
      prefix = " | ";
      parse_argument = false;
      append_or = false;
    }
    else if (append_or) {
      if (! only_parenthesis)
	prefix = " | ";
    }
    else {
      append_or = true;
    }

    value_t::sequence_t::const_iterator next = begin;
    if (++next != end) {
      if (arg == "desc" || arg == "DESC" ||
	  arg == "payee" || arg == "PAYEE") {
	arg = string("@") + (*++begin).as_string();
      }
      else if (arg == "code" || arg == "CODE") {
	arg = string("#") + (*++begin).as_string();
      }
      else if (arg == "note" || arg == "NOTE") {
	arg = string("&") + (*++begin).as_string();
      }
      else if (arg == "tag" || arg == "TAG" ||
	       arg == "meta" || arg == "META" ||
	       arg == "data" || arg == "DATA") {
	arg = string("%") + (*++begin).as_string();
      }
      else if (arg == "expr" || arg == "EXPR") {
	arg = string("=") + (*++begin).as_string();
      }
    }

    if (parse_argument) {
      bool in_prefix	   = true;
      bool found_specifier = false;
      bool no_final_slash  = false;

      only_parenthesis = true;

      std::ostringstream buf;
      string parens;

      for (const char * c = arg.c_str(); *c != '\0'; c++) {
	bool consumed = false;

	if (*c != '(' && *c != ')')
	  only_parenthesis = false;

	if (in_prefix) {
	  switch (*c) {
	  case ')':
	    if (only_parenthesis)
	      only_closed_parenthesis = true;
	    // fall through...
	  case '(':
	    parens += c;
	    consumed = true;
	    break;
	  case '@':
	    buf << "(payee =~ /";
	    found_specifier = true;
	    consumed = true;
	    break;
	  case '#':
	    buf << "(code =~ /";
	    found_specifier = true;
	    consumed = true;
	    break;
	  case '=':
	    buf << "(";
	    found_specifier = true;
	    no_final_slash = true;
	    consumed = true;
	    break;
	  case '&':
	    buf << "(note =~ /";
	    found_specifier = true;
	    consumed = true;
	    break;
	  case '%': {
	    bool found_metadata = false;
	    for (const char *q = c; *q != '\0'; q++)
	      if (*q == '=') {
		buf << "has_tag(/"
		     << string(c + 1, q - c - 1) << "/, /";
		found_metadata = true;
		c = q;
		break;
	      }
	    if (! found_metadata) {
	      buf << "has_tag(/";
	    }
	    found_specifier = true;
	    consumed = true;
	    break;
	  }
	  default:
	    if (! found_specifier) {
	      buf << parens << "(account =~ /";
	      parens.clear();
	      found_specifier = true;
	    }
	    in_prefix = false;
	    break;
	  }
	}

	if (! consumed)
	  buf << *c;
      }

      if (! prefix.empty() &&
	  ! (only_parenthesis && only_closed_parenthesis))
	expr << prefix;

      expr << parens << buf.str();

      if (found_specifier) {
	if (! no_final_slash)
	  expr << "/";
	expr << ")";
      }
    } else {
      expr << prefix;
    }

    begin++;
  }

  return std::string("(") + expr.str() + ")";
}

} // namespace ledger
