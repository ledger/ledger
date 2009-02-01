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
  bool		     append_and = false;
  bool		     only_parenthesis;

  while (begin != end) {
    string	 arg		= (*begin).as_string();
    bool	 parse_argument = true;

    if (arg == "not" || arg == "NOT") {
      expr << " ! ";
      parse_argument = false;
      append_and = false;
    }
    else if (arg == "and" || arg == "AND") {
      expr << " & ";
      parse_argument = false;
      append_and = false;
    }
    else if (arg == "or" || arg == "OR") {
      expr << " | ";
      parse_argument = false;
      append_and = false;
    }
    else if (append_and) {
      if (! only_parenthesis)
	expr << " & ";
    }
    else {
      append_and = true;
    }

    if (arg == "desc" || arg == "DESC" ||
	arg == "payee" || arg == "PAYEE") {
      arg = string("@") + (*++begin).as_string();
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

    if (parse_argument) {
      bool in_prefix	   = true;
      bool in_suffix	   = false;
      bool found_specifier = false;
      bool saw_tag_char    = false;
      bool no_final_slash  = false;

      only_parenthesis = true;

      for (const char * c = arg.c_str(); *c != '\0'; c++) {
	bool consumed = false;

	if (*c != '(' && *c != ')')
	  only_parenthesis = false;

	if (in_prefix) {
	  switch (*c) {
	  case '(':
	    break;
	  case '@':
	    expr << "(payee =~ /";
	    found_specifier = true;
	    consumed = true;
	    break;
	  case '=':
	    expr << "(";
	    found_specifier = true;
	    no_final_slash = true;
	    consumed = true;
	    break;
	  case '&':
	    expr << "(note =~ /";
	    found_specifier = true;
	    consumed = true;
	    break;
	  case '%': {
	    bool found_metadata = false;
	    for (const char *q = c; *q != '\0'; q++)
	      if (*q == '=') {
		expr << "(metadata(\""
		     << string(c + 1, q - c - 1) << "\") =~ /";
		found_metadata = true;
		c = q;
		break;
	      }
	    if (! found_metadata) {
	      expr << "(tag =~ /:";
	      saw_tag_char = true;
	    }
	    found_specifier = true;
	    consumed = true;
	    break;
	  }
	  case '/':
	  case '_':
	  default:
	    if (! found_specifier) {
	      expr << "(account =~ /";
	      found_specifier = true;
	    }
	    in_prefix = false;
	    break;
	  }
	} else {
	  switch (*c) {
	  case ')':
	    if (! in_suffix) {
	      if (found_specifier) {
		if (saw_tag_char)
		  expr << ':';
		expr << "/)";
	      }
	      in_suffix = true;
	    }
	    break;
	  default:
	    if (in_suffix)
	      throw_(parse_error, "Invalid text in specification argument");
	    break;
	  }
	}

	if (! consumed)
	  expr << *c;
      }

      if (! in_suffix) {
	if (found_specifier) {
	  if (saw_tag_char)
	    expr << ':';
	  if (! no_final_slash)
	    expr << "/";
	  expr << ")";
	}
      }
    }

    begin++;
  }

  return expr.str();
}

} // namespace ledger
