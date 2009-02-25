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

#include "interactive.h"

namespace ledger {

void interactive_t::verify_arguments() const
{
  value_t::sequence_t::const_iterator i;

  const char *	  p	    = spec.c_str();
  const char *	  label	    = _("unknown");
  bool		  wrong_arg = false;
  bool		  dont_skip = false;
  bool		  optional  = *p == '&';
  bool		  exit_loop = *p == '*';
  std::size_t	  offset    = 1;
  bool		  is_seq    = args.value().is_sequence();
  const value_t * next_arg  = NULL;
  string	  vlabel;

  if (is_seq) {
    i = args.begin();
    if (i != args.end())
      next_arg = &(*i);
  }
  else if (! args.value().is_null()) {
    next_arg = &args.value();
  }

  for (; ! wrong_arg && ! exit_loop && *p && next_arg; p++) {
    DEBUG("interactive.verify",
	  "Want " << *p << " got: " << next_arg->label());
    switch (*p) {
    case 'a':
      label = _("an amount");
      wrong_arg = (! next_arg->is_long() &&
		   ! next_arg->is_amount() &&
		   ! next_arg->is_balance());
      break;
    case 'b':
      label = _("a boolean");
      wrong_arg = false;	// booleans are converted
      break;
    case 'd':
      label = _("a date");
      wrong_arg = ! next_arg->is_date();
      break;
    case 'i':
    case 'l':
      label = _("an integer");
      if (next_arg->is_long() ||
	  (next_arg->is_amount() &&
	   ! next_arg->as_amount().has_commodity())) {
	wrong_arg = false;
      }
      else if (next_arg->is_string()) {
	wrong_arg = false;
	for (const char * q = next_arg->as_string().c_str(); *q; q++) {
	  if (! std::isdigit(*q) && *q != '-') {
	    wrong_arg = true;
	    break;
	  }
	}
      }
      else {
	wrong_arg = true;
      }
      break;
    case 'm':
      label = _("a regex");
      wrong_arg = ! next_arg->is_mask();
      break;
    case 's':
      label = _("a string");
      wrong_arg = ! next_arg->is_string();
      break;
    case 't':
      label = _("a date or time");
      wrong_arg = (! next_arg->is_date() &&
		   ! next_arg->is_datetime());
      break;
    case 'v':
      label = _("any value");
      wrong_arg = false;
      break;
    case 'P':
      label = _("a pointer");
      wrong_arg = ! next_arg->is_pointer();
      break;
    case 'S':
      label = _("a sequence");
      wrong_arg = false;
      break;
    case '&':
      optional = true;
      dont_skip = true;
      break;
    case '*':
      optional  = true;
      exit_loop = true;
      dont_skip = true;
      break;
    }

    if (wrong_arg)
      vlabel = next_arg->label();

    if (! dont_skip) {
      if (is_seq) {
	if (++i != args.end()) {
	  next_arg = &(*i);
	  offset++;
	} else {
	  next_arg = NULL;
	}
      } else {
	next_arg = NULL;
      }
    }
    dont_skip = false;
  }

  if (*p == '&' || *p == '*')
    optional = true;

  DEBUG("interactive.verify", "Remaining args are optional");

  if (wrong_arg) {
    throw_(std::logic_error,
	   _("Expected %1 for argument %2, but received %3")
	   << label << offset << vlabel);
  }
  else if (*p && ! optional && ! next_arg) {
    throw_(std::logic_error, _("Too few arguments to function"));
  }
  else if (! *p && next_arg) {
    throw_(std::logic_error, _("Too many arguments to function"));
  }
}

string join_args(call_scope_t& args)
{
  std::ostringstream buf;
  bool first = true;

  for (std::size_t i = 0; i < args.size(); i++) {
    if (first) {
      buf << args[i];
      first = false;
    } else {
      buf << ' ' << args[i];
    }
  }

  return buf.str();
}

} // namespace ledger
