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

#include "option.h"

#if 0
#ifdef USE_BOOST_PYTHON
static ledger::option_t * find_option(const string& name);
#endif
#endif

namespace ledger {

namespace {
  xml::xpath_t::ptr_op_t find_option(xml::xpath_t::scope_t * scope,
				     const string& name)
  {
    char buf[128];
    std::strcpy(buf, "option_");
    char * p = &buf[7];
    for (const char * q = name.c_str(); *q; q++) {
      if (*q == '-')
	*p++ = '_';
      else
	*p++ = *q;
    }
    *p = '\0';

    return scope->lookup(buf);
  }

  xml::xpath_t::ptr_op_t find_option(xml::xpath_t::scope_t * scope,
				     const char letter)
  {
    char buf[9];
    std::strcpy(buf, "option_");
    buf[7] = letter;
    buf[8] = '\0';

    return scope->lookup(buf);
  }

  void process_option(const xml::xpath_t::function_t& opt,
		      xml::xpath_t::scope_t * scope, const char * arg)
  {
#if 0
    try {
#endif
      scoped_ptr<xml::xpath_t::scope_t> args;
      if (arg) {
	args.reset(new xml::xpath_t::scope_t(scope, xml::xpath_t::scope_t::ARGUMENT));
	args->args.set_string(arg);
      }

      value_t temp;
      opt(temp, args.get());
#if 0
    }
    catch (error * err) {
      err->context.push_back
	(new error_context
	 (string("While parsing option '--") + opt->long_opt +
	  "'" + (opt->short_opt != '\0' ?
		 (string(" (-") + opt->short_opt + "):") : ":")));
      throw err;
    }
#endif
  }
}

bool process_option(const string& name, xml::xpath_t::scope_t * scope,
		    const char * arg)
{
  xml::xpath_t::ptr_op_t opt(find_option(scope, name));
  if (opt) {
    process_option(opt->as_function(), scope, arg);
    return true;
  }
  return false;
}

void process_environment(const char ** envp, const string& tag,
			 xml::xpath_t::scope_t * scope)
{
  const char * tag_p   = tag.c_str();
  unsigned int tag_len = tag.length();

  for (const char ** p = envp; *p; p++)
    if (! tag_p || std::strncmp(*p, tag_p, tag_len) == 0) {
      char   buf[128];
      char * r = buf;
      const char * q;
      for (q = *p + tag_len;
	   *q && *q != '=' && r - buf < 128;
	   q++)
	if (*q == '_')
	  *r++ = '-';
	else
	  *r++ = std::tolower(*q);
      *r = '\0';

      if (*q == '=') {
#if 0
	try {
#endif
	  if (! process_option(string(buf), scope, q + 1))
#if 0
	    throw new option_error("unknown option")
#endif
	      ;
#if 0
	}
	catch (error * err) {
	  err->context.push_back
	    (new error_context
	     (string("While parsing environment variable option '") +
	      *p + "':"));
	  throw err;
	}
#endif
      }
    }
}

void process_arguments(int argc, char ** argv, const bool anywhere,
		       xml::xpath_t::scope_t * scope,
		       std::list<string>& args)
{
  for (char ** i = argv; *i; i++) {
    if ((*i)[0] != '-') {
      if (anywhere) {
	args.push_back(*i);
	continue;
      } else {
	for (; *i; i++)
	  args.push_back(*i);
	break;
      }
    }

    // --long-option or -s
    if ((*i)[1] == '-') {
      if ((*i)[2] == '\0')
	break;

      char * name  = *i + 2;
      char * value = NULL;
      if (char * p = std::strchr(name, '=')) {
	*p++ = '\0';
	value = p;
      }

      xml::xpath_t::ptr_op_t opt(find_option(scope, name));
      if (! opt)
	throw_(option_error, "illegal option --" << name);

      if (/*def->wants_args &&*/ value == NULL) {
	value = *++i;
	if (value == NULL)
	  throw_(option_error, "missing option argument for --" << name);
      }
      process_option(opt->as_function(), scope, value);
    }
    else if ((*i)[1] == '\0') {
      throw_(option_error, "illegal option -");
    }
    else {
      std::list<xml::xpath_t::ptr_op_t> option_queue;

      int x = 1;
      for (char c = (*i)[x]; c != '\0'; x++, c = (*i)[x]) {
	xml::xpath_t::ptr_op_t opt = find_option(scope, c);
	if (! opt)
	  throw_(option_error, "illegal option -" << c);

	option_queue.push_back(opt);
      }

      foreach (xml::xpath_t::ptr_op_t& o, option_queue) {
	char * value = NULL;
#if 0
	if (def->wants_args) {
#endif
	  value = *++i;
	  if (value == NULL)
	    throw_(option_error, "missing option argument for -" <<
#if 0
		   def->short_opt
#else
		   '?'
#endif
		   );
#if 0
	}
#endif
	process_option(o->as_function(), scope, value);

      }
    }
  }
}

} // namespace ledger
