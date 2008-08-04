/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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

#ifndef _TEXTUAL_H
#define _TEXTUAL_H

#include "journal.h"
#include "format.h"
#include "walk.h"

namespace ledger {

class textual_parser_t : public journal_t::parser_t
{
public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream& in,
			     session_t&    session,
			     journal_t&    journal,
			     account_t *   master        = NULL,
			     const path *  original_file = NULL);
};

xact_t * parse_xact_text(char * line, account_t * account);
xact_t * parse_xact(std::istream& in, account_t * account);

void write_textual_journal(journal_t&	     journal,
			   const path&	     pathname,
			   xact_handler_ptr& formatter,
			   const string&     write_hdr_format,
			   std::ostream&     out);

#if 0
class include_context : public file_context
{
 public:
  include_context(const path& file, unsigned long line,
		  const string& desc = "") throw()
    : file_context(file, line, desc) {}
  virtual ~include_context() throw() {}

  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << ": ";
    out << "\"" << file.string() << "\", line " << line << ":"
	<< std::endl;
  }
};
#endif

} // namespace ledger

#endif // _TEXTUAL_H
