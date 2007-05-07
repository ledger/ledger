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

#ifndef _REGISTER_H
#define _REGISTER_H

#include "xpath.h"

namespace ledger {

class register_command : public xml::xpath_t::functor_t
{
 public:
  register_command() : xml::xpath_t::functor_t("register") {}

  virtual void operator()(value_t&, xml::xpath_t::scope_t * locals) {
    std::ostream *    out = get_ptr<std::ostream>(locals, 0);
    xml::document_t * doc = get_ptr<xml::document_t>(locals, 1);

    print_document(*out, doc);
  }

  virtual void print_document(std::ostream& out, xml::document_t * doc);
};

enum elision_style_t {
  TRUNCATE_TRAILING,
  TRUNCATE_MIDDLE,
  TRUNCATE_LEADING,
  ABBREVIATE
};

string abbreviate(const string&	  str,
		  unsigned int	  width,
		  elision_style_t elision_style = TRUNCATE_TRAILING,
		  const bool	  is_account	= false,
		  int		  abbrev_length = 2);

} // namespace ledger

#endif // _REGISTER_H
