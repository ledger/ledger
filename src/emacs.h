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

/**
 * @addtogroup parse
 */

/**
 * @file   emacs.h
 * @author John Wiegley
 *
 * @ingroup parse
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _EMACS_H
#define _EMACS_H

#include "handler.h"
#include "format.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class format_emacs_xacts : public item_handler<xact_t>
{
  format_emacs_xacts();

protected:
  std::ostream& out;
  entry_t *     last_entry;

public:
  format_emacs_xacts(std::ostream& _out)
    : out(_out), last_entry(NULL) {
    TRACE_CTOR(format_emacs_xacts, "std::ostream&");
  }
  ~format_emacs_xacts() {
    TRACE_DTOR(format_emacs_xacts);
  }

  virtual void write_entry(entry_t& entry);
  virtual void flush() {
    if (last_entry)
      out << "))\n";
    out.flush();
  }
  virtual void operator()(xact_t& xact);
};

} // namespace ledger

#endif // _REPORT_H
