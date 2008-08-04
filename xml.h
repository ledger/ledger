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

#ifndef _XML_H
#define _XML_H

#include "journal.h"
#include "report.h"
#include "output.h"

namespace ledger {

#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)

class xml_parser_t : public journal_t::parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream& in,
			     session_t&     session,
			     journal_t&   journal,
			     account_t *   master        = NULL,
			     const path *  original_file = NULL);
};

#endif

class format_xml_entries : public format_entries
{
  bool show_totals;

  format_xml_entries();

public:
  format_xml_entries(report_t& _report,
		     const bool _show_totals = false)
    : format_entries(_report, ""), show_totals(_show_totals) {
    TRACE_CTOR(format_xml_entries, "std::ostream&, const bool");
    *report.output_stream << "<?xml version=\"1.0\"?>\n"
			  << "<ledger version=\"2.5\">\n";
  }
  virtual ~format_xml_entries() throw() {
    TRACE_DTOR(format_xml_entries);
  }

  virtual void flush() {
    format_entries::flush();
    *report.output_stream << "</ledger>" << std::endl;
  }

  virtual void format_last_entry();
};

} // namespace ledger

#endif // _XML_H
