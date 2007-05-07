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

#ifndef _FORMAT_H
#define _FORMAT_H

#include "xpath.h"

namespace ledger {

class format_t
{
 public:
  struct element_t
  {
    bool	align_left;
    short	min_width;
    short	max_width;

    enum kind_t { UNKNOWN, TEXT, COLUMN, XPATH, GROUP } kind;
    union {
      string *  chars;
      xml::xpath_t * xpath;
      format_t *     format;
    };

    element_t()
      : align_left(false), min_width(-1), max_width(-1),
	kind(UNKNOWN), chars(NULL) {
      TRACE_CTOR(element_t, "");
    }

    ~element_t() {
      TRACE_DTOR(element_t);

      switch (kind) {
      case TEXT:
	checked_delete(chars);
	break;
      case XPATH:
	checked_delete(xpath);
	break;
      case GROUP:
	checked_delete(format);
	break;
      default:
	assert(! chars);
	break;
      }
    }

   private:
    element_t(const element_t& other);
  };

  struct element_formatter_t {
    virtual ~element_formatter_t() {}
    virtual int operator()(std::ostream& out, element_t * element,
			   xml::node_t * context, int column) const;
  };

  string		 format_string;
  std::list<element_t *> elements;

 private:
  format_t(const format_t&);

 public:
  format_t() {
    TRACE_CTOR(format_t, "");
  }
  format_t(const string& fmt) {
    TRACE_CTOR(format_t, "const string&");
    parse(fmt);
  }

  void clear_elements() {
    for (std::list<element_t *>::iterator i = elements.begin();
	 i != elements.end();
	 i++)
      checked_delete(*i);
    elements.clear();
  }

  virtual ~format_t() {
    TRACE_DTOR(format_t);
    clear_elements();
  }

  void parse(const string& fmt);

  void compile(const string& fmt, xml::node_t * context = NULL) {
    parse(fmt);
    compile(context);
  }
  void compile(xml::node_t * context = NULL);

  int format(std::ostream& out, xml::node_t * context = NULL,
	     int column = 0, const element_formatter_t& formatter =
	     element_formatter_t()) const;

  operator bool() const {
    return ! format_string.empty();
  }
};

DECLARE_EXCEPTION(format_error);

} // namespace ledger

#endif // _FORMAT_H
