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
 * @addtogroup expr
 */

/**
 * @file   format.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _FORMAT_H
#define _FORMAT_H

#include "expr.h"

namespace ledger {

class unistring;

DECLARE_EXCEPTION(format_error, std::runtime_error);

/**
 * @brief Brief
 *
 * Long.
 */
class format_t : public noncopyable
{
  struct element_t : public supports_flags<>, public noncopyable
  {
#define ELEMENT_ALIGN_LEFT 0x01

    enum kind_t {
      STRING,
      EXPR,
    };

    kind_t	type;
    std::size_t min_width;
    std::size_t max_width;
    string	chars;
    expr_t	expr;

    scoped_ptr<struct element_t> next;

    element_t() throw()
      : supports_flags<>(), type(STRING), min_width(0), max_width(0) {
      TRACE_CTOR(element_t, "");
    }
    ~element_t() throw() {
      TRACE_DTOR(element_t);
    }

    friend inline void mark_red(std::ostream& out, const element_t * elem) {
      out.setf(std::ios::left);
      out.width(0);
      out << "\e[31m";

      if (elem->has_flags(ELEMENT_ALIGN_LEFT))
	out << std::left;
      else
	out << std::right;

      if (elem->min_width > 0)
	out.width(elem->min_width);
    }

    void dump(std::ostream& out) const;
  };

  string		 format_string;
  scoped_ptr<element_t>	 elements;

public:
  enum elision_style_t {
    TRUNCATE_TRAILING,
    TRUNCATE_MIDDLE,
    TRUNCATE_LEADING,
    ABBREVIATE
  };

private:
  static element_t * parse_elements(const string& fmt);

public:
  format_t() {
    TRACE_CTOR(format_t, "");
  }
  format_t(const string& _format) {
    TRACE_CTOR(format_t, "const string&");
    parse(_format);
  }
  ~format_t() {
    TRACE_DTOR(format_t);
  }

  void parse(const string& _format) {
    elements.reset(parse_elements(_format));
    format_string = _format;
  }

  void format(std::ostream& out, scope_t& scope);

  void dump(std::ostream& out) const {
    for (const element_t * elem = elements.get();
	 elem;
	 elem = elem->next.get())
      elem->dump(out);
  }

  static string truncate(const unistring& str, std::size_t width,
			 const int account_abbrev_length = -1);
};

#define FMT_PREFIX "fmt_"
#define FMT_PREFIX_LEN 4

#define WANT_FMT()					\
  (std::strncmp(p, FMT_PREFIX, FMT_PREFIX_LEN) == 0)

} // namespace ledger

#endif // _FORMAT_H
