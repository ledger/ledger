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

#ifndef _FORMAT_H
#define _FORMAT_H

#define SUPPORT_UNICODE 1

#include "journal.h"
#include "expr.h"
#if defined(SUPPORT_UNICODE)
#include "utf8.h"
#endif

namespace ledger {

DECLARE_EXCEPTION(format_error, std::runtime_error);

#if defined(SUPPORT_UNICODE)
/**
 * @class unistring
 *
 * @brief Abstract working with UTF-32 encoded Unicode strings
 *
 * The input to the string is a UTF8 encoded ledger::string, which can
 * then have its true length be taken, or characters extracted.
 */
class unistring
{
  std::vector<uint32_t> utf32chars;

public:
  unistring(const string& input)
  {
    TRACE_CTOR(unistring, "");

    const char * p   = input.c_str();
    std::size_t	 len = input.length();

    VERIFY(utf8::is_valid(p, p + len));

    utf8::utf8to32(p, p + len, std::back_inserter(utf32chars));
  }
  ~unistring() {
    TRACE_DTOR(unistring);
  }

  std::size_t length() const {
    return utf32chars.size();
  }

  string extract(const std::size_t begin = 0,
		 const std::size_t len   = 0) const
  {
    string utf8result;
    utf8::utf32to8(utf32chars.begin() + begin,
		   utf32chars.begin() + begin + (len ? len : length()),
		   std::back_inserter(utf8result));
    return utf8result;
  }
};
#endif

class report_t;

class format_t : public noncopyable
{
  struct element_t : public supports_flags<>, public noncopyable
  {
#define ELEMENT_ALIGN_LEFT 0x01
#define ELEMENT_FORMATTED  0x02

    enum kind_t {
      STRING,
      EXPR,
    };

    kind_t	  type;
    unsigned char min_width;
    unsigned char max_width;
    string	  chars;
    expr_t	  expr;

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
  // jww (2008-08-02): Should these four be here, or in session_t?
  static elision_style_t elision_style;
  static int		 abbrev_length;

  static bool		 ansi_codes;
  static bool		 ansi_invert;

  static element_t * parse_elements(const string& fmt);

  friend class report_t;

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
			 const bool is_account = false);
};

} // namespace ledger

#endif // _FORMAT_H
