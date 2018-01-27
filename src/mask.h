/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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
 * @addtogroup util
 */

/**
 * @file   mask.h
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief Regular expression masking.
 */
#ifndef _MASK_H
#define _MASK_H

#include "utils.h"
#if HAVE_BOOST_REGEX_UNICODE
#include "unistring.h"
#endif

namespace ledger {

class mask_t
{
public:
#if HAVE_BOOST_REGEX_UNICODE
  boost::u32regex expr;
#else
  boost::regex expr;
#endif

  explicit mask_t(const string& pattern);

  mask_t() : expr() {
    TRACE_CTOR(mask_t, "");
  }
  mask_t(const mask_t& m) : expr(m.expr) {
    TRACE_CTOR(mask_t, "copy");
  }
  ~mask_t() throw() {
    TRACE_DTOR(mask_t);
  }

  mask_t& operator=(const string& other);
  mask_t& assign_glob(const string& other);

  bool operator<(const mask_t& other) const {
    return expr < other.expr;
  }
  bool operator==(const mask_t& other) const {
    return expr == other.expr;
  }

  bool match(const string& text) const {
#if HAVE_BOOST_REGEX_UNICODE
    DEBUG("mask.match",
          "Matching: \"" << text << "\" =~ /" << str() << "/ = "
          << (boost::u32regex_search(text, expr) ? "true" : "false"));
    return boost::u32regex_search(text, expr);
#else
    DEBUG("mask.match",
          "Matching: \"" << text << "\" =~ /" << str() << "/ = "
          << (boost::regex_search(text, expr) ? "true" : "false"));
    return boost::regex_search(text, expr);
#endif
  }

  bool empty() const {
    return expr.empty();
  }

  string str() const {
    if (! empty()) {
#if HAVE_BOOST_REGEX_UNICODE
      assert(sizeof(boost::uint32_t) == sizeof(UChar32));
      unistring ustr;
      std::basic_string<UChar32> expr_str = expr.str();
      std::copy(expr_str.begin(), expr_str.end(),
                std::back_inserter(ustr.utf32chars));
      return ustr.extract();
#else
      return expr.str();
#endif
    } else {
      return empty_string;
    }
  }

  bool valid() const {
    if (expr.status() != 0) {
      DEBUG("ledger.validate", "mask_t: expr.status() != 0");
      return false;
    }
    return true;
  }
};

inline std::ostream& operator<<(std::ostream& out, const mask_t& mask) {
  out << mask.str();
  return out;
}

inline void put_mask(property_tree::ptree& pt, const mask_t& mask) {
  pt.put_value(mask.str());
}

} // namespace ledger

#endif // _MASK_H
