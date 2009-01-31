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
 * @defgroup the_group Group Title
 * @addtogroup the_group
 */

/**
 * @file   template.h
 * @author John Wiegley
 *
 * @ingroup the_group
 *
 * @brief Brief description
 *
 * Full description.
 */
#ifndef _TEMPLATE_H
#define _TEMPLATE_H

namespace template {

/**
 * @brief Brief description
 *
 * Full description.
 */
class template_t : public ordered_field_operators<template_t>
#if 0
		   public boost::noncopyable
#endif
{
public:
  /**
   * @name Class statics
   */
  /*@{*/

  static bool stream_fullstrings;

  /*@}*/

  /**
   * @name Constructors
   */
  /*@{*/

  /**
   * @see other_function
   * @warning Watch out!
   */
  template_t() {
    TRACE_CTOR(template_t, "");
  }

  /*@}*/

  /**
   * @name Destructor
   */
  /*@{*/

  ~template_t() {
    TRACE_DTOR(template_t);
  }

  /*@}*/

  /**
   * @name Assignment and copy
   */
  /*@{*/

  template_t(const template_t& other) {
    TRACE_CTOR(template_t, "copy");
  }

  template_t& operator=(const template_t& other);

  /*@}*/

private:
  template_t(const template_t& other);

public:
  /**
   * @name Comparison
   */
  /*@{*/

  /**
   * Compare two template objects.
   * @param other
   * @return An integer <0, 0 or >0.
   */
  int compare(const template_t& other) const;

  template <typename T>
  bool operator==(const T& val) const {
    return compare(val) == 0;
  }
  template <typename T>
  bool operator<(const T& other) const {
    return compare(other) < 0;
  }
  template <typename T>
  bool operator>(const T& other) const {
    return compare(other) > 0;
  }

  /*@}*/

  /**
   * @name Binary arithmetic
   */
  /*@{*/

  template_t& operator+=(const template_t& other);
  template_t& operator-=(const template_t& other);
  template_t& operator*=(const template_t& other);
  template_t& operator/=(const template_t& other);

  /*@}*/

  /**
   * @name Unary arithmetic
   */
  /*@{*/

  template_t operator-() const {
    return template_t();
  }

  /*@}*/

  /**
   * @name Truth tests
   */
  /*@{*/

  operator bool() const {
    return false;
  }

  /*@}*/

  /**
   * @name Conversion
   */
  /*@{*/

  operator int() const {
    return 0;
  }

  /*@}*/

  /**
   * @name Parsing
   */
  /*@{*/

  bool parse(std::istream& in);
  bool parse(const string& str) {
    std::istringstream stream(str);
    bool result = parse(stream, flags);
    assert(stream.eof());
    return result;
  }

  /*@}*/

  /**
   * @name Printing
   */
  /*@{*/

  void print(std::ostream& out) const;

  /*@}*/

  /**
   * @name Serialization
   */
  /*@{*/

  void read(std::istream& in);
  void read(const char *& data);
  void write(std::ostream& out) const;

  /*@}*/

  /**
   * @name XML Serialization
   */
  /*@{*/

  void read_xml(std::istream& in);
  void write_xml(std::ostream& out, const int depth = 0) const;

  /*@}*/

  /**
   * @name Debugging
   */
  /*@{*/

  void dump(std::ostream& out) const {
    print(out);
  }

  bool valid() const;

  /*@}*/
};

} // namespace template

#endif // _TEMPLATE_H
