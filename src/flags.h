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

#ifndef _FLAGS_H
#define _FLAGS_H

template <typename T = boost::uint_least8_t>
class supports_flags
{
public:
  typedef T flags_t;

protected:
  flags_t flags_;

public:
  supports_flags() : flags_(0) {
    TRACE_CTOR(supports_flags, "");
  }
  supports_flags(const flags_t& arg) : flags_(arg) {
    TRACE_CTOR(supports_flags, "copy");
  }
  ~supports_flags() throw() {
    TRACE_DTOR(supports_flags);
  }

  flags_t flags() const {
    return flags_;
  }
  bool has_flags(const flags_t arg) const {
    return flags_ & arg;
  }

  void set_flags(const flags_t arg) {
    flags_ = arg;
  }
  void clear_flags() {
    flags_ = 0;
  }
  void add_flags(const flags_t arg) {
    flags_ |= arg;
  }
  void drop_flags(const flags_t arg) {
    flags_ &= ~arg;
  }
};

template <typename T = boost::uint_least8_t>
class delegates_flags : public boost::noncopyable
{
public:
  typedef T flags_t;

protected:
  supports_flags<T>& flags_;

public:
  delegates_flags() : flags_() {
    TRACE_CTOR(delegates_flags, "");
  }
  delegates_flags(supports_flags<T>& arg) : flags_(arg) {
    TRACE_CTOR(delegates_flags, "const supports_flags<T>&");
  }
  ~delegates_flags() throw() {
    TRACE_DTOR(delegates_flags);
  }

  flags_t flags() const {
    return flags_.flags();
  }
  bool has_flags(const flags_t arg) const {
    return flags_.has_flags(arg);
  }

  void set_flags(const flags_t arg) {
    flags_.set_flags(arg);
  }
  void clear_flags() {
    flags_.clear_flags();
  }
  void add_flags(const flags_t arg) {
    flags_.add_flags(arg);
  }
  void drop_flags(const flags_t arg) {
    flags_.drop_flags(arg);
  }
};

#endif // _FLAGS_H
