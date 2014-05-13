/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
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
 * @addtogroup data
 */

/**
 * @file   context.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#ifndef _CONTEXT_H
#define _CONTEXT_H

#include "utils.h"
#include "times.h"

namespace ledger {

class journal_t;
class account_t;
class scope_t;

class parse_context_t
{
public:
  static const std::size_t MAX_LINE = 4096;

  shared_ptr<std::istream> stream;

  path             pathname;
  path             current_directory;
  journal_t *      journal;
  account_t *      master;
  scope_t *        scope;
  char             linebuf[MAX_LINE + 1];
  istream_pos_type line_beg_pos;
  istream_pos_type curr_pos;
  std::size_t      linenum;
  std::size_t      errors;
  std::size_t      count;
  std::size_t      sequence;

  explicit parse_context_t(const path& cwd)
    : current_directory(cwd), master(NULL), scope(NULL),
      linenum(0), errors(0), count(0), sequence(1) {}

  explicit parse_context_t(shared_ptr<std::istream> _stream,
                           const path& cwd)
    : stream(_stream), current_directory(cwd), master(NULL),
      scope(NULL), linenum(0), errors(0), count(0), sequence(1) {}

  parse_context_t(const parse_context_t& context)
   : stream(context.stream),
     pathname(context.pathname),
     current_directory(context.current_directory),
     journal(context.journal),
     master(context.master),
     scope(context.scope),
     line_beg_pos(context.line_beg_pos),
     curr_pos(context.curr_pos),
     linenum(context.linenum),
     errors(context.errors),
     count(context.count),
     sequence(context.sequence) {
    std::memcpy(linebuf, context.linebuf, MAX_LINE);
  }

  string location() const {
    return file_context(pathname, linenum);
  }

  void warning(const string& what) const {
    warning_func(location() + " " + what);
  }
  void warning(const boost::format& what) const {
    warning_func(location() + " " + string(what.str()));
  }
};

inline parse_context_t open_for_reading(const path& pathname,
                                        const path& cwd)
{
  path filename = resolve_path(pathname);
#if BOOST_VERSION >= 104600 && BOOST_FILESYSTEM_VERSION >= 3
  filename = filesystem::absolute(filename, cwd);
#else
  filename = filesystem::complete(filename, cwd);
#endif
  if (! exists(filename) || is_directory(filename))
    throw_(std::runtime_error,
           _f("Cannot read journal file %1%") % filename);

  path parent(filename.parent_path());
  shared_ptr<std::istream> stream(new ifstream(filename));
  parse_context_t context(stream, parent);
  context.pathname = filename;
  return context;
}

class parse_context_stack_t
{
  std::list<parse_context_t> parsing_context;

public:
  void push() {
    parsing_context.push_front(parse_context_t(filesystem::current_path()));
  }
  void push(shared_ptr<std::istream> stream,
            const path& cwd = filesystem::current_path()) {
    parsing_context.push_front(parse_context_t(stream, cwd));
  }
  void push(const path& pathname,
            const path& cwd = filesystem::current_path()) {
    parsing_context.push_front(open_for_reading(pathname, cwd));
  }

  void push(const parse_context_t& context) {
    parsing_context.push_front(context);
  }

  void pop() {
    assert(! parsing_context.empty());
    parsing_context.pop_front();
  }

  parse_context_t& get_current() {
    assert(! parsing_context.empty());
    return parsing_context.front();
  }
};

} // namespace ledger

#endif // _CONTEXT_H
