/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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
 * @brief  Parse context for journal file reading -- tracks position, errors,
 *         and scope while the textual parser processes each input file.
 *
 * @ingroup data
 *
 * Every journal file (or stream) being parsed is associated with a
 * parse_context_t that records the current line number, stream position,
 * pathname, and error count.  When the parser encounters an `include`
 * directive, a new context is pushed onto parse_context_stack_t so that
 * error messages can show the full include chain.
 *
 * The helper function open_for_reading() resolves a path, optionally
 * decrypts GPG-encrypted files, and returns a ready-to-use context.
 */
#pragma once

#include <utility>

#include "utils.h"
#include "times.h"

#if HAVE_GPGME
#include "gpgme.h"
#endif

namespace ledger {

class journal_t;
class account_t;
class scope_t;

/**
 * @brief Tracks all per-file state needed while parsing a journal file.
 *
 * A parse_context_t is created for every input file or stream. It carries
 * the stream handle, the file path (for error messages), the current line
 * buffer, position bookkeeping, and cumulative error/transaction counts.
 *
 * The `master` account is the root under which new accounts are registered
 * -- it may differ from journal->master when an `apply account` directive
 * is active.  The `scope` is used to evaluate value expressions that
 * appear in amount fields, metadata, or directives.
 */
class parse_context_t {
public:
  std::shared_ptr<std::istream> stream; ///< The input stream (file, string, or decrypted GPG)

  path pathname;          ///< Absolute path of the file being parsed (empty for stdin)
  path current_directory; ///< Working directory for resolving relative `include` paths
  journal_t* journal;     ///< The journal accumulating parsed data
  account_t* master;   ///< Root account for this parse scope (may be narrowed by `apply account`)
  scope_t* scope;      ///< Expression evaluation scope (typically the session)
  std::string linebuf; ///< Line buffer filled by std::getline
  std::istream::pos_type line_beg_pos; ///< Stream position at the start of the current line
  std::istream::pos_type curr_pos;     ///< Stream position after the most recently read line
  std::size_t linenum;                 ///< 1-based line number of the most recently read line
  std::size_t errors;                  ///< Count of parse errors encountered in this file
  std::size_t count;                   ///< Count of transactions successfully added to the journal
  std::size_t sequence; ///< Monotonically increasing sequence for ordering items across files
  std::string last;     ///< Text of the most recent error (for reporting after parsing)
  std::shared_ptr<string> source_content; ///< Buffered stdin content (when pathname is empty)

  explicit parse_context_t(const path& cwd)
      : current_directory(cwd), master(nullptr), scope(nullptr), linenum(0), errors(0), count(0),
        sequence(1) {}

  explicit parse_context_t(std::shared_ptr<std::istream> _stream, const path& cwd)
      : stream(std::move(_stream)), current_directory(cwd), master(nullptr), scope(nullptr),
        linenum(0), errors(0), count(0), sequence(1) {}

  parse_context_t(const parse_context_t& context)
      : stream(context.stream), pathname(context.pathname),
        current_directory(context.current_directory), journal(context.journal),
        master(context.master), scope(context.scope), linebuf(context.linebuf),
        line_beg_pos(context.line_beg_pos), curr_pos(context.curr_pos), linenum(context.linenum),
        errors(context.errors), count(context.count), sequence(context.sequence),
        source_content(context.source_content) {}
  parse_context_t& operator=(const parse_context_t&) = default;

  /// @brief Return a human-readable "file:line" string for error messages.
  string location() const { return file_context(pathname, linenum); }

  /// @brief Emit a warning prefixed with the current file location.
  void warning(const string& what) const { warning_func(location() + " " + what); }
  /// @brief Emit a warning from a boost::format, prefixed with location.
  void warning(const boost::format& what) const {
    warning_func(location() + " " + string(what.str()));
  }
};

/**
 * @brief Resolve a journal file path and open it for reading.
 *
 * Resolves the path relative to @p cwd, verifies the file exists and is
 * not a directory, optionally decrypts GPG-encrypted files (when GPGME
 * support is compiled in), and returns a parse_context_t whose stream is
 * ready for line-by-line reading.
 *
 * @param pathname  Path to the journal file (may be relative or use ~).
 * @param cwd       Directory used to resolve relative paths.
 * @return A parse_context_t with an open stream and the resolved pathname.
 * @throws std::runtime_error if the file cannot be found or is a directory.
 */
inline parse_context_t open_for_reading(const path& pathname, const path& cwd) {
  path filename = resolve_path(pathname);
  filename = filename.is_absolute() ? filename : std::filesystem::absolute(cwd / filename);
  if (!exists(filename) || is_directory(filename))
    throw_(std::runtime_error, _f("Cannot read journal file %1%") % filename);

  path parent(filename.parent_path());
#if HAVE_GPGME
  std::shared_ptr<std::istream> stream(decrypted_stream_t::open_stream(filename));
#else
  std::shared_ptr<std::istream> stream(new ifstream(filename, std::ios::binary));
#endif
  parse_context_t context(stream, parent);
  context.pathname = filename;
  return context;
}

/// @brief Return the current working directory, falling back to "/" on error.
inline path safe_current_path() {
  std::error_code ec;
  path cwd = std::filesystem::current_path(ec);
  if (ec)
    return path("/");
  return cwd;
}

/**
 * @brief A stack of parse contexts, one per file in an include chain.
 *
 * When the parser encounters an `include` directive, it pushes a new
 * parse_context_t for the included file.  When parsing of that file
 * completes, the context is popped so that the parent file resumes.
 * The front of the list is always the currently-active context.
 */
class parse_context_stack_t {
  std::list<parse_context_t> parsing_context; ///< Stack of active contexts (front = current)

public:
  /// Push a default context using the current working directory.
  void push() { parsing_context.push_front(parse_context_t(safe_current_path())); }
  /// Push a context for an already-opened stream.
  void push(std::shared_ptr<std::istream> stream, const path& cwd = safe_current_path()) {
    parsing_context.push_front(parse_context_t(std::move(stream), cwd));
  }
  /// Push a context by opening a file at the given path.
  void push(const path& pathname, const path& cwd = safe_current_path()) {
    parsing_context.push_front(open_for_reading(pathname, cwd));
  }

  /// Push a pre-built context (used when cloning state for sub-parsing).
  void push(const parse_context_t& context) { parsing_context.push_front(context); }

  /// Pop the most recent context after an included file is fully parsed.
  void pop() {
    assert(!parsing_context.empty());
    parsing_context.pop_front();
  }

  /// @brief Access the context for the file currently being parsed.
  parse_context_t& get_current() {
    assert(!parsing_context.empty());
    return parsing_context.front();
  }
};

} // namespace ledger
