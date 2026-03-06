#include <utility>

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
 * @addtogroup util
 */

/**
 * @file   error.h
 * @author John Wiegley
 *
 * @ingroup util
 *
 * @brief Exception throwing, warning, and error context facilities.
 *
 * Ledger avoids using raw `throw` statements.  Instead, all exceptions are
 * raised through the `throw_()` macro, which first assembles the message in
 * a thread-local `std::ostringstream` and then throws the specified exception
 * type.  This keeps the throw-site syntax concise and supports Boost.Format
 * and streaming operators in the message expression.
 *
 * Similarly, `warning_()` prints a warning to stderr and `add_error_context()`
 * appends location information (file name, line number, source excerpt) to a
 * thread-local context buffer.  When an exception is caught and reported,
 * `error_context()` retrieves and clears this buffer to display the full
 * chain of context surrounding the error.
 *
 * Helper functions `file_context()`, `line_context()`, and `source_context()`
 * format specific kinds of context strings (source file positions, caret
 * indicators for column positions, and raw source text excerpts).
 */
#pragma once

namespace ledger {

/// Thread-local buffer used by `throw_()` and `warning_()` to assemble
/// exception/warning messages via streaming operators before the throw
/// or print.  Cleared after each use.
extern thread_local std::ostringstream _desc_buffer;

/// Throw an exception of type @p T with the given message string.
/// Clears the description buffer before throwing.
template <typename T>
[[noreturn]] inline void throw_func(const string& message) {
  _desc_buffer.clear();
  _desc_buffer.str("");
  throw T(message);
}

/// Assemble an error message via streaming operators and throw it as
/// an exception of type @p cls.  Example:
///   `throw_(std::runtime_error, "bad value: " << val)`
#define throw_(cls, msg) ((_desc_buffer << (msg)), throw_func<cls>(_desc_buffer.str()))

/// Print a warning message to stderr, prefixed with "Warning: ".
inline void warning_func(const string& message) {
  std::cerr << "Warning: " << message << '\n';
  _desc_buffer.clear();
  _desc_buffer.str("");
}

/// Assemble a warning message via streaming operators and print it.
#define warning_(msg) ((_desc_buffer << (msg)), warning_func(_desc_buffer.str()))

/// Thread-local buffer that accumulates error context lines (file positions,
/// source excerpts) while parsing.  Retrieved by `error_context()` when
/// an exception is reported.
extern thread_local std::ostringstream _ctxt_buffer;

/// Append a context line to the error context buffer.  Multiple lines
/// are separated by newlines.  The parser calls this as it enters
/// nested scopes (files, transactions, postings) so that if an error
/// occurs, the full chain of context is available.
#define add_error_context(msg)                                                                     \
  ((long(_ctxt_buffer.tellp()) == 0) ? (_ctxt_buffer << (msg)) : (_ctxt_buffer << '\n' << (msg)))

/// Retrieve and clear the accumulated error context string.
string error_context();

/// Format a "file:line:" context string for error messages.
string file_context(const path& file, std::size_t line);

/// Format a source line with a caret (^) indicator under the error position.
/// If @p end_pos is nonzero, underlines from @p pos to @p end_pos.
string line_context(const string& line, const string::size_type pos = 0,
                    const string::size_type end_pos = 0);

/// Extract and format raw source text from a journal file between two
/// stream positions.  Each line is prefixed with @p prefix.  Output is
/// truncated to 4096 bytes to avoid unbounded allocation for very large
/// transactions.
string source_context(const path& file, const std::istream::pos_type pos,
                      const std::istream::pos_type end_pos, const string& prefix = "");

/// Tracks the number of errors encountered during a parse run.  Thrown
/// when the error threshold is exceeded, carrying the count and a summary
/// message.  This is not a std::exception subclass; it is caught specially
/// by the top-level error handler.
struct error_count {
  std::size_t count;       ///< Number of errors encountered
  std::string message;     ///< Summary message
  explicit error_count(std::size_t _count, std::string _msg)
      : count(_count), message(std::move(_msg)) {}
  const char* what() const { return message.c_str(); }
};

} // namespace ledger
