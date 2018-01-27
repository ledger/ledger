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
 * @file   stream.h
 * @author John Wiegley, Omari Norman
 *
 * @ingroup util
 *
 * @brief A utility class for abstracting an output stream.
 *
 * Because Ledger might send output to a file, the console, or a pager
 * child process, different cleanup is needed for each scenario.  This
 * file abstracts those various needs.
 */
#ifndef _STREAM_H
#define _STREAM_H

#include "utils.h"

namespace ledger {

/**
 * @brief An output stream
 *
 * A stream to output in Ledger may be going to one of three places:
 * to stdout, to a file, or to a pager. Construct an output_stream_t and
 * the stream will automatically be cleaned up upon destruction.
 *
 * This class suffers from "else-if-heimer's disease," see Marshall
 * Cline's "C++ FAQ Lite". Arguably this should be three different
 * classes, but that introduces additional unneeded complications.
 */
class output_stream_t
{
  output_stream_t& operator=(const output_stream_t&);

private:
  int pipe_to_pager_fd;

public:
  /**
   * A pointer to the ostream.  Don't delete this; the output_stream_t
   * class takes care of this.
   */
  std::ostream * os;

  /**
   * Construct a new output_stream_t.
   */
  output_stream_t() : pipe_to_pager_fd(-1), os(&std::cout) {
    TRACE_CTOR(output_stream_t, "");
  }

  /**
   * When copy-constructed, make the copy just be a new output stream.  This
   * allows large classes to rely on their default copy-constructor without
   * worrying about pointer copying within output_stream_t.
   */
  output_stream_t(const output_stream_t&)
    : pipe_to_pager_fd(-1), os(&std::cout) {
    TRACE_CTOR(output_stream_t, "copy");
  }

  /**
   * Destroys an output_stream_t.  This deletes the dynamically
   * allocated ostream, if necessary. It also closes output file
   * descriptor, if necessary.
   */
  ~output_stream_t() {
    TRACE_DTOR(output_stream_t);
    close();
  }

  /**
   * Initialize the output stream object.
   *
   * @param output_file File to which to send output. If both this
   * and pager are set, output_file takes priority.
   *
   * @param pager_path Path to a pager. To not use a pager, leave this
   * empty.
   */
  void initialize(const optional<path>& output_file = none,
                  const optional<path>& pager_path  = none);

  /**
   * Convertor to a standard ostream.  This is used so that we can
   * stream directly to an object of type output_stream_t.
   */
  operator std::ostream&() {
    return *os;
  }

  /**
   * Flushing function.  A simple proxy for ostream's flush.
   */
  void flush() {
    os->flush();
  }

  /**
   * Close the output stream, waiting on the pager process if necessary.
   */
  void close();
};

} // namespace ledger

#endif // _STREAM_H
