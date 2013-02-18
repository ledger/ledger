/*
 * Copyright (c) 2003-2013, John Wiegley.  All rights reserved.
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
 * @defgroup report Reporting
 */

/**
 * @file   archive.h
 * @author John Wiegley
 *
 * @ingroup report
 */
#ifndef _ARCHIVE_H
#define _ARCHIVE_H

#include "journal.h"

namespace ledger {

class archive_t
{
  path     file;

  std::list<journal_t::fileinfo_t> sources;

public:
  archive_t() {
    TRACE_CTOR(archive_t, "");
  }
  archive_t(const path& _file) : file(_file) {
    TRACE_CTOR(archive_t, "const path&");
  }
  archive_t(const archive_t& ar) : file(ar.file) {
    TRACE_CTOR(archive_t, "copy");
  }
  ~archive_t() {
    TRACE_DTOR(archive_t);
  }

  bool read_header();

  bool should_load(const std::list<path>& data_files);
  bool should_save(journal_t& journal);

  void save(journal_t& journal);
  bool load(journal_t& journal);

#if HAVE_BOOST_SERIALIZATION
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & sources;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

} // namespace ledger

#endif // _ARCHIVE_H
