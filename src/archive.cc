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

#include <system.hh>

#if HAVE_BOOST_SERIALIZATION

#include "archive.h"
#include "amount.h"
#include "commodity.h"
#include "pool.h"
#include "scope.h"
#include "account.h"
#include "post.h"
#include "xact.h"

#define LEDGER_MAGIC    0x4c454447
#define ARCHIVE_VERSION 0x03000006

//BOOST_IS_ABSTRACT(ledger::scope_t)
BOOST_CLASS_EXPORT(ledger::scope_t)
BOOST_CLASS_EXPORT(ledger::child_scope_t)
BOOST_CLASS_EXPORT(ledger::symbol_scope_t)
BOOST_CLASS_EXPORT(ledger::call_scope_t)
BOOST_CLASS_EXPORT(ledger::account_t)
BOOST_CLASS_EXPORT(ledger::item_t)
BOOST_CLASS_EXPORT(ledger::post_t)
BOOST_CLASS_EXPORT(ledger::xact_base_t)
BOOST_CLASS_EXPORT(ledger::xact_t)
BOOST_CLASS_EXPORT(ledger::auto_xact_t)
BOOST_CLASS_EXPORT(ledger::period_xact_t)

template void ledger::journal_t::serialize(boost::archive::binary_oarchive&,
                                           const unsigned int);
template void ledger::journal_t::serialize(boost::archive::binary_iarchive&,
                                           const unsigned int);
namespace ledger {

namespace {
  bool read_header_bits(std::istream& in) {
    uint32_t bytes;

    assert(sizeof(uint32_t) == 4);
    in.read(reinterpret_cast<char *>(&bytes), sizeof(uint32_t));
    if (bytes != LEDGER_MAGIC) {
      DEBUG("archive.journal", "Magic bytes not present");
      return false;
    }

    in.read(reinterpret_cast<char *>(&bytes), sizeof(uint32_t));
    if (bytes != ARCHIVE_VERSION) {
      DEBUG("archive.journal", "Archive version mismatch");
      return false;
    }

    return true;
  }

  void write_header_bits(std::ostream& out) {
    uint32_t bytes;

    assert(sizeof(uint32_t) == 4);
    bytes = LEDGER_MAGIC;
    out.write(reinterpret_cast<char *>(&bytes), sizeof(uint32_t));

    bytes = ARCHIVE_VERSION;
    out.write(reinterpret_cast<char *>(&bytes), sizeof(uint32_t));
  }
}

bool archive_t::read_header()
{
  uintmax_t size = file_size(file);
  if (size < 8)
    return false;

  // Open the stream, read the version number and the list of sources
  ifstream stream(file, std::ios::binary);
  if (! read_header_bits(stream))
    return false;

  boost::archive::binary_iarchive iarchive(stream);

  DEBUG("archive.journal", "Reading header from archive");
  iarchive >> *this;

  DEBUG("archive.journal",
        "Version number:    " << std::hex << ARCHIVE_VERSION << std::dec);
  DEBUG("archive.journal", "Number of sources: " << sources.size());

#if DEBUG_ON
  foreach (const journal_t::fileinfo_t& i, sources)
    DEBUG("archive.journal", "Loaded source: " << *i.filename);
#endif

  return true;
}

bool archive_t::should_load(const std::list<path>& data_files)
{
  std::size_t found = 0;

  DEBUG("archive.journal", "Should the archive be loaded?");

  if (! exists(file)) {
    DEBUG("archive.journal", "No, it does not exist");
    return false;
  }

  if (! read_header()) {
    DEBUG("archive.journal", "No, header failed to read");
    return false;
  }

  if (data_files.empty()) {
    DEBUG("archive.journal", "No, there were no data files!");
    return false;
  }

  if (sources.empty()) {
    DEBUG("archive.journal", "No, there were no sources!");
    return false;
  }

  if (data_files.size() != sources.size()) {
    DEBUG("archive.journal", "No, number of sources doesn't match: "
          << data_files.size() << " != " << sources.size());
    return false;
  }

  foreach (const path& p, data_files) {
    DEBUG("archive.journal", "Scanning for data file: " << p);

    if (! exists(p)) {
      DEBUG("archive.journal", "No, an input source no longer exists: " << p);
      return false;
    }

    foreach (const journal_t::fileinfo_t& i, sources) {
      assert(! i.from_stream);
      assert(i.filename);

      DEBUG("archive.journal", "Comparing against source file: " << *i.filename);

      if (*i.filename == p) {
        if (! exists(*i.filename)) {
          DEBUG("archive.journal",
                "No, a referent source no longer exists: " << *i.filename);
          return false;
        }

        if (i.modtime != posix_time::from_time_t(last_write_time(p))) {
          DEBUG("archive.journal", "No, a source's modtime has changed: " << p);
          return false;
        }

        if (i.size != file_size(p)) {
          DEBUG("archive.journal", "No, a source's size has changed: " << p);
          return false;
        }

        found++;
      }
    }
  }

  if (found != data_files.size()) {
    DEBUG("archive.journal", "No, not every source's name matched");
    return false;
  }

  DEBUG("archive.journal", "Yes, it should be loaded!");
  return true;
}

bool archive_t::should_save(journal_t& journal)
{
  std::list<path> data_files;

  DEBUG("archive.journal", "Should the archive be saved?");

  if (journal.was_loaded) {
    DEBUG("archive.journal", "No, it's one we loaded before");
    return false;
  }

  if (journal.sources.empty()) {
    DEBUG("archive.journal", "No, there were no sources!");
    return false;
  }

  foreach (const journal_t::fileinfo_t& i, journal.sources) {
    if (i.from_stream) {
      DEBUG("archive.journal", "No, one source was from a stream");
      return false;
    }

    if (! exists(*i.filename)) {
      DEBUG("archive.journal",
            "No, a source no longer exists: " << *i.filename);
      return false;
    }

    data_files.push_back(*i.filename);
  }

  if (should_load(data_files)) {
    DEBUG("archive.journal", "No, because it's still loadable");
    return false;
  }

  DEBUG("archive.journal", "Yes, it should be saved!");
  return true;
}

void archive_t::save(journal_t& journal)
{
  INFO_START(archive, "Saved journal file cache");

  ofstream stream(file, std::ios::binary);

  write_header_bits(stream);
  sources = journal.sources;

#if DEBUG_ON
  foreach (const journal_t::fileinfo_t& i, sources)
    DEBUG("archive.journal", "Saving source: " << *i.filename);
#endif

  boost::archive::binary_oarchive oa(stream);

  DEBUG("archive.journal", "Creating archive with version "
        << std::hex << ARCHIVE_VERSION << std::dec);
  oa << *this;

  DEBUG("archive.journal",
        "Archiving journal with " << sources.size() << " sources");
  oa << journal;

  INFO_FINISH(archive);
}

bool archive_t::load(journal_t& journal)
{
  INFO_START(archive, "Read cached journal file");

  ifstream stream(file, std::ios::binary);
  if (! read_header_bits(stream))
    return false;

  boost::archive::binary_iarchive iarchive(stream);

  // Skip past the archive header, it was already read in before
  archive_t temp;
  iarchive >> temp;

  iarchive >> journal;
  journal.was_loaded = true;

  INFO_FINISH(archive);

  return true;
}

} // namespace ledger

#endif // HAVE_BOOST_SERIALIZATION
