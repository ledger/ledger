/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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
 * @file   journal.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#ifndef _JOURNAL_H
#define _JOURNAL_H

#include "utils.h"
#include "times.h"
#include "mask.h"

namespace ledger {

class xact_base_t;
class xact_t;
class auto_xact_t;
class period_xact_t;
class account_t;
class scope_t;

typedef std::list<xact_t *>        xacts_list;
typedef std::list<auto_xact_t *>   auto_xacts_list;
typedef std::list<period_xact_t *> period_xacts_list;

typedef std::pair<mask_t, string>      payee_mapping_t;
typedef std::list<payee_mapping_t>     payee_mappings_t;
typedef std::pair<mask_t, account_t *> account_mapping_t;
typedef std::list<account_mapping_t>   account_mappings_t;

class journal_t : public noncopyable
{
public:
  struct fileinfo_t
  {
    optional<path> filename;
    uintmax_t      size;
    datetime_t     modtime;
    bool           from_stream;

    fileinfo_t() : size(0), from_stream(true) {
      TRACE_CTOR(journal_t::fileinfo_t, "");
    }
    fileinfo_t(const path& _filename)
      : filename(_filename), from_stream(false) {
      TRACE_CTOR(journal_t::fileinfo_t, "const path&");
      size    = file_size(*filename);
      modtime = posix_time::from_time_t(last_write_time(*filename));
    }
    fileinfo_t(const fileinfo_t& info)
      : filename(info.filename), size(info.size),
        modtime(info.modtime), from_stream(info.from_stream)
    {
      TRACE_CTOR(journal_t::fileinfo_t, "copy");
    }
    ~fileinfo_t() throw() {
      TRACE_DTOR(journal_t::fileinfo_t);
    }

#if defined(HAVE_BOOST_SERIALIZATION)
  private:
    /** Serialization. */

    friend class boost::serialization::access;

    template<class Archive>
    void serialize(Archive& ar, const unsigned int /* version */) {
      ar & filename;
      ar & size;
      ar & modtime;
      ar & from_stream;
    }
#endif // HAVE_BOOST_SERIALIZATION
  };

  account_t *           master;
  account_t *           bucket;
  xacts_list            xacts;
  auto_xacts_list       auto_xacts;
  period_xacts_list     period_xacts;
  std::list<fileinfo_t> sources;
  payee_mappings_t      payee_mappings;
  account_mappings_t    account_mappings;
  bool                  was_loaded;

  journal_t();
  journal_t(const path& pathname);
  journal_t(const string& str);
  ~journal_t();

  void initialize();

  std::list<fileinfo_t>::iterator sources_begin() {
    return sources.begin();
  }
  std::list<fileinfo_t>::iterator sources_end() {
    return sources.end();
  }

  // These four methods are delegated to the current session, since all
  // accounts processed are gathered together at the session level.
  void        add_account(account_t * acct);
  bool        remove_account(account_t * acct);
  account_t * find_account(const string& name, bool auto_create = true);
  account_t * find_account_re(const string& regexp);

  bool add_xact(xact_t * xact);
  void extend_xact(xact_base_t * xact);
  bool remove_xact(xact_t * xact);

  xacts_list::iterator xacts_begin() {
    return xacts.begin();
  }
  xacts_list::iterator xacts_end() {
    return xacts.end();
  }
  auto_xacts_list::iterator auto_xacts_begin() {
    return auto_xacts.begin();
  }
  auto_xacts_list::iterator auto_xacts_end() {
    return auto_xacts.end();
  }
  period_xacts_list::iterator period_xacts_begin() {
    return period_xacts.begin();
  }
  period_xacts_list::iterator period_xacts_end() {
    return period_xacts.end();
  }

  std::size_t read(std::istream& in,
                   const path&   pathname,
                   account_t *   master = NULL,
                   scope_t *     scope  = NULL);
  std::size_t read(const path&   pathname,
                   account_t *   master = NULL,
                   scope_t *     scope  = NULL);

  std::size_t parse(std::istream& in,
                    scope_t&      session_scope,
                    account_t *   master        = NULL,
                    const path *  original_file = NULL,
                    bool          strict        = false);

  bool has_xdata();
  void clear_xdata();

  bool valid() const;

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  /** Serialization. */

  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */) {
    ar & master;
    ar & bucket;
    ar & xacts;
    ar & auto_xacts;
    ar & period_xacts;
    ar & sources;
    ar & payee_mappings;
    ar & account_mappings;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

} // namespace ledger

#endif // _JOURNAL_H
