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
#include "expr.h"

namespace ledger {

class xact_base_t;
class xact_t;
class auto_xact_t;
class period_xact_t;
class post_t;
class account_t;
class parse_context_t;
class parse_context_stack_t;

typedef std::list<xact_t *>              xacts_list;
typedef std::list<auto_xact_t *>         auto_xacts_list;
typedef std::list<period_xact_t *>       period_xacts_list;
typedef std::pair<mask_t, string>        payee_alias_mapping_t;
typedef std::list<payee_alias_mapping_t> payee_alias_mappings_t;
typedef std::pair<string, string>        payee_uuid_mapping_t;
typedef std::list<payee_uuid_mapping_t>  payee_uuid_mappings_t;
typedef std::pair<mask_t, account_t *>   account_mapping_t;
typedef std::list<account_mapping_t>     account_mappings_t;
typedef std::map<string, account_t *>    accounts_map;
typedef std::map<string, xact_t *>       checksum_map_t;

typedef std::multimap<string, expr_t::check_expr_pair> tag_check_exprs_map;

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
      size    = file_size(*filename);
      modtime = posix_time::from_time_t(last_write_time(*filename));
      TRACE_CTOR(journal_t::fileinfo_t, "const path&");
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

#if HAVE_BOOST_SERIALIZATION
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

  account_t *            master;
  account_t *            bucket;
  xacts_list             xacts;
  auto_xacts_list        auto_xacts;
  period_xacts_list      period_xacts;
  std::list<fileinfo_t>  sources;
  std::set<string>       known_payees;
  std::set<string>       known_tags;
  bool                   fixed_accounts;
  bool                   fixed_payees;
  bool                   fixed_commodities;
  bool                   fixed_metadata;
  bool                   was_loaded;
  bool                   force_checking;
  bool                   check_payees;
  bool                   day_break;
  bool                   recursive_aliases;
  bool                   no_aliases;
  payee_alias_mappings_t payee_alias_mappings;
  payee_uuid_mappings_t  payee_uuid_mappings;
  account_mappings_t     account_mappings;
  accounts_map           account_aliases;
  account_mappings_t     payees_for_unknown_accounts;
  checksum_map_t         checksum_map;
  tag_check_exprs_map    tag_check_exprs;
  optional<expr_t>       value_expr;
  parse_context_t *      current_context;

  enum checking_style_t {
    CHECK_PERMISSIVE,
    CHECK_NORMAL,
    CHECK_WARNING,
    CHECK_ERROR
  } checking_style;

  journal_t();
#if 0
  journal_t(const path& pathname);
  journal_t(const string& str);
#endif
  ~journal_t();

  void initialize();

  std::list<fileinfo_t>::iterator sources_begin() {
    return sources.begin();
  }
  std::list<fileinfo_t>::iterator sources_end() {
    return sources.end();
  }

  void        add_account(account_t * acct);
  bool        remove_account(account_t * acct);
  account_t * find_account(const string& name, bool auto_create = true);
  account_t * find_account_re(const string& regexp);

  account_t * expand_aliases(string name);

  account_t * register_account(const string& name, post_t * post,
                               account_t * master = NULL);
  string      register_payee(const string& name, xact_t * xact);
  void        register_commodity(commodity_t& comm,
                                 variant<int, xact_t *, post_t *> context);
  void        register_metadata(const string& key, const value_t& value,
                                variant<int, xact_t *, post_t *> context);

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

  std::size_t read(parse_context_stack_t& context);

  bool has_xdata();
  void clear_xdata();

  bool valid() const;

private:
  std::size_t read_textual(parse_context_stack_t& context);

#if HAVE_BOOST_SERIALIZATION
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
    ar & checksum_map;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

} // namespace ledger

#endif // _JOURNAL_H
