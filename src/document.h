/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#ifndef _DOCUMENT_H
#define _DOCUMENT_H

#include "node.h"
#include "value.h"

namespace ledger {
namespace xml {

enum ledger_builtins_t {
  ACCOUNT_ATTR = 10,
  ACCOUNT_PATH_NODE,
  AMOUNT_ATTR,
  AMOUNT_NODE,
  AMOUNT_EXPR_NODE,
  ARG_ATTR,
  AUTO_ENTRY_NODE,
  BALANCE_ATTR,
  CHECKIN_NODE,
  CLEARED_ATTR,
  CODE_ATTR,
  COMMODITY_NODE,
  COMMODITY_CONVERSION_NODE,
  COMMODITY_NOMARKET_NODE,
  COMMODITY_TEMPLATE_NODE,
  COST_ATTR,
  CURRENT_YEAR_NODE,
  DATE_ATTR,
  DEFAULT_ACCOUNT_NODE,
  DIRECTIVE_NODE,
  EFF_DATE_ATTR,
  ENTRIES_NODE,
  ENTRY_NODE,
  FROM_ATTR,
  JOURNAL_NODE,
  LEDGER_NODE,
  NAME_ATTR,
  NOTE_NODE,
  PAYEE_NODE,
  PENDING_ATTR,
  PERIOD_NODE,
  PERIOD_ENTRY_NODE,
  PRICE_ATTR,
  PRICE_HISTORY_NODE,
  RULE_NODE,
  SYMBOL_ATTR,
  TEMPLATE_ATTR,
  TIME_ATTR,
  TO_ATTR,
  TRANSACTION_NODE,
  VIRTUAL_ATTR,
  YEAR_ATTR
};

class document_t : public parent_node_t
{
  typedef std::pair<string, nameid_t> name_pair;

  typedef multi_index_container<
    name_pair,
    multi_index::indexed_by<
      multi_index::random_access<>,
      multi_index::hashed_unique<
	multi_index::member<name_pair, string, &name_pair::first> >
    >
  > names_t;

  optional<names_t> names;

public:
  // Ids 0-9 are reserved.  10-999 are for "builtin" names.  1000+ are
  // for dynamically registered names.
  enum special_names_t {
    CURRENT, PARENT, ROOT, ALL, LAST_BUILTIN = 10
  };

  document_t(node_t::nameid_t _name_id)
    : parent_node_t(_name_id, *this) {
    TRACE_CTOR(xml::document_t, "node_t::nameid_t");
  }
  ~document_t() {
    TRACE_DTOR(xml::document_t);
  }

  nameid_t register_name(const string& name);

  optional<nameid_t>        lookup_name_id(const string& name) const;
  static optional<nameid_t> lookup_builtin_id(const string& name);
  optional<const char *>    lookup_name(nameid_t id) const;

  void print(std::ostream& out) const;

#if 0
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
  class parser_t
  {
  public:
    document_t *	document;
    XML_Parser		parser;
    string		have_error;
    const char *	pending;
    node_t::attrs_map * pending_attrs;
    bool                handled_data;

    std::list<parent_node_t *> node_stack;

    parser_t() : document(NULL), pending(NULL), pending_attrs(NULL),
		 handled_data(false) {}
    virtual ~parser_t() {}

    virtual bool         test(std::istream& in) const;
    virtual document_t * parse(std::istream& in);
  };
#endif
#endif
};

} // namespace xml
} // namespace ledger

#endif // _DOCUMENT_H 
