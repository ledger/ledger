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

#include "document.h"

namespace ledger {
namespace xml {

namespace {
  const std::size_t ledger_builtins_size = 39;
  const char *      ledger_builtins[] = {
    "account",
    "account-path",
    "amount",
    "amount",
    "amount-expr",
    "arg",
    "auto-entry",
    "balance",
    "checkin",
    "cleared",
    "code",
    "commodity",
    "commodity-conversion",
    "commodity-nomarket",
    "commodity-template",
    "cost",
    "current-year",
    "date",
    "default-account",
    "directive",
    "effective",
    "entries",
    "entry",
    "from",
    "journal",
    "ledger",
    "name",
    "note",
    "payee",
    "pending",
    "period",
    "period-entry",
    "price",
    "price-history",
    "rule",
    "symbol",
    "template",
    "time",
    "to",
    "transaction",
    "virtual",
    "year"
  };
}

node_t::nameid_t document_t::register_name(const string& name)
{
  optional<nameid_t> index = lookup_name_id(name);
  if (index)
    return *index;

  if (! names)
    names = names_t();

  nameid_t real_index = names->size() + 1000;
  names->push_back(name_pair(name, real_index));
  DEBUG("xml.lookup", this << " Inserted name: " << name);

  return real_index;
}

optional<node_t::nameid_t> document_t::lookup_name_id(const string& name) const
{
  if (optional<node_t::nameid_t> id = lookup_builtin_id(name))
    return id;

  if (! names)
    return none;

  DEBUG("xml.lookup", this << " Finding name: " << name);

  typedef names_t::nth_index<1>::type names_by_name;

  const names_by_name& name_index = names->get<1>();
  names_by_name::const_iterator i = name_index.find(name);
  if (i != name_index.end())
    return (*i).second;

  return none;
}

optional<node_t::nameid_t> document_t::lookup_builtin_id(const string& name)
{
  int first = 0;
  int last  = static_cast<int>(ledger_builtins_size);

  while (first <= last) {
    int mid = (first + last) / 2; // compute mid point.

    int result;
    if ((result = (static_cast<int>(name[0]) -
		   static_cast<int>(ledger_builtins[mid][0]))) == 0)
      result = std::strcmp(name.c_str(), ledger_builtins[mid]);

    if (result > 0)
      first = mid + 1;		// repeat search in top half.
    else if (result < 0)
      last = mid - 1;		// repeat search in bottom half.
    else
      return nameid_t(mid + 10);
  }

  return none;
}

optional<const char *> document_t::lookup_name(nameid_t id) const
{
  if (id < 1000) {
    switch (id) {
    case CURRENT:
      return ".";
    case PARENT:
      return "..";
    case ROOT:
      return "";
    case ALL:
      return "*";

    default:
      assert(id >= 10);
      return ledger_builtins[id - 10];
    }
  }
  else if (names) {
    assert(id >= 1000);
    std::size_t index = id - 1000;
    typedef names_t::nth_index<0>::type names_by_random_access;
    const names_by_random_access& random_access = names->get<0>();
    if (index < random_access.size())
      return random_access[index].first.c_str();
  }
  return none;
}

void document_t::print(std::ostream& out) const
{
  out << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  parent_node_t::print(out);
}

} // namespace xml
} // namespace ledger
