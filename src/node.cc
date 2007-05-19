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

#include "node.h"
#include "document.h"

namespace ledger {
namespace xml {

const char * node_t::name() const
{
  return *document().lookup_name(name_id());
}

optional<value_t> node_t::get_attr(const string& _name) const
{
  optional<nameid_t> name_id = document().lookup_name_id(_name);
  if (name_id)
    return get_attr(*name_id);
  else
    return none;
}

void output_xml_string(std::ostream& out, const string& str)
{
  for (const char * s = str.c_str(); *s; s++) {
    switch (*s) {
    case '<':
      out << "&lt;";
      break;
    case '>':
      out << "&gt;";
      break;
    case '&':
      out << "&amp;";
      break;
    default:
      out << *s;
      break;
    }
  }
}

void node_t::print_attributes(std::ostream& out) const
{
  if (attributes)
    foreach (const attr_pair& attr, attributes->get<0>())
      out << ' ' << *document().lookup_name(attr.first)
	  << "=\"" << attr.second << "\"";

  IF_VERIFY()
    out << " type=\"parent_node_t\"";
}

void parent_node_t::print(std::ostream& out) const
{
  out << '<' << name();
  print_attributes(out);
  out << '>';

  foreach (node_t * child, *this)
    child->print(out);

  out << "</" << name() << '>';
}

void terminal_node_t::print(std::ostream& out) const
{
  if (data.empty()) {
    out << '<' << name();
    print_attributes(out);
    out << " />";
  } else {
    out << '<' << name();
    print_attributes(out);
    out << '>';
    output_xml_string(out, text());
    out << "</" << name() << '>';
  }
}

} // namespace xml
} // namespace ledger
