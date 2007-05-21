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

#include "jbuilder.h"
#include "compile.h"

namespace ledger {
namespace xml {

void journal_builder_t::begin_node(const node_t::nameid_t name_id,
				   bool terminal)
{
  switch (name_id) {
  case JOURNAL_NODE:
    current = current->as_parent_node().create_child<journal_node_t>(name_id);
    break;
  case ENTRY_NODE:
    current = current->as_parent_node().create_child<entry_node_t>(name_id);
    break;
  case TRANSACTION_NODE:
    current = current->as_parent_node().create_child<transaction_node_t>(name_id);
    break;

  default:
    if (terminal)
      current = current->as_parent_node().create_child<terminal_node_t>(name_id);
    else
      current = current->as_parent_node().create_child<parent_node_t>(name_id);
    break;
  }

  foreach (const attrs_list::value_type& pair, current_attrs)
    current->set_attr(pair.first, pair.second.c_str());

  current_attrs.clear();
}

} // namespace xml
} // namespace ledger
