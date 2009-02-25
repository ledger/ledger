/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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

#include "scope.h"

namespace ledger {

void symbol_scope_t::define(const string& name, expr_t::ptr_op_t def)
{
  DEBUG("scope.symbols", "Defining '" << name << "' = " << def);

  std::pair<symbol_map::iterator, bool> result
    = symbols.insert(symbol_map::value_type(name, def));
  if (! result.second) {
    symbol_map::iterator i = symbols.find(name);
    assert(i != symbols.end());
    symbols.erase(i);

    std::pair<symbol_map::iterator, bool> result2
      = symbols.insert(symbol_map::value_type(name, def));
    if (! result2.second)
      throw_(compile_error, _("Redefinition of '%1' in same scope") << name);
  }
}

expr_t::ptr_op_t symbol_scope_t::lookup(const string& name)
{
  symbol_map::const_iterator i = symbols.find(name);
  if (i != symbols.end())
    return (*i).second;

  return child_scope_t::lookup(name);
}

} // namespace ledger
