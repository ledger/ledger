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

#include "pyinterp.h"
#include "pyutils.h"
#include "iterators.h"

#include <boost/python/exception_translator.hpp>
#include <boost/python/implicit.hpp>
#include <boost/python/args.hpp>

namespace ledger {

using namespace boost::python;

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_ArithmeticError, err.what());	\
  }

//EXC_TRANSLATOR(iterators_error)

void export_iterators()
{
#if 0
  class_< xacts_iterator > ("XactsIterator")
    ;
  class_< entry_xacts_iterator > ("EntryXactsIterator")
    ;
  class_< entries_iterator > ("EntriesIterator")
    ;
  class_< session_xacts_iterator > ("SessionXactsIterator")
    ;
  class_< accounts_iterator > ("AccountsIterator")
    ;
  class_< basic_accounts_iterator > ("BasicAccountsIterator")
    ;
  class_< sorted_accounts_iterator > ("SortedAccountsIterator")
    ;
  class_< journals_iterator > ("JournalsIterator")
    ;
#endif

  //register_optional_to_python<amount_t>();

  //implicitly_convertible<string, amount_t>();

#define EXC_TRANSLATE(type)					\
  register_exception_translator<type>(&exc_translate_ ## type);

  //EXC_TRANSLATE(iterators_error);
}

} // namespace ledger
