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
#include "filters.h"

#include <boost/python/exception_translator.hpp>
#include <boost/python/implicit.hpp>
#include <boost/python/args.hpp>

namespace ledger {

using namespace boost::python;

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_ArithmeticError, err.what());	\
  }

//EXC_TRANSLATOR(filters_error)

void export_filters()
{
#if 0
  class_< ignore_xacts > ("IgnoreXacts")
    ;
  class_< clear_xact_xdata > ("ClearXactXdata")
    ;
  class_< pass_down_xacts > ("PassDownXacts")
    ;
  class_< push_to_xacts_list > ("PushToXactsList")
    ;
  class_< truncate_entries > ("TruncateEntries")
    ;
  class_< set_account_value > ("SetAccountValue")
    ;
  class_< sort_xacts > ("SortXacts")
    ;
  class_< sort_entries > ("SortEntries")
    ;
  class_< filter_xacts > ("FilterXacts")
    ;
  class_< anonymize_xacts > ("AnonymizeXacts")
    ;
  class_< calc_xacts > ("CalcXacts")
    ;
  class_< invert_xacts > ("InvertXacts")
    ;
  class_< collapse_xacts > ("CollapseXacts")
    ;
  class_< component_xacts > ("ComponentXacts")
    ;
  class_< related_xacts > ("RelatedXacts")
    ;
  class_< changed_value_xacts > ("ChangedValueXacts")
    ;
  class_< subtotal_xacts > ("SubtotalXacts")
    ;
  class_< interval_xacts > ("IntervalXacts")
    ;
  class_< by_payee_xacts > ("ByPayeeXacts")
    ;
  class_< set_comm_as_payee > ("SetCommAsPayee")
    ;
  class_< set_code_as_payee > ("SetCodeAsPayee")
    ;
  class_< dow_xacts > ("DowXacts")
    ;
  class_< generate_xacts > ("GenerateXacts")
    ;
  class_< budget_xacts > ("BudgetXacts")
    ;
  class_< forecast_xacts > ("ForecastXacts")
    ;
  class_< clear_account_xdata > ("ClearAccountXdata")
    ;
  class_< pass_down_accounts > ("PassDownAccounts")
    ;
#endif

  //register_optional_to_python<amount_t>();

  //implicitly_convertible<string, amount_t>();

#define EXC_TRANSLATE(type)					\
  register_exception_translator<type>(&exc_translate_ ## type);

  //EXC_TRANSLATE(filters_error);
}

} // namespace ledger
