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
#include "amount.h"

namespace ledger {

using namespace boost::python;

void py_add_price(commodity_t&	    commodity,
		  const datetime_t& date,
		  const amount_t&   price)
{
  commodity.add_price(date, price);
}

void export_commodity()
{
  scope().attr("COMMODITY_STYLE_DEFAULTS")  = COMMODITY_STYLE_DEFAULTS;
  scope().attr("COMMODITY_STYLE_SUFFIXED")  = COMMODITY_STYLE_SUFFIXED;
  scope().attr("COMMODITY_STYLE_SEPARATED") = COMMODITY_STYLE_SEPARATED;
  scope().attr("COMMODITY_STYLE_EUROPEAN")  = COMMODITY_STYLE_EUROPEAN;
  scope().attr("COMMODITY_STYLE_THOUSANDS") = COMMODITY_STYLE_THOUSANDS;
  scope().attr("COMMODITY_NOMARKET")        = COMMODITY_NOMARKET;
  scope().attr("COMMODITY_BUILTIN")         = COMMODITY_BUILTIN;
  scope().attr("COMMODITY_WALKED")          = COMMODITY_WALKED;

  class_< commodity_t, bases<>,
	  commodity_t, boost::noncopyable > ("Commodity", no_init)
    .def(self == self)

    .def("drop_flags", &commodity_t::drop_flags)

    .def("add_price", py_add_price)

    .def("precision", &commodity_t::precision)
    ;

#if 0
  class_< annotation_t, bases<>,
    commodity_t, boost::noncopyable > ("Annotation", no_init)
    ;
  class_< keep_details_t, bases<>,
    commodity_t, boost::noncopyable > ("KeepDetails", no_init)
    ;
  class_< annotated_commodity_t, bases<>,
    commodity_t, boost::noncopyable > ("AnnotatedCommodity", no_init)
    ;
#endif
}

} // namespace ledger
