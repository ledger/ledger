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

using namespace boost::python;
using namespace ledger;

void export_session()
{
  class_< session_t > ("Session")
    .def_readwrite("init_file", &session_t::init_file)
    .def_readwrite("data_file", &session_t::data_file)
    .def_readwrite("cache_file", &session_t::cache_file)
    .def_readwrite("price_db", &session_t::price_db)

    .def_readwrite("balance_format", &session_t::balance_format)
    .def_readwrite("register_format", &session_t::register_format)
    .def_readwrite("wide_register_format", &session_t::wide_register_format)
    .def_readwrite("plot_amount_format", &session_t::plot_amount_format)
    .def_readwrite("plot_total_format", &session_t::plot_total_format)
    .def_readwrite("print_format", &session_t::print_format)
    .def_readwrite("write_hdr_format", &session_t::write_hdr_format)
    .def_readwrite("write_xact_format", &session_t::write_xact_format)
    .def_readwrite("equity_format", &session_t::equity_format)
    .def_readwrite("prices_format", &session_t::prices_format)
    .def_readwrite("pricesdb_format", &session_t::pricesdb_format)

    .def_readwrite("pricing_leeway", &session_t::pricing_leeway)

    .def_readwrite("download_quotes", &session_t::download_quotes)
    .def_readwrite("use_cache", &session_t::use_cache)
    .def_readwrite("cache_dirty", &session_t::cache_dirty)
    .def_readwrite("debug_mode", &session_t::debug_mode)
    .def_readwrite("verbose_mode", &session_t::verbose_mode)
    .def_readwrite("trace_alloc_mode", &session_t::trace_alloc_mode)
    .def_readwrite("trace_class_mode", &session_t::trace_class_mode)

    .def_readwrite("journals", &session_t::journals)
    ;
}
