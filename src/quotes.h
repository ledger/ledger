/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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
 * @addtogroup extra
 */

/**
 * @file   quotes.h
 * @author John Wiegley
 *
 * @ingroup extra
 */
#ifndef _QUOTES_H
#define _QUOTES_H

namespace ledger {


/** @brief singleton class to retrieve commodity quotes from an external source
 */
 class quote_loader_t
 {
 public:
   static quote_loader_t* instance();
   static quote_loader_t* instance(path script_path);

   static path get_path();
   static void set_path(path getquote_path);

  optional<price_point_t>
     get_quote(commodity_t& commodity,
	       const optional<commodity_t&>& exchange_commodity);
  optional<price_point_t>

  commodity_quote_from_script(commodity_t& commodity,
				const optional<commodity_t&>& exchange_commodity);


 private:
   path script;
   quote_loader_t(){};

   quote_loader_t(quote_loader_t const &){};
   quote_loader_t& operator=(quote_loader_t const &){};
   static quote_loader_t* instance_p;
 };
} // namespace ledger

#endif // _QUOTES_H
