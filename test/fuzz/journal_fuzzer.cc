/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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

#include <ledger.hh>

#include "amount.h"
#include "journal.h"
#include "context.h"
#include "account.h"
#include "scope.h"
#include "utils.h"

#include <cstdint>
#include <cstddef>
#include <sstream>
#include <string>

using namespace ledger;

static bool initialized = false;

static void ensure_initialized() {
  if (!initialized) {
    amount_t::initialize();
    initialized = true;
  }
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
  // Limit input size to avoid timeouts on large inputs
  if (size > 65536)
    return 0;

  ensure_initialized();

  try {
    std::string input(reinterpret_cast<const char*>(data), size);
    shared_ptr<std::istringstream> stream =
        shared_ptr<std::istringstream>(new std::istringstream(input));

    journal_t journal;
    account_t master("");
    journal.master = &master;

    parse_context_stack_t context_stack;
    context_stack.push(
        shared_ptr<std::istream>(stream),
        filesystem::current_path());
    parse_context_t& context = context_stack.get_current();
    context.journal = &journal;
    context.master = &master;

    // Use an empty_scope since we don't have a full session
    empty_scope_t empty_scope;
    context.scope = &empty_scope;

    journal.read_textual(context_stack, NO_HASHES);
  } catch (...) {
    // Suppress all exceptions; we only care about crashes and sanitizer
    // findings (ASan, UBSan).
  }

  return 0;
}
