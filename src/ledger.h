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

/**
 * @file   ledger.h
 * @author John Wiegley
 *
 * @mainpage Ledger Accounting Tool
 *
 * There are essentially nine steps involved in realizing a ledger reporting
 * session, with steps 5 and 8 -- which relate to the user's journal file --
 * being optional in the case of "pre-commands", since they do not require the
 * user's data to be read.
 *
 * \section global_init Initialize the global environment
 *
 * \section create_objs  Create session and report objects
 *
 * jww (2009-02-02): Set the "session context".
 *
 * \section process_opts Process user options
 *
 * This configures session and report objects
 *
 *    - environment
 *    - initialization file
 *    - command-line options
 *
 * \section lookup_cmd Locate object relating to command verb
 *
 * \section parse_data Parse the user's journal files
 *
 * \section create_out Create the output stream
 *
 * \section invoke_cmd Invoke the command object
 *
 * \section shutdown Wrap up, closing everything and releasing memory
 */
#ifndef _LEDGER_H
#define _LEDGER_H

#include <utils.h>
#include <option.h>

#include <value.h>

#include <expr.h>

#include <journal.h>
#include <iterators.h>
#include <compare.h>

#include <session.h>
#include <report.h>
#include <quotes.h>
#include <emacs.h>
#include <help.h>

#if defined(HAVE_BOOST_PYTHON)
#include <pyinterp.h>
#define LEDGER_SESSION_T python_interpreter_t
#else
#define LEDGER_SESSION_T session_t
#endif

#endif // _LEDGER_H
