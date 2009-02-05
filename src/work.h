/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
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
 * @file   work.h
 * @author John Wiegley
 *
 * @brief Contains the top-level functions used by main.cc
 */
#ifndef _WORK_H
#define _WORK_H

namespace ledger {

typedef strings_list::iterator string_iterator;
typedef std::pair<string_iterator, string_iterator> string_iterator_pair;

void         handle_debug_options(int argc, char * argv[]);
void         read_environment_settings(report_t& report, char * envp[]);
strings_list read_command_arguments(report_t& report, strings_list args);
void         normalize_session_options(session_t& session);
function_t   look_for_precommand(report_t& report, const string& verb);
void	     read_journal_files(session_t& session, const string& account);
function_t   look_for_command(report_t& report, const string& verb);
void         normalize_report_options(report_t& report, const string& verb);
void         create_output_stream(report_t& report);
void         invoke_command_verb(report_t&       report,
				 function_t&	 command,
				 string_iterator args_begin,
				 string_iterator args_end);

} // namespace ledger

#endif // _WORK_H
