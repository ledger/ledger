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

#include <ledger.h>

#include "work.h"

namespace ledger {

void handle_debug_options(int argc, char * argv[])
{
  for (int i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (std::strcmp(argv[i], "--verify") == 0) {
#if defined(VERIFY_ON)
	verify_enabled = true; // global in utils.h
#endif
      }
      else if (std::strcmp(argv[i], "--verbose") == 0 ||
	       std::strcmp(argv[i], "-v") == 0) {
#if defined(LOGGING_ON)
	_log_level = LOG_INFO; // global in utils.h
#endif
      }
      else if (i + 1 < argc && std::strcmp(argv[i], "--debug") == 0) {
#if defined(DEBUG_ON)
	_log_level    = LOG_DEBUG; // global in utils.h
	_log_category = argv[i + 1]; // global in utils.h
	i++;
#endif
      }
      else if (i + 1 < argc && std::strcmp(argv[i], "--trace") == 0) {
#if defined(TRACING_ON)
	_log_level   = LOG_TRACE; // global in utils.h
	try {
	  // global in utils.h
	  _trace_level = boost::lexical_cast<int>(argv[i + 1]);
	}
	catch (const boost::bad_lexical_cast& e) {
	  throw std::logic_error("Argument to --trace must be an integer");
	}
	i++;
#endif
      }
    }
  }
}

void read_environment_settings(report_t& report, char * envp[])
{
  TRACE_START(environment, 1, "Processed environment variables");

  process_environment(const_cast<const char **>(envp), "LEDGER_",
		      report);

#if 1
  // These are here for backwards compatability, but are deprecated.

  if (const char * p = std::getenv("LEDGER"))
    process_option("file", report, p, "LEDGER");
  if (const char * p = std::getenv("LEDGER_INIT"))
    process_option("init-file", report, p, "LEDGER_INIT");
  if (const char * p = std::getenv("PRICE_HIST"))
    process_option("price-db", report, p, "PRICE_HIST");
  if (const char * p = std::getenv("PRICE_EXP"))
    process_option("price-exp", report, p, "PRICE_EXP");
#endif

  TRACE_FINISH(environment, 1);
}

strings_list
read_command_line_arguments(report_t& report, int argc, char * argv[])
{
  TRACE_START(arguments, 1, "Processed command-line arguments");

  strings_list args;
  process_arguments(argc - 1, argv + 1, report, args);

  if (args.empty()) {
    help(std::cout);
    throw int(1);
  }

  TRACE_FINISH(arguments, 1);

  return args;
}

void normalize_session_options(session_t& session)
{
  if (! session.cache_file)
    session.use_cache = false;

  DEBUG("ledger.session.cache", "1. use_cache = " << session.use_cache);

  if (std::find(session.data_files.begin(),
		session.data_files.end(), *session.cache_file) !=
      session.data_files.end())
    session.use_cache = false;

  DEBUG("ledger.session.cache", "2. use_cache = " << session.use_cache);

  INFO("Initialization file is " << session.init_file->string());
  INFO("Price database is " << session.price_db->string());
  INFO("Binary cache is " << session.cache_file->string());

  foreach (const path& pathname, session.data_files)
    INFO("Journal file is " << pathname.string());

  if (! session.use_cache)
    INFO("Binary cache mechanism will not be used");
}

function_t look_for_precommand(report_t& report, const string& verb)
{
  if (expr_t::ptr_op_t def = report.lookup(string("ledger_precmd_") + verb))
    return def->as_function();
  else
    return function_t();
}

journal_t * read_journal_files(session_t& session, const string& account)
{
  INFO_START(journal, "Read journal file");

  journal_t * journal(session.create_journal());

  std::size_t count = session.read_data(*journal, account);
  if (count == 0)
    throw_(parse_error, "Failed to locate any journal entries; "
	   "did you specify a valid file with -f?");

  INFO_FINISH(journal);

  INFO("Found " << count << " entries");

  TRACE_FINISH(entry_text, 1);
  TRACE_FINISH(entry_details, 1);
  TRACE_FINISH(entry_xacts, 1);
  TRACE_FINISH(entries, 1);
  TRACE_FINISH(session_parser, 1);
  TRACE_FINISH(parsing_total, 1);

  return journal;
}

function_t look_for_command(report_t& report, const string& verb)
{
  if (expr_t::ptr_op_t def = report.lookup(string("ledger_cmd_") + verb))
    return def->as_function();
  else
    return function_t();
}

void normalize_report_options(report_t& report, const string& verb)
{
#if 1
  // Patch up some of the reporting options based on what kind of
  // command it was.

  // jww (2008-08-14): This code really needs to be rationalized away
  // for 3.0.

  if (verb == "print" || verb == "entry" || verb == "dump") {
    report.show_related     = true;
    report.show_all_related = true;
  }
  else if (verb == "equity") {
    report.show_subtotal = true;
  }
  else if (report.show_related) {
    if (verb[0] == 'r') {
      report.show_inverted = true;
    } else {
      report.show_subtotal    = true;
      report.show_all_related = true;
    }
  }

  if (verb[0] != 'b' && verb[0] != 'r')
    amount_t::keep_base = true;

  // Setup the default value for the display predicate

  if (report.display_predicate.empty()) {
    if (verb[0] == 'b') {
      if (! report.show_empty)
	report.display_predicate = "total";
      if (! report.show_subtotal) {
	if (! report.display_predicate.empty())
	  report.display_predicate += "&";
	report.display_predicate += "depth<=1";
      }
    }
    else if (verb == "equity") {
      report.display_predicate = "amount_expr"; // jww (2008-08-14): ???
    }
    else if (verb[0] == 'r' && ! report.show_empty) {
      report.display_predicate = "amount";
    }
  }
#endif

  // Now setup the various formatting strings

  // jww (2008-08-14): I hear a song, and it's sound is "HaAaaCcK"

#if 0
  if (! date_output_format.empty())
    date_t::output_format = date_output_format;
#endif

  amount_t::keep_price = report.keep_price;
  amount_t::keep_date  = report.keep_date;
  amount_t::keep_tag   = report.keep_tag;

  if (! report.report_period.empty() && ! report.sort_all)
    report.entry_sort = true;
}

void create_output_stream(report_t& report)
{
  report.output_stream.initialize(report.output_file, report.pager_path);
}

void invoke_command_verb(report_t&       report,
			 function_t&	 command,
			 string_iterator args_begin,
			 string_iterator args_end)
{
  // Create an argument scope containing the report command's
  // arguments, and then invoke the command.

  call_scope_t command_args(report);

  for (string_iterator i = args_begin; i != args_end; i++)
    command_args.push_back(string_value(*i));

  INFO_START(command, "Finished executing command");

  command(command_args);

  INFO_FINISH(command);
}

void write_binary_cache(session_t& session, journal_t * journal)
{
  // Write out the binary cache, if need be

  if (session.use_cache && session.cache_dirty && session.cache_file) {
    TRACE_START(binary_cache, 1, "Wrote binary journal file");

    ofstream stream(*session.cache_file);
#if 0
    // jww (2009-01-31): NYI
    journal->write(stream);
#endif

    TRACE_FINISH(binary_cache, 1);
  }
}

} // namespace ledger
