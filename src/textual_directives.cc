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

#include "textual_internal.h"

namespace ledger {

using detail::instance_t;
using detail::application_t;
using detail::fixed_rate_t;

#if TIMELOG_SUPPORT

void instance_t::clock_in_directive(char* line, bool capitalized) {
  string datetime(line, 2, 19);

  char* p = skip_ws(line + 22);
  char* n = next_element(p, true);
  char* end = n ? next_element(n, true) : NULL;

  if (end && *end == ';')
    end = skip_ws(end + 1);
  else
    end = NULL;

  position_t position;
  position.pathname = context.pathname;
  position.beg_pos = context.line_beg_pos;
  position.beg_line = context.linenum;
  position.end_pos = context.curr_pos;
  position.end_line = context.linenum;
  position.sequence = context.sequence++;

  datetime_t when = parse_datetime(datetime);
  time_xact_t event(position, when, capitalized,
                    p ? top_account()->find_account(p) : NULL, n ? n : "", end ? end : "");

  timelog.clock_in(event);
}

void instance_t::clock_out_directive(char* line, bool capitalized) {
  string datetime(line, 2, 19);

  char* p = skip_ws(line + 22);
  char* n = next_element(p, true);
  char* end = n ? next_element(n, true) : NULL;

  if (end && *end == ';')
    end = skip_ws(end + 1);
  else
    end = NULL;

  position_t position;
  position.pathname = context.pathname;
  position.beg_pos = context.line_beg_pos;
  position.beg_line = context.linenum;
  position.end_pos = context.curr_pos;
  position.end_line = context.linenum;
  position.sequence = context.sequence++;

  datetime_t when = parse_datetime(datetime);
  time_xact_t event(position, when, capitalized,
                    p ? top_account()->find_account(p) : NULL, n ? n : "", end ? end : "");

  context.count += timelog.clock_out(event);
}

#endif // TIMELOG_SUPPORT

void instance_t::default_commodity_directive(char* line) {
  amount_t amt(skip_ws(line + 1));
  VERIFY(amt.valid());
  commodity_pool_t::current_pool->default_commodity = &amt.commodity();
  amt.commodity().add_flags(COMMODITY_KNOWN);
  // Set COMMODITY_STYLE_NO_MIGRATE to lock the format and prevent
  // observational formatting from overriding it (issue #1197)
  amt.commodity().add_flags(COMMODITY_STYLE_NO_MIGRATE);
}

void instance_t::default_account_directive(char* line) {
  context.journal->bucket = top_account()->find_account(skip_ws(line));
  context.journal->bucket->add_flags(ACCOUNT_KNOWN);
}

void instance_t::price_conversion_directive(char* line) {
  if (char* p = std::strchr(line + 1, '=')) {
    *p++ = '\0';
    amount_t::parse_conversion(line + 1, p);
  }
}

void instance_t::price_xact_directive(char* line) {
  std::optional<std::pair<commodity_t*, price_point_t>> point =
      commodity_pool_t::current_pool->parse_price_directive(skip_ws(line + 1));
  if (!point)
    throw parse_error(_("Pricing entry failed to parse"));
}

void instance_t::nomarket_directive(char* line) {
  char* p = skip_ws(line + 1);
  string symbol;
  commodity_t::parse_symbol(p, symbol);

  if (commodity_t* commodity = commodity_pool_t::current_pool->find_or_create(symbol))
    commodity->add_flags(COMMODITY_NOMARKET | COMMODITY_KNOWN);
}

void instance_t::option_directive(char* line) {
  // Check if this is a short option (single dash) vs long option (double dash)
  if (line[0] == '-' && line[1] != '-') {
    // Short options are not supported in configuration files
    throw_(option_error,
           _("Short options (e.g., -S) are not supported in configuration files. "
             "Please use long options (e.g., --sort) instead"));
  }

  char* p = next_element(line);
  if (!p) {
    p = std::strchr(line, '=');
    if (p)
      *p++ = '\0';
  }

  if (!process_option(context.pathname.string(), line + 2, *context.scope, p, line))
    throw_(option_error, _f("Illegal option --%1%") % (line + 2));
}

void instance_t::include_directive(char* line) {
  path filename;

  DEBUG("textual.include", "include: " << line);

  if (line[0] != '/' && line[0] != '\\' && line[0] != '~') {
    DEBUG("textual.include", "received a relative path");
    DEBUG("textual.include", "parent file path: " << context.pathname);
    path parent_path = context.pathname.parent_path();
    if (parent_path.empty()) {
      filename = context.current_directory / line;
    } else {
      filename = parent_path / line;
      DEBUG("textual.include", "normalized path: " << filename.string());
    }
  } else {
    filename = line;
  }

  filename = resolve_path(filename);
  DEBUG("textual.include", "resolved path: " << filename.string());

  mask_t glob;
  path parent_path = filename.parent_path();
  glob.assign_glob('^' + filename.filename().string() + '$');

  bool files_found = false;
  if (exists(parent_path)) {
    std::filesystem::directory_iterator end;

    // Sort parent_path since on some file systems it is unsorted.
    std::vector<path> sorted_parent_path;
    std::copy(std::filesystem::directory_iterator(parent_path), std::filesystem::directory_iterator(),
              std::back_inserter(sorted_parent_path));
    std::sort(sorted_parent_path.begin(), sorted_parent_path.end());

    for (std::vector<path>::const_iterator iter(sorted_parent_path.begin()),
         it_end(sorted_parent_path.end());
         iter != it_end; ++iter) {
      if (is_regular_file(*iter)) {
        string base = (*iter).filename().string();
        // Skip files with invalid UTF-8 in their names to avoid encoding errors
        if (!utf8::is_valid(base.begin(), base.end())) {
          DEBUG("textual.include", "Skipping file with invalid UTF-8 name: " << base);
          continue;
        }
        if (context.pathname == *iter) {
          DEBUG("textual.include", "Avoiding recursive include of: " << *iter);
          continue;
        }
        if (glob.match(base)) {
          journal_t* journal = context.journal;
          account_t* master = top_account();
          scope_t* scope = context.scope;
          std::size_t& errors = context.errors;
          std::size_t& count = context.count;
          std::size_t& sequence = context.sequence;

          DEBUG("textual.include", "Including: " << *iter);
          DEBUG("textual.include", "Master account: " << master->fullname());

          context_stack.push(*iter);

          context_stack.get_current().journal = journal;
          context_stack.get_current().master = master;
          context_stack.get_current().scope = scope;

          parse_context_t* save_current_context = journal->current_context;
          journal->current_context = &context_stack.get_current();

          try {
            instance_t instance(context_stack, context_stack.get_current(), this, no_assertions,
                                hash_type);
            instance.apply_stack.push_front(application_t("account", master));
            instance.parse();
          } catch (...) {
            errors += context_stack.get_current().errors;
            count += context_stack.get_current().count;
            sequence += context_stack.get_current().sequence;

            journal->current_context = save_current_context;
            context_stack.pop();
            throw;
          }

          errors += context_stack.get_current().errors;
          count += context_stack.get_current().count;
          sequence += context_stack.get_current().sequence;

          journal->current_context = save_current_context;
          context_stack.pop();

          files_found = true;
        }
      }
    }
  }

  if (!files_found)
    throw_(std::runtime_error, _f("File to include was not found: %1%") % filename);
}

void instance_t::apply_directive(char* line) {
  char* b = next_element(line);
  string keyword(line);
  if (!b)
    throw_(parse_error, _f("Directive 'apply %1%' requires an argument") % keyword);
  if (keyword == "account")
    apply_account_directive(b);
  else if (keyword == "tag")
    apply_tag_directive(b);
  else if (keyword == "fixed" || keyword == "rate")
    apply_rate_directive(b);
  else if (keyword == "year")
    apply_year_directive(b, true); // "apply year" uses apply_stack
}

void instance_t::apply_account_directive(char* line) {
  if (!line)
    throw_(parse_error, _("Directive 'apply account' requires an argument"));

  if (account_t* acct = top_account()->find_account(line))
    apply_stack.push_front(application_t("account", acct));
  else
    assert("Failed to create account" == NULL);
}

void instance_t::apply_tag_directive(char* line) {
  if (!line)
    throw_(parse_error, _("Directive 'apply tag' requires an argument"));

  string tag(trim_ws(line));

  if (tag.find(':') == string::npos)
    tag = string(":") + tag + ":";

  apply_stack.push_front(application_t("tag", tag));
}

void instance_t::apply_rate_directive(char* line) {
  if (!line)
    throw_(parse_error, _("Directive 'apply fixed/rate' requires an argument"));

  if (std::optional<std::pair<commodity_t*, price_point_t>> price_point =
          commodity_pool_t::current_pool->parse_price_directive(trim_ws(line), true, true)) {
    apply_stack.push_front(
        application_t("fixed", fixed_rate_t(price_point->first, price_point->second.price)));
  } else {
    throw_(std::runtime_error, _("Error in fixed directive"));
  }
}

void instance_t::apply_year_directive(char* line, bool use_apply_stack) {
  if (!line)
    throw_(parse_error, _("Directive 'apply year' requires an argument"));

  try {
    unsigned short year(lexical_cast<unsigned short>(skip_ws(line)));
    if (use_apply_stack) {
      // Used for "apply year" which needs "end apply"
      application_t entry("year", epoch);
      entry.saved_year_directive = year_directive_year;
      apply_stack.push_front(entry);
    }
    // Otherwise for plain "year" directive, don't use apply_stack - it's a permanent change
    DEBUG("times.epoch", "Setting current year to " << year);

    // Track the year directive separately
    year_directive_year = year;

    // Only set epoch if it's not already set (e.g., by --now)
    if (!epoch) {
      // This must be set to the last day of the year, otherwise partial
      // dates like "11/01" will refer to last year's November, not the
      // current year.
      epoch = datetime_t(date_t(year, 12, 31));
    }
  } catch (bad_lexical_cast&) {
    throw_(parse_error, _f("Argument '%1%' not a valid year") % skip_ws(line));
  }
}

void instance_t::end_apply_directive(char* kind) {
  char* b = kind ? next_element(kind) : NULL;
  string name(b ? b : "");

  if (apply_stack.size() <= 1) {
    if (name.empty()) {
      throw_(std::runtime_error,
             _("'end' or 'end apply' found, but no enclosing 'apply' directive"));
    } else {
      throw_(std::runtime_error,
             _f("'end apply %1%' found, but no enclosing 'apply' directive") % name);
    }
  }

  if (!name.empty() && name != apply_stack.front().label)
    throw_(std::runtime_error,
           _f("'end apply %1%' directive does not match 'apply %2%' directive") % name %
               apply_stack.front().label);

  if (std::holds_alternative<boost::optional<datetime_t>>(apply_stack.front().value)) {
    epoch = std::get<boost::optional<datetime_t>>(apply_stack.front().value);
    year_directive_year = apply_stack.front().saved_year_directive;
  }

  apply_stack.pop_front();
}

void instance_t::account_directive(char* line) {
  std::istream::pos_type beg_pos = context.line_beg_pos;
  std::size_t beg_linenum = context.linenum;

  char* p = skip_ws(line);
  account_t* account = context.journal->register_account(p, NULL, top_account());
  account->add_flags(ACCOUNT_KNOWN);
  unique_ptr<auto_xact_t> ae;

  while (peek_whitespace_line()) {
    read_line(line);
    char* q = skip_ws(line);
    if (!*q)
      break;

    char* b = next_element(q);
    string keyword(q);
    // Ensure there's an argument for the directives that need one.
    if (!b && keyword != "default")
      throw_(parse_error, _f("Account directive '%1%' requires an argument") % keyword);

    if (keyword == "alias") {
      account_alias_directive(account, b);
    } else if (keyword == "payee") {
      account_payee_directive(account, b);
    } else if (keyword == "value") {
      account_value_directive(account, b);
    } else if (keyword == "default") {
      account_default_directive(account);
    } else if (keyword == "assert" || keyword == "check") {
      keep_details_t keeper(true, true, true);
      expr_t expr(string("account == \"") + account->fullname() + "\"");
      predicate_t pred(expr.get_op(), keeper);

      if (!ae.get()) {
        ae.reset(new auto_xact_t(pred));

        ae->pos = position_t();
        ae->pos->pathname = context.pathname;
        ae->pos->beg_pos = beg_pos;
        ae->pos->beg_line = beg_linenum;
        ae->pos->sequence = context.sequence++;
        ae->check_exprs = expr_t::check_expr_list();
      }

      ae->check_exprs->push_back(expr_t::check_expr_pair(
          expr_t(b), keyword == "assert" ? expr_t::EXPR_ASSERTION : expr_t::EXPR_CHECK));
    } else if (keyword == "eval" || keyword == "expr") {
      // jww (2012-02-27): Make account into symbol scopes so that this
      // can be used to override definitions within the account.
      bind_scope_t bound_scope(*context.scope, *account);
      expr_t(b).calc(bound_scope);
    } else if (keyword == "note") {
      account->note = b;
    }
  }

  if (ae.get()) {
    ae->journal = context.journal;
    ae->pos->end_pos = in.tellg();
    ae->pos->end_line = context.linenum;

    context.journal->auto_xacts.push_back(std::move(ae));
  }
}

void instance_t::account_alias_directive(account_t* account, string alias) {
  // Once we have an alias name (alias) and the target account
  // (account), add a reference to the account in the `account_aliases'
  // map, which is used by the post parser to resolve alias references.
  trim(alias);
  // Ensure that no alias like "alias Foo=Foo" is registered.
  if (alias == account->fullname()) {
    throw_(parse_error, _f("Illegal alias %1%=%2%") % alias % account->fullname());
  }
  std::pair<accounts_map::iterator, bool> result =
      context.journal->account_aliases.insert(accounts_map::value_type(alias, account));
  if (!result.second)
    (*result.first).second = account;
}

void instance_t::alias_directive(char* line) {
  if (char* e = std::strchr(line, '=')) {
    char* z = e - 1;
    while (std::isspace(static_cast<unsigned char>(*z)))
      *z-- = '\0';
    *e++ = '\0';
    e = skip_ws(e);

    account_alias_directive(top_account()->find_account(e), line);
  }
}

void instance_t::account_payee_directive(account_t* account, string payee) {
  trim(payee);
  context.journal->payees_for_unknown_accounts.push_back(account_mapping_t(mask_t(payee), account));
}

void instance_t::account_default_directive(account_t* account) {
  context.journal->bucket = account;
}

void instance_t::account_value_directive(account_t* account, string expr_str) {
  account->value_expr = expr_t(expr_str);
}

void instance_t::payee_directive(char* line) {
  string payee = context.journal->register_payee(line);

  while (peek_whitespace_line()) {
    read_line(line);
    char* p = skip_ws(line);
    if (!*p)
      break;

    char* b = next_element(p);
    string keyword(p);
    if (!b)
      throw_(parse_error, _f("Payee directive '%1%' requires an argument") % keyword);

    if (keyword == "alias")
      payee_alias_directive(payee, b);
    else if (keyword == "uuid")
      payee_uuid_directive(payee, b);
  }
}

void instance_t::payee_alias_directive(const string& payee, string alias) {
  trim(alias);
  context.journal->payee_alias_mappings.push_back(payee_alias_mapping_t(mask_t(alias), payee));
}

void instance_t::payee_uuid_directive(const string& payee, string uuid) {
  trim(uuid);
  context.journal->payee_uuid_mappings[uuid] = payee;
}

void instance_t::commodity_directive(char* line) {
  char* p = skip_ws(line);
  string symbol;
  commodity_t::parse_symbol(p, symbol);

  if (commodity_t* commodity = commodity_pool_t::current_pool->find_or_create(symbol)) {
    context.journal->register_commodity(*commodity, 0);

    while (peek_whitespace_line()) {
      read_line(line);
      char* q = skip_ws(line);
      if (!*q)
        break;

      char* b = next_element(q);
      string keyword(q);
      // Ensure there's an argument for the directives that need one.
      if (!b && keyword != "nomarket" && keyword != "default")
        throw_(parse_error, _f("Commodity directive '%1%' requires an argument") % keyword);

      if (keyword == "alias")
        commodity_alias_directive(*commodity, b);
      else if (keyword == "value")
        commodity_value_directive(*commodity, b);
      else if (keyword == "format")
        commodity_format_directive(*commodity, b);
      else if (keyword == "nomarket")
        commodity_nomarket_directive(*commodity);
      else if (keyword == "default")
        commodity_default_directive(*commodity);
      else if (keyword == "note")
        commodity->set_note(string(b));
    }
  }
}

void instance_t::commodity_alias_directive(commodity_t& comm, string alias) {
  trim(alias);
  // Strip surrounding quotes if present, matching how commodity names are parsed
  if (alias.length() > 2 && alias[0] == '"' && alias[alias.length() - 1] == '"')
    alias = alias.substr(1, alias.length() - 2);
  commodity_pool_t::current_pool->alias(alias, comm);
}

void instance_t::commodity_value_directive(commodity_t& comm, string expr_str) {
  comm.set_value_expr(expr_t(expr_str));
}

void instance_t::commodity_format_directive(commodity_t& comm, string format) {
  // jww (2012-02-27): A format specified this way should turn off
  // observational formatting.
  trim(format);
  amount_t amt;
  (void)amt.parse(format, PARSE_NO_REDUCE);
  if (amt.commodity() != comm)
    throw_(parse_error,
           _f("commodity directive symbol %1% and format directive symbol %2% should be the same") %
               comm.symbol() % amt.commodity().symbol());
  // Explicitly set the precision from the format directive. The parse() call
  // above only updates precision when the new precision is greater, but a
  // format directive should be authoritative and set the exact precision.
  amt.commodity().set_precision(amt.precision());
  amt.commodity().add_flags(COMMODITY_STYLE_NO_MIGRATE);
  VERIFY(amt.valid());
}

void instance_t::commodity_nomarket_directive(commodity_t& comm) {
  comm.add_flags(COMMODITY_NOMARKET);
}

void instance_t::commodity_default_directive(commodity_t& comm) {
  commodity_pool_t::current_pool->default_commodity = &comm;
}

void instance_t::tag_directive(char* line) {
  char* p = skip_ws(line);
  context.journal->register_metadata(p, NULL_VALUE, 0);

  while (peek_whitespace_line()) {
    read_line(line);
    char* q = skip_ws(line);
    if (!*q)
      break;

    char* b = next_element(q);
    string keyword(q);
    if (keyword == "assert" || keyword == "check") {
      context.journal->tag_check_exprs.insert(tag_check_exprs_map::value_type(
          string(p), expr_t::check_expr_pair(expr_t(b), keyword == "assert" ? expr_t::EXPR_ASSERTION
                                                                            : expr_t::EXPR_CHECK)));
    }
  }
}

void instance_t::eval_directive(char* line) {
  expr_t expr(line);
  expr.calc(*context.scope);
}

void instance_t::assert_directive(char* line) {
  expr_t expr(line);
  if (!expr.calc(*context.scope).to_boolean())
    throw_(parse_error, _f("Assertion failed: %1%") % line);
}

void instance_t::check_directive(char* line) {
  expr_t expr(line);
  if (!expr.calc(*context.scope).to_boolean())
    context.warning(_f("Check failed: %1%") % line);
}

void instance_t::value_directive(char* line) {
  context.journal->value_expr = expr_t(line);
}

void instance_t::comment_directive(char* line) {
  while (in.good() && !in.eof()) {
    if (read_line(line) > 0) {
      std::string buf(line);
      if (starts_with(buf, "end comment") || starts_with(buf, "end test"))
        break;
    }
  }
}

#if HAVE_BOOST_PYTHON

void instance_t::import_directive(char* line) {
  string module_name(line);
  trim(module_name);
  python_session->import_option(module_name);
}

void instance_t::python_directive(char* line) {
  std::ostringstream script;

  if (line)
    script << skip_ws(line) << '\n';

  std::size_t indent = 0;

  while (peek_whitespace_line() || peek_blank_line()) {
    if (read_line(line) > 0) {
      if (!indent) {
        const char* p = line;
        while (*p && std::isspace(static_cast<unsigned char>(*p))) {
          ++indent;
          ++p;
        }
      }

      const char* p = line;
      for (std::size_t i = 0; i < indent; i++) {
        if (std::isspace(static_cast<unsigned char>(*p)))
          ++p;
        else
          break;
      }

      if (*p)
        script << p << '\n';
    }
  }

  if (!python_session->is_initialized)
    python_session->initialize();

  python_session->main_module->define_global(
      "journal", boost::python::object(boost::python::ptr(context.journal)));
  python_session->eval(script.str(), python_interpreter_t::PY_EVAL_MULTI);
}

#else

void instance_t::import_directive(char*) {
  throw_(parse_error, _("'import' directive seen, but Python support is missing"));
}

void instance_t::python_directive(char*) {
  throw_(parse_error, _("'python' directive seen, but Python support is missing"));
}

#endif // HAVE_BOOST_PYTHON

bool instance_t::general_directive(char* line) {
  std::string buf(line);

  char* p = &buf[0];
  char* arg = next_element(&buf[0]);

  if (*p == '@' || *p == '!')
    p++;

  // Ensure there's an argument for all directives that need one.
  if (!arg && std::strcmp(p, "comment") != 0 && std::strcmp(p, "end") != 0 &&
      std::strcmp(p, "python") != 0 && std::strcmp(p, "test") != 0 && *p != 'Y') {
    throw_(parse_error, _f("Directive '%1%' requires an argument") % p);
  }

  switch (*p) {
  case 'a':
    if (std::strcmp(p, "account") == 0) {
      account_directive(arg);
      return true;
    } else if (std::strcmp(p, "alias") == 0) {
      alias_directive(arg);
      return true;
    } else if (std::strcmp(p, "apply") == 0) {
      apply_directive(arg);
      return true;
    } else if (std::strcmp(p, "assert") == 0) {
      assert_directive(arg);
      return true;
    }
    break;

  case 'b':
    if (std::strcmp(p, "bucket") == 0) {
      default_account_directive(arg);
      return true;
    }
    break;

  case 'c':
    if (std::strcmp(p, "check") == 0) {
      check_directive(arg);
      return true;
    } else if (std::strcmp(p, "comment") == 0) {
      comment_directive(arg);
      return true;
    } else if (std::strcmp(p, "commodity") == 0) {
      commodity_directive(arg);
      return true;
    }
    break;

  case 'd':
    if (std::strcmp(p, "def") == 0 || std::strcmp(p, "define") == 0) {
      eval_directive(arg);
      return true;
    }
    break;

  case 'e':
    if (std::strcmp(p, "end") == 0) {
      end_apply_directive(arg);
      return true;
    } else if (std::strcmp(p, "expr") == 0 || std::strcmp(p, "eval") == 0) {
      eval_directive(arg);
      return true;
    }
    break;

  case 'i':
    if (std::strcmp(p, "include") == 0) {
      include_directive(arg);
      return true;
    } else if (std::strcmp(p, "import") == 0) {
      import_directive(arg);
      return true;
    }
    break;

  case 'p':
    if (std::strcmp(p, "payee") == 0) {
      payee_directive(arg);
      return true;
    } else if (std::strcmp(p, "python") == 0) {
      python_directive(arg);
      return true;
    }
    break;

  case 't':
    if (std::strcmp(p, "tag") == 0) {
      tag_directive(arg);
      return true;
    } else if (std::strcmp(p, "test") == 0) {
      comment_directive(arg);
      return true;
    }
    break;

  case 'v':
    if (std::strcmp(p, "value") == 0) {
      value_directive(arg);
      return true;
    }
    break;

  case 'y':
    if (std::strcmp(p, "year") == 0) {
      apply_year_directive(arg);
      return true;
    }
    break;
  }

  if (expr_t::ptr_op_t op = lookup(symbol_t::DIRECTIVE, p)) {
    call_scope_t args(*this);
    args.push_back(string_value(p));
    op->as_function()(args);
    return true;
  }

  return false;
}

} // namespace ledger
