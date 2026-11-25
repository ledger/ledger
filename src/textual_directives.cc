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

/**
 * @file   textual_directives.cc
 * @author John Wiegley
 * @brief  Directive parsing for the textual journal parser.
 *
 * @ingroup data
 *
 * This file implements all of the non-transaction directives that can appear
 * in a Ledger journal file.  Directives configure the journal's metadata
 * (accounts, commodities, payees, tags), control parsing scope (`apply`,
 * `include`, `year`), embed expressions and assertions, and handle time
 * tracking (clock-in/out).
 *
 * The directives fall into several categories:
 *
 *   **Declaration directives** -- `account`, `commodity`, `payee`, `tag`
 *   Pre-declare entities and attach sub-properties (alias, format, etc.).
 *
 *   **Scope directives** -- `apply account`, `apply tag`, `apply fixed`,
 *   `apply year`, `end apply`
 *   Push/pop entries on the apply stack to affect all subsequent items.
 *
 *   **Legacy single-character directives** -- `A`, `C`, `D`, `N`, `P`, `Y`
 *   Inherited from the original Ledger format; each has a word-based
 *   equivalent in the general_directive() dispatch table.
 *
 *   **File directives** -- `include`, `comment`/`test`, `--option`
 *   Control which files are parsed and how options are set.
 *
 *   **Expression directives** -- `eval`/`expr`/`def`/`define`, `assert`,
 *   `check`, `value`
 *   Evaluate or verify value expressions at parse time.
 *
 *   **Python directives** -- `import`, `python`
 *   Execute Python code (requires Boost.Python).
 *
 *   **Time tracking** -- `i`/`I` (clock-in), `o`/`O` (clock-out)
 *   Record time-log events that are converted to transactions.
 *
 * The general_directive() method dispatches word directives through the
 * word_directives table, which maps each keyword to its handler and says
 * whether the keyword may stand alone; when it finds no match,
 * read_next_directive() tries the legacy single-character directives.
 */

#include "textual_internal.h"

namespace ledger {

using detail::application_t;
using detail::directory_cache_t;
using detail::directory_listing_t;
using detail::fixed_rate_t;
using detail::instance_t;

/*--- Time Tracking Directives ---*/

time_xact_t instance_t::parse_clock_event(char* line, bool capitalized) {
  if (std::strlen(line) < 21)
    throw_(parse_error, _("Clock-in/out directive is too short"));
  string datetime(line, 2, 19); // NOLINT(bugprone-unused-local-non-trivial-variable)

  // line[21] ends the time: either the NUL of a bare "o DATE TIME" line or the
  // separator before the account.  Never read past the NUL: the bytes beyond it
  // belong to an earlier line in the reused line buffer.
  char* p = line[21] != '\0' ? skip_ws(line + 22) : line + 21;
  char* n = next_element(p, true);
  char* end = n ? next_element(n, true) : nullptr;

  if (end && *end == ';')
    end = skip_ws(end + 1);
  else
    end = nullptr;

  position_t position;
  position.pathname = context.pathname_ref;
  position.beg_pos = context.line_beg_pos;
  position.beg_line = context.linenum;
  position.end_pos = context.curr_pos;
  position.end_line = context.linenum;
  position.sequence = context.sequence++;

  datetime_t when = parse_datetime(datetime);
  return time_xact_t(position, when, capitalized,
                     line[21] != '\0' ? top_account()->find_account(p) : nullptr, n ? n : "",
                     end ? end : "");
}

void instance_t::clock_in_directive(char* line, bool capitalized) {
  timelog.clock_in(parse_clock_event(line, capitalized));
}

void instance_t::clock_out_directive(char* line, bool capitalized) {
  context.count += timelog.clock_out(parse_clock_event(line, capitalized));
}

/*--- Legacy Single-Character Directives ---*/

void instance_t::default_commodity_directive(char* line) {
  // Parse with PARSE_APPLY_STYLE_TO_BARE so that a bare D directive
  // like "D 1.000,00" (no commodity symbol) transfers its format
  // (thousands separator, decimal comma) and precision to
  // null_commodity.  This makes subsequent bare amounts display with
  // the same format (issue #761).  PARSE_DECLARES_STYLE says the
  // directive declares the style, so a decimal comma that only its
  // digits suggest is no conflict with an earlier decimal period.
  amount_t amt;
  std::istringstream stream(skip_ws(line + 1));
  (void)amt.parse(stream, PARSE_APPLY_STYLE_TO_BARE | PARSE_DECLARES_STYLE);
  VERIFY(amt.valid());
  commodity_pool_t::current_pool->default_commodity = &amt.commodity();
  amt.commodity().add_flags(COMMODITY_KNOWN);
  // Set COMMODITY_STYLE_NO_MIGRATE to lock the format and prevent
  // observational formatting from overriding it (issue #1197).  When
  // the directive is bare, amt.commodity() returns null_commodity, so
  // this also locks null_commodity's newly-installed style.
  amt.commodity().add_flags(COMMODITY_STYLE_NO_MIGRATE);
}

void instance_t::default_account_directive(char* line) {
  context.journal->bucket =
      context.journal->register_account(skip_ws(line), nullptr, top_account());
  context.journal->bucket->add_flags(ACCOUNT_KNOWN);
}

void instance_t::price_conversion_directive(char* line) {
  if (char* p = std::strchr(line + 1, '=')) {
    *p++ = '\0';

    // Parse the larger and smaller amounts to register a price in the
    // commodity history graph, so that -X (--exchange) can use
    // C-directive rates.  Only journal-level C directives get this
    // treatment; built-in unit conversions (e.g. hours/minutes) do not.
    amount_t larger, smaller;
    (void)larger.parse(line + 1, PARSE_NO_REDUCE);
    (void)smaller.parse(p, PARSE_NO_REDUCE);
    if (larger.commodity() && smaller.commodity()) {
      amount_t per_unit_price(smaller);
      per_unit_price /= larger.number();
      larger.commodity().add_price(datetime_t(date_t(1970, 1, 1)), per_unit_price, true);
    }

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
  string symbol; // NOLINT(bugprone-unused-local-non-trivial-variable)
  commodity_t::parse_symbol(p, symbol);

  commodity_t* commodity = commodity_pool_t::current_pool->find_or_create(symbol);
  commodity->add_flags(COMMODITY_NOMARKET | COMMODITY_KNOWN);
}

/*--- File and Option Directives ---*/

void instance_t::option_directive(char* line) {
  // Check if this is a short option (single dash) vs long option (double dash)
  if (line[0] == '-' && line[1] != '-') {
    // Short options are not supported in configuration files
    throw_(option_error, _("Short options (e.g., -S) are not supported in configuration files. "
                           "Please use long options (e.g., --sort) instead"));
  }

  char* p = next_element(line);
  if (!p) {
    p = std::strchr(line, '=');
    if (p)
      *p++ = '\0';
  }

  // Strip outer quotes and process escape sequences in the option value.
  // Double-quoted strings: process only \" -> " and \\ -> \ (leave all other
  // escape sequences such as \n and \t for the format parser).
  // Single-quoted strings: strip quotes with no escape processing at all.
  string unquoted;
  if (p && *p == '"') {
    const char* s = p + 1;
    while (*s && *s != '"') {
      if (*s == '\\' && (*(s + 1) == '"' || *(s + 1) == '\\')) {
        unquoted += *(s + 1);
        s += 2;
      } else {
        unquoted += *s++;
      }
    }
    p = const_cast<char*>(unquoted.c_str());
  } else if (p && *p == '\'') {
    const char* s = p + 1;
    while (*s && *s != '\'')
      unquoted += *s++;
    p = const_cast<char*>(unquoted.c_str());
  }

  if (!process_option(context.pathname().string(), line + 2, *context.scope, p, line))
    throw_(option_error, _f("Illegal option --%1%") % (line + 2));
}

namespace {

/// A listing taken this soon after its directory's modification time
/// cannot be trusted: a file created within the same clock tick leaves the
/// time unchanged, and some filesystems store it in two-second steps.
const auto racy_window = std::chrono::seconds(2);

/// Return the listing of @p dir, listing it when the cache has no
/// listing that can be trusted.
///
/// A listing is reused while the directory's modification time is
/// unchanged and old enough to be trusted; a parse-time writer (a `python`
/// block) that adds a file changes the time, so the next include sees it.
/// The time is read before the directory is listed, so a change that lands
/// in between costs one extra listing, never a stale one.  The entries are
/// full paths, sorted.  A directory that cannot be listed (one that may be
/// searched but not read, say) is cached with its error and no entries; an
/// error part-way through a listing counts as a failed listing.
const directory_listing_t& list_directory(directory_cache_t& cache, const path& dir) {
  std::error_code ec;
  const auto stamp = std::filesystem::last_write_time(dir, ec);

  directory_listing_t& element = cache[dir];
  if (!ec && !element.racy && element.stamp == stamp)
    return element;

  element.entries.clear();
  element.error.clear();
  element.stamp = ec ? std::filesystem::file_time_type::min() : stamp;
  element.racy = ec || std::filesystem::file_time_type::clock::now() - stamp < racy_window;

  std::error_code list_ec;
  std::filesystem::directory_iterator it(dir, list_ec);
  for (; !list_ec && it != std::filesystem::directory_iterator(); it.increment(list_ec))
    element.entries.push_back(it->path());
  if (list_ec) {
    element.entries.clear();
    element.error = list_ec;
  } else {
    std::sort(element.entries.begin(), element.entries.end());
  }
  return element;
}

/// Recursively expand a glob path by walking its components.
///
/// Each directory on the path is listed at most once per parse (see
/// list_directory).  A component holding a glob metacharacter is matched
/// against every entry of its directory as a shell glob; a component
/// without one is looked up by exact name in that listing instead.  Both
/// compare names exactly, using `mask_t::assign_glob` (which is
/// case-sensitive) rather than a filesystem lookup, so `include FOO.dat`
/// does not silently match a differently-cased `foo.dat` on
/// case-insensitive filesystems (issue #1660).  Nor does
/// `--ignore-diacritics` let `include cafe.dat` match `café.dat`.  A
/// directory that cannot be listed (searchable but unreadable) still lets
/// a component without glob characters through by direct lookup, so
/// case-exactness is enforced only for directories ledger can list; a glob
/// component under such a directory is a parse error.  Matched entries
/// drive the next level of recursion; the final component contributes
/// regular files to @p results.
///
/// This allows patterns with glob metacharacters in any component --
/// not just the filename -- to match across multiple directory levels
/// (issue #1214).
void expand_glob_path(directory_cache_t& cache, const path& base,
                      const std::vector<path>& components, std::size_t index,
                      std::vector<path>& results) {
  if (index == components.size())
    return;

  if (!exists(base) || !is_directory(base))
    return;

  const string comp = components[index].string();
  mask_t glob;
  glob.assign_glob(comp);

  const directory_listing_t& listing = list_directory(cache, base);

  // The characters mask_t::assign_glob treats specially.
  const bool literal = comp.find_first_of("*?[\\") == string::npos;
  std::vector<path> single;
  const std::vector<path>* candidates = &listing.entries;
  if (literal) {
    // No glob character: the pattern matches exactly this name, so look it up
    // instead of matching every entry.  A directory that could not be listed
    // gets the same one candidate, which the loop below confirms on disk.
    path wanted = base / comp;
    if (listing.error || std::binary_search(listing.entries.begin(), listing.entries.end(), wanted))
      single.push_back(wanted);
    candidates = &single;
  } else if (listing.error) {
    throw_(parse_error, _f("Cannot list directory %1% to expand include pattern '%2%': %3%") %
                            base.string() % comp % listing.error.message());
  }

  const bool is_last = (index + 1 == components.size());

  for (const path& entry : *candidates) {
    string name = entry.filename().string();
    if (!utf8::is_valid(name.begin(), name.end())) {
      DEBUG("textual.include", "Skipping file with invalid UTF-8 name: " << name);
      continue;
    }
    if (!glob.match(name))
      continue;

    if (is_last) {
      if (is_regular_file(entry))
        results.push_back(entry);
    } else if (is_directory(entry)) {
      expand_glob_path(cache, entry, components, index + 1, results);
    }
  }
}

/// Split @p filename into a starting @p base directory and the list of
/// remaining path components that may contain glob metacharacters.
///
/// For absolute paths the base is the root path (e.g. `/`); for relative
/// paths the base is the current directory so that enumeration works when
/// the first component is itself a glob.
void split_glob_path(const path& filename, path& base, std::vector<path>& components) {
  if (filename.has_root_path()) {
    base = filename.root_path();
    for (const path& c : filename.relative_path())
      components.push_back(c);
  } else {
    base = path(".");
    for (const path& c : filename)
      components.push_back(c);
  }
}

} // namespace

/**
 * @brief Process an `include` directive.
 *
 * The include path may contain shell glob wildcards in any path
 * component, not just the filename -- wildcards in parent directory
 * components are expanded as well, so a single directive can match
 * files across multiple directory levels.  Paths are resolved relative
 * to the directory of the current file.  Recursive includes are
 * detected and skipped; a hard depth limit of 100 prevents runaway
 * include chains.
 *
 * For each matched file, a new instance_t is created and parsed.  The
 * parent file's year/epoch state is saved and restored after the
 * include so that `year` directives in included files do not leak back.
 */
void instance_t::include_directive(char* line) {
  int include_depth = 0;
  for (instance_t* p = parent; p; p = p->parent)
    ++include_depth;
  if (include_depth > 100)
    throw_(parse_error, _("Maximum include depth exceeded (100)"));

  // Strip balanced surrounding quotes (" or ') from the include path so that
  // both `include "file.ledger"` and `include 'file.ledger'` work as expected.
  // We also avoid the confusing case where a stray trailing quote (e.g.
  // `include test.ledger"`) is silently embedded in the filename.
  {
    std::size_t line_len = std::strlen(line);
    if (line_len >= 2 && (line[0] == '"' || line[0] == '\'') && line[line_len - 1] == line[0]) {
      line[line_len - 1] = '\0';
      ++line;
    }
  }

  path filename;

  DEBUG("textual.include", "include: " << line);

  // Detect absolute paths so they are not joined with the parent directory.
  // In addition to the POSIX-style leading '/', '\', and '~' checks, we
  // consult std::filesystem::path::is_absolute() which recognizes Windows
  // drive-letter paths such as `D:/ledger/main.ledger` (issue #1670).
  path include_path(line);
  if (line[0] != '/' && line[0] != '\\' && line[0] != '~' && !include_path.is_absolute()) {
    DEBUG("textual.include", "received a relative path");
    DEBUG("textual.include", "parent file path: " << context.pathname());
    path parent_path = context.pathname().parent_path();
    if (parent_path.empty()) {
      filename = context.current_directory / line;
    } else {
      filename = parent_path / line;
      DEBUG("textual.include", "normalized path: " << filename.string());
    }
  } else {
    filename = include_path;
  }

  filename = resolve_path(filename);
  DEBUG("textual.include", "resolved path: " << filename.string());

  path base;
  std::vector<path> components;
  split_glob_path(filename, base, components);

  std::vector<path> matched_files;
  expand_glob_path(listing_cache(), base, components, 0, matched_files);

  bool files_found = false;
  for (const path& matched : matched_files) {
    if (context.pathname() == matched) {
      DEBUG("textual.include", "Avoiding recursive include of: " << matched);
      continue;
    }

    journal_t* journal = context.journal;
    account_t* master = top_account();
    scope_t* scope = context.scope;

    DEBUG("textual.include", "Including: " << matched);
    DEBUG("textual.include", "Master account: " << master->fullname());

    parse_context_stack_t::pushed_t pushed(context_stack, matched);

    context_stack.get_current().journal = journal;
    context_stack.get_current().master = master;
    context_stack.get_current().scope = scope;

    // Save year state so that year/Y directives inside the included
    // file do not bleed back into the parent file after the include
    // returns.  This mirrors what end_apply_directive does for the
    // "apply year" / "end apply year" pair.
    scoped_assign<optional<datetime_t>> epoch_guard(epoch);
    scoped_assign<optional<int>> year_guard(year_directive_year);

    // Whether the included file parses or throws, fold its counts into this
    // file's and make this file's context current again.  Declared after
    // `pushed`, this runs first, while the included file's context is still
    // on the stack.
    struct restore_t {
      parse_context_t& parent;
      parse_context_t& child;
      journal_t* journal;
      parse_context_t* saved_current_context;

      ~restore_t() {
        parent.errors += child.errors;
        parent.count += child.count;
        parent.sequence += child.sequence;
        journal->current_context = saved_current_context;
      }
    } restore{context, context_stack.get_current(), journal, journal->current_context};

    journal->current_context = &context_stack.get_current();
    instance_t instance(context_stack, context_stack.get_current(), this, hash_type);
    instance.apply_stack.push_front(application_t("account", master));
    instance.parse();

    files_found = true;
  }

  if (!files_found)
    throw_(std::runtime_error, _f("File to include was not found: %1%") % filename.string());
}

/*--- Apply / Scope Directives ---*/

void instance_t::apply_directive(char* line) {
  char* b = next_element(line);
  string keyword(line);
  // As in account_directive_body(), only a recognized keyword is checked
  // for a missing argument.
  auto argument = [&]() -> char* {
    if (!b)
      throw_(parse_error, _f("Directive 'apply %1%' requires an argument") % keyword);
    return b;
  };

  if (keyword == "account")
    apply_account_directive(argument());
  else if (keyword == "tag")
    apply_tag_directive(argument());
  else if (keyword == "fixed" || keyword == "rate")
    apply_rate_directive(argument());
  else if (keyword == "year")
    apply_year_directive(argument(), true); // "apply year" uses apply_stack
  else
    throw_(parse_error, _f("Unrecognized directive 'apply %1%'") % keyword);
}

void instance_t::apply_account_directive(char* line) {
  if (account_t* acct = top_account()->find_account(line))
    apply_stack.push_front(application_t("account", acct));
  else
    assert("Failed to create account" == nullptr);
}

void instance_t::apply_tag_directive(char* line) {
  string tag(trim_ws(line));
  if (tag.find(':') == string::npos)
    tag = string(":") + tag + ":";

  // Declare the names the tag string applies, read by the same scanner that
  // applies them to each transaction, so that --strict does not warn.
  for (const metadata_t::parsed_tag_t& parsed : metadata_t::scan_metadata_tags(tag.c_str()))
    context.journal->register_metadata(parsed.name, NULL_VALUE, 0);

  apply_stack.push_front(application_t("tag", tag));
}

void instance_t::apply_rate_directive(char* line) {
  if (std::optional<std::pair<commodity_t*, price_point_t>> price_point =
          commodity_pool_t::current_pool->parse_price_directive(trim_ws(line), true, true)) {
    apply_stack.push_front(
        application_t("fixed", fixed_rate_t(price_point->first, price_point->second.price)));
  } else {
    throw_(std::runtime_error, _("Error in fixed directive"));
  }
}

void instance_t::apply_year_directive(char* line, bool use_apply_stack) {
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
  } catch (bad_lexical_cast&) { // NOLINT(bugprone-empty-catch)
    throw_(parse_error, _f("Argument '%1%' not a valid year") % skip_ws(line));
  }
}

void instance_t::pop_application() {
  const application_t& entry = apply_stack.front();
  if (std::holds_alternative<optional<datetime_t>>(entry.value)) {
    epoch = std::get<optional<datetime_t>>(entry.value);
    year_directive_year = entry.saved_year_directive;
  }

  apply_stack.pop_front();
}

void instance_t::end_apply_directive(char* kind) {
  // `kind` is the text after `end`.  Three forms close a block: `end`,
  // `end apply [NAME]` and the older `end KEYWORD [NAME]`, which is
  // checked like `end apply KEYWORD`.  NAME, when given, is the name the
  // block is checked against.
  string name;
  if (kind && *kind) {
    char* rest = next_element(kind);
    string word(kind);
    if (word == "apply") {
      name = rest ? rest : "";
    } else if (word == "account" || word == "tag" || word == "fixed" || word == "rate" ||
               word == "year") {
      name = rest && *rest ? rest : word;
    } else {
      throw_(parse_error, _f("Unrecognized directive 'end %1%'") % word);
    }
  }

  if (apply_stack.size() <= 1) {
    if (name.empty()) {
      throw_(std::runtime_error,
             _("'end' or 'end apply' found, but no enclosing 'apply' directive"));
    } else {
      throw_(std::runtime_error,
             _f("'end apply %1%' found, but no enclosing 'apply' directive") % name);
    }
  }

  // apply_rate_directive records both `apply rate` and `apply fixed` as
  // "fixed", so either spelling closes either block.
  if (!name.empty() && (name == "rate" ? string("fixed") : name) != apply_stack.front().label)
    throw_(std::runtime_error,
           _f("'end apply %1%' directive does not match 'apply %2%' directive") % name %
               apply_stack.front().label);

  pop_application();
}

/*--- Directive Blocks ---*/

namespace {

/// Return true if @p line, an indented line beneath an `account`,
/// `commodity`, `payee` or `tag` directive with its leading whitespace
/// skipped, is a comment rather than a sub-directive.  These are the
/// characters that read_next_directive() treats as starting a comment at
/// the beginning of a top-level line.
bool is_block_comment(const char* line) {
  switch (*line) {
  case ';':
  case '#':
  case '%':
  case '*':
  case '|':
    return true;
  default:
    return false;
  }
}

} // namespace

char* instance_t::subdirective_t::argument() const {
  if (!arg) {
    string name(kind);
    name[0] = static_cast<char>(std::toupper(static_cast<unsigned char>(name[0])));
    throw_(parse_error, _f("%1% directive '%2%' requires an argument") % name % keyword);
  }
  return arg;
}

void instance_t::read_subdirectives(std::size_t outer_indent, const char* kind,
                                    const std::function<bool(const subdirective_t&)>& on_keyword,
                                    const std::function<void(char*)>& on_comment) {
  while (true) {
    // At top level a plain one-character look at the stream decides; the
    // seek-and-restore of peek_line_indent() is needed only to compare the
    // depth of a nested block against its parent's, before the line is read.
    std::size_t indent = 0;
    if (outer_indent == 0) {
      if (!peek_whitespace_line())
        break;
    } else {
      indent = peek_line_indent();
      if (indent <= outer_indent) // 0 when the next line is not indented
        break;
    }

    char* line;
    read_line(line);
    char* q = skip_ws(line);
    if (outer_indent == 0)
      indent = static_cast<std::size_t>(q - line); // same count peek_line_indent() makes
    if (!*q)
      break;

    if (is_block_comment(q)) {
      if (on_comment)
        on_comment(skip_ws(q + 1));
      continue;
    }

    char* arg = next_element(q); // terminates the keyword at q
    subdirective_t sub{kind, q, arg, indent};
    if (!on_keyword(sub))
      throw_(parse_error, _f("Unrecognized %1% directive '%2%'") % kind % sub.keyword);
  }
}

/*--- Account Directives ---*/

void instance_t::account_directive(char* line) {
  account_directive_body(line, top_account(), 0);
}

void instance_t::account_directive_body(char* line, account_t* parent, std::size_t outer_indent) {
  std::istream::pos_type beg_pos = context.line_beg_pos;
  std::size_t beg_linenum = context.linenum;

  char* p = skip_ws(line);
  // A ';' after a hard separator begins a comment, as it does after a
  // transaction's payee, and the account name ends before it (issue
  // #3320).  A ';' after a single space is part of the name, as it is in
  // a posting.  The comment carries metadata for the account, as a comment
  // line beneath the directive does.
  char* comment = split_inline_note(p);
  account_t* account = context.journal->register_account(p, nullptr, parent);
  account->add_flags(ACCOUNT_KNOWN);
  if (comment)
    account->parse_tags(skip_ws(comment), *context.scope, true);
  unique_ptr<auto_xact_t> ae;

  read_subdirectives(
      outer_indent, "account",
      [&](const subdirective_t& sub) -> bool {
        if (sub.keyword == "alias") {
          account_alias_directive(account, sub.argument());
        } else if (sub.keyword == "payee") {
          account_payee_directive(account, sub.argument());
        } else if (sub.keyword == "value") {
          account_value_directive(account, sub.argument());
        } else if (sub.keyword == "default") {
          account_default_directive(account);
        } else if (sub.keyword == "account") {
          // Nested account declaration (issue #877): register a child account
          // using the enclosing account as its parent, avoiding the need to
          // repeat the full colon-separated path.
          account_directive_body(sub.argument(), account, sub.indent);
        } else if (sub.keyword == "assert" || sub.keyword == "check") {
          char* expr_str = sub.argument();

          if (!ae.get()) {
            // No predicate: apply_checks_to_post never evaluates one.
            ae.reset(new auto_xact_t());

            ae->pos = position_t();
            ae->pos->pathname = context.pathname_ref;
            ae->pos->beg_pos = beg_pos;
            ae->pos->beg_line = beg_linenum;
            ae->pos->sequence = context.sequence++;
            ae->check_exprs = expr_t::check_expr_list();
          }

          ae->check_exprs->push_back(expr_t::check_expr_pair(
              expr_t(expr_str),
              sub.keyword == "assert" ? expr_t::EXPR_ASSERTION : expr_t::EXPR_CHECK));
        } else if (sub.keyword == "eval" || sub.keyword == "expr") {
          // jww (2012-02-27): Make account into symbol scopes so that this
          // can be used to override definitions within the account.
          bind_scope_t bound_scope(*context.scope, *account);
          expr_t(sub.argument()).calc(bound_scope);
        } else if (sub.keyword == "note") {
          account->note = sub.argument();
        } else {
          return false;
        }
        return true;
      },
      // A comment line under an account directive (e.g.
      // `    ; Contribution: 12.97 EUR`) carries metadata tags for the
      // account rather than a sub-directive.  Parsing it through
      // account_t::parse_tags makes the `:tag:` and `Key: value` forms
      // available to balance-format expressions such as
      // `%(tag("Contribution"))` (issue #1681).
      [&](char* comment) { account->parse_tags(comment, *context.scope, true); });

  if (ae.get()) {
    ae->journal = context.journal;
    ae->pos->end_pos = context.curr_pos;
    ae->pos->end_line = context.linenum;
    // Indexed by account for journal_t::extend_xact() (#562).
    context.journal->account_check_xacts[account].push_back(std::move(ae));
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
  context.journal->account_aliases.insert_or_assign(alias, account);
}

void instance_t::alias_directive(char* line) {
  if (char* e = std::strchr(line, '=')) {
    char* z = e - 1;
    while (std::isspace(static_cast<unsigned char>(*z)))
      *z-- = '\0';
    *e++ = '\0';
    e = skip_ws(e);

    account_t* account = top_account()->find_account(e);
    account->add_flags(ACCOUNT_KNOWN);
    account_alias_directive(account, line);
  }
}

void instance_t::account_payee_directive(account_t* account, string payee) {
  trim(payee);
  context.journal->payees_for_unknown_accounts.push_back(account_mapping_t(mask_t(payee), account));
}

void instance_t::account_default_directive(account_t* account) {
  context.journal->bucket = account;
}

void instance_t::account_value_directive(account_t* account, const string& expr_str) {
  account->value_expr = expr_t(expr_str);
}

/*--- Payee Directives ---*/

void instance_t::payee_directive(char* line) {
  string payee = context.journal->register_payee(line);

  read_subdirectives(0, "payee", [&](const subdirective_t& sub) -> bool {
    if (sub.keyword == "alias")
      payee_alias_directive(payee, sub.argument());
    else if (sub.keyword == "uuid")
      payee_uuid_directive(payee, sub.argument());
    else
      return false;
    return true;
  });
}

void instance_t::payee_alias_directive(const string& payee, string alias) {
  trim(alias);
  context.journal->payee_alias_mappings.push_back(payee_alias_mapping_t(mask_t(alias), payee));
}

void instance_t::payee_uuid_directive(const string& payee, string uuid) {
  trim(uuid);
  context.journal->payee_uuid_mappings[uuid] = payee;
}

namespace {

/// Read the `PATTERN  REPLACEMENT` argument of a payee-rewrite or
/// account-rewrite directive into the pattern mask and the trimmed
/// replacement, throwing parse_error with @p missing_replacement_message
/// when the replacement is missing.
std::pair<mask_t, string> parse_rewrite_args(char* line, const char* missing_replacement_message) {
  // Format: payee-rewrite|account-rewrite PATTERN   REPLACEMENT
  // The pattern and replacement are separated by two spaces or a tab.
  char* p = skip_ws(line);
  char* repl = next_element(p, true);
  if (!repl)
    throw_(parse_error, missing_replacement_message);
  string pattern(p);
  trim(pattern);
  string replacement(repl);
  trim(replacement);
  return {mask_t(pattern), replacement};
}

} // namespace

void instance_t::payee_rewrite_directive(char* line) {
  context.journal->payee_rewrite_mappings.push_back(
      parse_rewrite_args(line, _("payee-rewrite directive requires a pattern and a replacement")));
}

void instance_t::account_rewrite_directive(char* line) {
  context.journal->account_rewrite_mappings.push_back(parse_rewrite_args(
      line, _("account-rewrite directive requires a pattern and a replacement")));
}

/*--- Commodity Directives ---*/

namespace {

/// Erase a trailing comment from the argument of a sub-directive that
/// names a commodity (`alias`, `format`).  The comment begins at the
/// first ';' that is neither inside a quoted symbol nor escaped with a
/// backslash, the only places a commodity symbol can hold one (issue
/// #3313).
void strip_commodity_comment(string& arg) {
  bool quoted = false;
  for (std::size_t i = 0; i < arg.length(); ++i) {
    if (arg[i] == '"') {
      quoted = !quoted;
    } else if (quoted) {
      continue;
    } else if (arg[i] == '\\') {
      ++i;
    } else if (arg[i] == ';') {
      arg.erase(i);
      break;
    }
  }
}

} // namespace

void instance_t::commodity_directive(char* line) {
  char* p = skip_ws(line);
  string symbol; // NOLINT(bugprone-unused-local-non-trivial-variable)
  commodity_t::parse_symbol(p, symbol);

  commodity_t* commodity = commodity_pool_t::current_pool->find_or_create(symbol);
  context.journal->register_commodity(*commodity, 0);

  read_subdirectives(0, "commodity", [&](const subdirective_t& sub) -> bool {
    if (sub.keyword == "alias")
      commodity_alias_directive(*commodity, sub.argument());
    else if (sub.keyword == "value")
      commodity_value_directive(*commodity, sub.argument());
    else if (sub.keyword == "format")
      commodity_format_directive(*commodity, sub.argument());
    else if (sub.keyword == "nomarket")
      commodity_nomarket_directive(*commodity);
    else if (sub.keyword == "default")
      commodity_default_directive(*commodity);
    else if (sub.keyword == "note")
      commodity->set_note(string(sub.argument()));
    else
      return false;
    return true;
  });
}

void instance_t::commodity_alias_directive(commodity_t& comm, string alias) {
  strip_commodity_comment(alias);
  trim(alias);
  // A line holding only a comment names no alias; an empty name would
  // remap the null commodity, whose symbol is empty.
  if (alias.empty())
    throw_(parse_error, _f("Commodity directive '%1%' requires an argument") % "alias");
  // Strip surrounding quotes if present, matching how commodity names are parsed
  if (alias.length() > 2 && alias[0] == '"' && alias[alias.length() - 1] == '"')
    alias = alias.substr(1, alias.length() - 2);
  commodity_pool_t::current_pool->alias(alias, comm);
}

void instance_t::commodity_value_directive(commodity_t& comm, const string& expr_str) {
  comm.set_value_expr(expr_t(expr_str));
}

namespace {

/// True if a bare symbol and a number that meet at characters @p a and @p b
/// would run together: both are characters parse_quantity() reads as part of
/// a number.
bool runs_together(char a, char b) {
  auto in_number = [](char c) {
    return std::isdigit(static_cast<unsigned char>(c)) || c == '.' || c == ',' || c == '\'';
  };
  return in_number(a) && in_number(b);
}

/// Quote a bare @p symbol found at either end of the format sample @p format.
///
/// A commodity directive reads its symbol as a whole word, so `commodity N1`
/// declares the commodity that an amount writes as "N1": unquoted, the digit
/// would end the symbol.  Quoting the bare spelling lets the format
/// sub-directive name the commodity as the directive did (issue #3312).
///
/// A sample that quotes its own symbol is left as written.  So is one in
/// which the bare symbol meets the number between two number characters, as
/// in `N11,000.00`, since nothing there marks where the symbol ends.
string quote_bare_symbol(const string& format, const string& symbol) {
  if (!commodity_t::symbol_needs_quotes(symbol) || format.find('"') != string::npos)
    return format;

  const string quoted = '"' + symbol + '"';
  const std::size_t len = symbol.length();

  if (format.compare(0, len, symbol) == 0 &&
      (format.length() == len || !runs_together(symbol.back(), format[len])))
    return quoted + format.substr(len);

  if (format.length() > len) {
    const std::size_t start = format.length() - len;
    if (format.compare(start, len, symbol) == 0 &&
        !runs_together(format[start - 1], symbol.front()))
      return format.substr(0, start) + quoted;
  }

  return format;
}

} // namespace

void instance_t::commodity_format_directive(commodity_t& comm, string format) {
  // jww (2012-02-27): A format specified this way should turn off
  // observational formatting.  The format declares the style
  // (PARSE_DECLARES_STYLE), so a decimal comma that only its digits
  // suggest is no conflict with an earlier decimal period.
  strip_commodity_comment(format);
  trim(format);
  amount_t amt;
  std::istringstream in(quote_bare_symbol(format, comm.base_symbol()));
  (void)amt.parse(in, PARSE_NO_REDUCE | PARSE_DECLARES_STYLE);
  string trailing;
  if (in >> trailing)
    throw_(parse_error,
           _f("unexpected characters '%1%' after commodity symbol in format directive") % trailing);
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

/*--- Tag Directive ---*/

void instance_t::tag_directive(char* line) {
  char* p = skip_ws(line);
  context.journal->register_metadata(p, NULL_VALUE, 0);

  read_subdirectives(0, "tag", [&](const subdirective_t& sub) -> bool {
    if (sub.keyword != "assert" && sub.keyword != "check")
      return false;
    context.journal->tag_check_exprs.insert(tag_check_exprs_map::value_type(
        string(p), expr_t::check_expr_pair(expr_t(sub.argument()), sub.keyword == "assert"
                                                                       ? expr_t::EXPR_ASSERTION
                                                                       : expr_t::EXPR_CHECK)));
    return true;
  });
}

/*--- Expression and Assertion Directives ---*/

void instance_t::eval_directive(char* line) {
  expr_t expr(line);
  expr.calc(*context.scope);
}

void instance_t::assert_directive(char* line) {
  // Invalidate cached family totals so the assertion evaluates against
  // the current journal state.  Without this, a repeated expression
  // such as account("X").total returns a stale cached value (#1679).
  context.journal->master->clear_display_state();

  expr_t expr(line);
  if (!expr.calc(*context.scope).to_boolean())
    throw_(parse_error, _f("Assertion failed: %1%") % line);
}

void instance_t::check_directive(char* line) {
  context.journal->master->clear_display_state();

  expr_t expr(line);
  if (!expr.calc(*context.scope).to_boolean())
    context.warning(_f("Check failed: %1%") % line);
}

void instance_t::value_directive(char* line) {
  context.journal->value_expr = expr_t(line);
}

/*--- Comment / Test Block Directive ---*/

void instance_t::comment_directive(char* line) {
  while (in.good() && !in.eof()) {
    if (read_line(line) > 0) {
      std::string buf(line); // NOLINT(bugprone-unused-local-non-trivial-variable)
      if (starts_with(buf, "end comment") || starts_with(buf, "end test"))
        break;
    }
  }
}

/*--- Python Directives ---*/

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
  python_session->eval(script.str());
}

#else

void instance_t::import_directive(char*) {
  throw_(parse_error, _("'import' directive seen, but Python support is missing"));
}

void instance_t::python_directive(char*) {
  throw_(parse_error, _("'python' directive seen, but Python support is missing"));
}

#endif // HAVE_BOOST_PYTHON

/*--- General Directive Dispatch ---*/

namespace {

/// A word directive: the keyword that begins its line, the handler that
/// reads the rest of the line, and whether the keyword may stand alone.
struct word_directive_t {
  const char* keyword;
  void (*handler)(instance_t& instance, char* arg);
  bool bare_ok;
};

const word_directive_t word_directives[] = {
    {"account", [](instance_t& in, char* arg) { in.account_directive(arg); }, false},
    {"account-rewrite", [](instance_t& in, char* arg) { in.account_rewrite_directive(arg); },
     false},
    {"alias", [](instance_t& in, char* arg) { in.alias_directive(arg); }, false},
    {"apply", [](instance_t& in, char* arg) { in.apply_directive(arg); }, false},
    {"assert", [](instance_t& in, char* arg) { in.assert_directive(arg); }, false},
    {"bucket", [](instance_t& in, char* arg) { in.default_account_directive(arg); }, false},
    {"check", [](instance_t& in, char* arg) { in.check_directive(arg); }, false},
    {"comment", [](instance_t& in, char* arg) { in.comment_directive(arg); }, true},
    {"commodity", [](instance_t& in, char* arg) { in.commodity_directive(arg); }, false},
    {"def", [](instance_t& in, char* arg) { in.eval_directive(arg); }, false},
    {"define", [](instance_t& in, char* arg) { in.eval_directive(arg); }, false},
    {"end", [](instance_t& in, char* arg) { in.end_apply_directive(arg); }, true},
    {"eval", [](instance_t& in, char* arg) { in.eval_directive(arg); }, false},
    {"expr", [](instance_t& in, char* arg) { in.eval_directive(arg); }, false},
    {"import", [](instance_t& in, char* arg) { in.import_directive(arg); }, false},
    {"include", [](instance_t& in, char* arg) { in.include_directive(arg); }, false},
    {"payee", [](instance_t& in, char* arg) { in.payee_directive(arg); }, false},
    {"payee-rewrite", [](instance_t& in, char* arg) { in.payee_rewrite_directive(arg); }, false},
    {"python", [](instance_t& in, char* arg) { in.python_directive(arg); }, true},
    {"tag", [](instance_t& in, char* arg) { in.tag_directive(arg); }, false},
    {"test", [](instance_t& in, char* arg) { in.comment_directive(arg); }, true},
    {"value", [](instance_t& in, char* arg) { in.value_directive(arg); }, false},
    {"year", [](instance_t& in, char* arg) { in.apply_year_directive(arg); }, false},
};

} // namespace

/**
 * @brief Dispatch a word directive (account, commodity, include, ...)
 *        through the word_directives table.
 *
 * @return true if the line's first word names a word directive, which has
 *         then been handled; false otherwise, and the caller then tries the
 *         legacy single-character directives.
 */
bool instance_t::general_directive(char* line) {
  std::string buf(line); // NOLINT(bugprone-unused-local-non-trivial-variable)

  char* p = &buf[0];
  char* arg = next_element(&buf[0]);

  if (*p == '@' || *p == '!')
    p++;

  // Comparing the first character before calling strcmp() keeps the scan
  // cheap for the legacy single-character directives, such as the `P` of a
  // price history, whose letters begin no keyword.
  const word_directive_t* const end = std::end(word_directives);
  const word_directive_t* directive =
      std::find_if(std::begin(word_directives), end, [p](const word_directive_t& d) {
        return *p == *d.keyword && std::strcmp(p, d.keyword) == 0;
      });

  // Ensure there's an argument for all directives that need one.  This runs
  // before the legacy single-character directives are tried, so a bare word
  // that names no directive is reported here too; `Y2024` carries its year.
  if (!arg && !(directive != end && directive->bare_ok) && *p != 'Y')
    throw_(parse_error, _f("Directive '%1%' requires an argument") % p);

  if (directive == end)
    return false;
  directive->handler(*this, arg);
  return true;
}

} // namespace ledger
