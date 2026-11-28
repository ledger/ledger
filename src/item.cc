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
 * @file   item.cc
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Implementation of item_t -- the base class for all journal items.
 *
 * The metadata storage, lookups, and `:tag:` / `Key:` parser live in
 * metadata.cc.  This file implements the date-aware parse_tags
 * extension, the expression-engine bindings (lookup), and utility
 * functions for printing and serializing journal items.
 */

#include <system.hh>

#include "item.h"
#include "context.h"

namespace ledger {

bool item_t::use_aux_date = false;

/*----------------------------------------------------------------------*/
/*  Tag Parsing (date extension)                                        */
/*                                                                      */
/*  Items extend the base parser with a bracketed-date form `[date]`    */
/*  or `[date=auxdate]`, recognized by note_date_span (also used by     */
/*  print) while no colon precedes it.                                  */
/*  Everything else is delegated to metadata_t::parse_metadata_tags.    */
/*----------------------------------------------------------------------*/

std::optional<std::pair<std::size_t, std::size_t>> item_t::note_date_span(const char* line) {
  // Skip any additional leading `;` characters (with interleaved whitespace)
  // before scanning for `[date]`.  `parse_metadata_tags` repeats this skip
  // so the colon parser sees the same starting position.
  const char* d = line;
  while (*d == ' ' || *d == '\t' || *d == ';')
    ++d;

  // `[date]` / `[=auxdate]` is a date directive only while no colon precedes
  // it in the comment.  A colon means the metadata parser may own the text
  // the bracket sits in: a leading `Key:` / `Key::` setting claims the rest
  // of the line as that tag's value, so `; Due:: [2024/01/01]` records a
  // value and leaves the dates alone (#3192), and a `:tag:` series claims
  // its own token.  Everything ahead of the first colon is plain note text,
  // where a trailing bracketed date has always moved the item -- as in
  // `; Returned clothes [2026/01/01]` (#3252).
  //
  // Only the text preceding the bracket is consulted, so the documented
  // `; [=date] :tag:` forms keep their dates (#1644, #1968).
  const char* b = std::strchr(d, '[');
  if (!b || !(std::isdigit(static_cast<unsigned char>(*(b + 1))) || *(b + 1) == '=') ||
      std::memchr(d, ':', static_cast<std::size_t>(b - d)))
    return std::nullopt;

  // A span of 256 characters or more is far too long to be a date.
  const char* e = std::strchr(b, ']');
  if (!e || static_cast<std::size_t>(e - b - 1) >= 256)
    return std::nullopt;

  return std::make_pair(static_cast<std::size_t>(b + 1 - line), static_cast<std::size_t>(e - line));
}

void item_t::parse_bracket_dates(const char* p) {
  if (auto span = note_date_span(p)) {
    string content(p + span->first, span->second - span->first);
    if (auto eq = content.find('='); eq != string::npos) {
      _date_aux = parse_date(content.substr(eq + 1));
      content.resize(eq);
    }
    if (!content.empty())
      _date = parse_date(content);
  }
}

void item_t::parse_tags(const char* p, scope_t& scope, bool overwrite_existing) {
  parse_bracket_dates(p);
  parse_metadata_tags(p, scope, *this, overwrite_existing);
}

void item_t::append_note(const char* p, scope_t& scope, bool overwrite_existing) {
  if (note) { // NOLINT(bugprone-branch-clone)
    *note += '\n';
    *note += p;
  } else {
    note = p;
  }

  parse_tags(p, scope, overwrite_existing);
}

/*----------------------------------------------------------------------*/
/*  Expression Bindings                                                 */
/*                                                                      */
/*  These getter functions are referenced by item_t::lookup() to map    */
/*  expression function names (e.g., "date", "status", "note") to      */
/*  callable wrappers.  The get_wrapper<> template adapts a simple      */
/*  item_t getter into a call_scope_t functor that the expression       */
/*  engine can invoke.                                                  */
/*----------------------------------------------------------------------*/

namespace {
value_t get_status(item_t& item) {
  return long(item.state());
}
value_t get_uncleared(item_t& item) {
  return item.state() == item_t::UNCLEARED;
}
value_t get_cleared(item_t& item) {
  return item.state() == item_t::CLEARED;
}
value_t get_pending(item_t& item) {
  return item.state() == item_t::PENDING;
}

value_t get_actual(item_t& item) {
  return !item.has_flags(ITEM_GENERATED);
}

value_t get_date(item_t& item) {
  return item.date();
}
value_t get_primary_date(item_t& item) {
  return item.primary_date();
}
value_t get_aux_date(item_t& item) {
  if (optional<date_t> aux_date = item.aux_date())
    return *aux_date;
  return NULL_VALUE;
}
value_t get_note(item_t& item) {
  return item.note ? string_value(*item.note) : NULL_VALUE;
}

value_t item_has_tag(call_scope_t& args) {
  return metadata_has_tag(find_scope<item_t>(args), args);
}

value_t item_get_tag(call_scope_t& args) {
  return metadata_get_tag(find_scope<item_t>(args), args);
}

value_t get_pathname(item_t& item) {
  if (item.pos)
    return string_value(item.pos->pathname->string());
  else
    return NULL_VALUE;
}

value_t get_filebase(item_t& item) {
  if (item.pos)
    return string_value(item.pos->pathname->filename().string());
  else
    return NULL_VALUE;
}

value_t get_filepath(item_t& item) {
  if (item.pos)
    return string_value(item.pos->pathname->parent_path().string());
  else
    return NULL_VALUE;
}

value_t get_beg_pos(item_t& item) {
  return item.pos ? long(item.pos->beg_pos) : 0L;
}

value_t get_beg_line(item_t& item) {
  return item.pos ? long(item.pos->beg_line) : 0L;
}

value_t get_end_pos(item_t& item) {
  return item.pos ? long(item.pos->end_pos) : 0L;
}

value_t get_end_line(item_t& item) {
  return item.pos ? long(item.pos->end_line) : 0L;
}

value_t get_seq(item_t& item) {
  return long(item.seq());
}
value_t get_id(item_t& item) {
  return string_value(item.id());
}

value_t get_addr(item_t& item) {
  return boost::numeric_cast<long>(reinterpret_cast<uintptr_t>(&item));
}

value_t get_depth(item_t&) {
  return 0L;
}

value_t ignore(item_t&) {
  return false;
}

template <value_t (*Func)(item_t&)>
value_t get_wrapper(call_scope_t& scope) {
  return (*Func)(find_scope<item_t>(scope));
}

/// The functions item_t binds by name.
constexpr symbol_entry_t<value_t (*)(call_scope_t&)> item_functions[] = {
    {"L", get_wrapper<&get_actual>},
    {"X", get_wrapper<&get_cleared>},
    {"Y", get_wrapper<&get_pending>},
    {"actual", get_wrapper<&get_actual>},
    {"actual_date", get_wrapper<&get_primary_date>},
    {"addr", get_wrapper<&get_addr>},
    {"aux_date", get_wrapper<&get_aux_date>},
    {"beg_line", get_wrapper<&get_beg_line>},
    {"beg_pos", get_wrapper<&get_beg_pos>},
    {"cleared", get_wrapper<&get_cleared>},
    {"comment", get_wrapper<&get_comment>},
    {"d", get_wrapper<&get_date>},
    {"date", get_wrapper<&get_date>},
    {"depth", get_wrapper<&get_depth>},
    {"effective_date", get_wrapper<&get_aux_date>},
    {"end_line", get_wrapper<&get_end_line>},
    {"end_pos", get_wrapper<&get_end_pos>},
    {"filebase", get_wrapper<&get_filebase>},
    {"filename", get_wrapper<&get_pathname>},
    {"filepath", get_wrapper<&get_filepath>},
    {"has_meta", item_has_tag},
    {"has_tag", item_has_tag},
    {"id", get_wrapper<&get_id>},
    {"is_account", get_wrapper<&ignore>},
    {"meta", item_get_tag},
    {"note", get_wrapper<&get_note>},
    {"parent", get_wrapper<&ignore>},
    {"pending", get_wrapper<&get_pending>},
    {"primary_date", get_wrapper<&get_primary_date>},
    {"seq", get_wrapper<&get_seq>},
    {"state", get_wrapper<&get_status>},
    {"status", get_wrapper<&get_status>},
    {"tag", item_get_tag},
    {"uncleared", get_wrapper<&get_uncleared>},
    {"uuid", get_wrapper<&get_id>},
    {"value_date", get_wrapper<&get_date>},
};
static_assert(symbol_table_sorted(item_functions), "item_functions must stay sorted");
} // namespace

/*----------------------------------------------------------------------*/
/*  Utility Functions                                                   */
/*----------------------------------------------------------------------*/

/**
 * @brief Format an item's note as a semicolon-prefixed comment string.
 *
 * Short notes (15 characters or fewer) are formatted inline as
 * `  ;note`.  Longer notes start on a new line as `\n    ;note`.
 * Embedded newlines in the note text produce additional `\n    ;`
 * continuation lines.
 */
value_t get_comment(item_t& item) {
  if (!item.note) {
    return string_value("");
  } else {
    std::ostringstream buf;
    if (item.note->length() > 15)
      buf << "\n    ;";
    else
      buf << "  ;";

    bool need_separator = false;
    for (const char* p = item.note->c_str(); *p; p++) {
      if (*p == '\n') {
        need_separator = true;
      } else {
        if (need_separator) {
          buf << "\n    ;";
          need_separator = false;
        }
        buf << *p;
      }
    }
    return string_value(buf.str());
  }
}

/**
 * @brief Store an expression result as a metadata tag.
 *
 * Evaluates the expression @p def in the default scope bound to this
 * item and stores the result under the given @p name as a metadata
 * tag.  The defining_ flag prevents re-entrant calls (which could
 * occur if the expression itself triggers a define on this item).
 */
void item_t::define(const symbol_t::kind_t, const string& name, const expr_t::ptr_op_t& def) {
  if (defining_)
    return;
  defining_ = true;
  try {
    bind_scope_t bound_scope(*scope_t::default_scope, *this);
    set_tag(name, def->calc(bound_scope));
    defining_ = false;
  } catch (...) {
    defining_ = false;
    throw;
  }
}

/**
 * @brief Dispatch table mapping expression names to item property getters.
 *
 * This is the core of item_t's participation in the expression engine.
 * When an expression like `date > [2024-01-01]` or `has_tag("Payee")`
 * is evaluated, the expression engine calls lookup() to resolve each
 * identifier.  This method maps names to getter functors:
 *
 *   - `date` / `d` -- effective date (respects --aux-date)
 *   - `primary_date` / `actual_date` -- always the entry date
 *   - `aux_date` / `effective_date` -- the auxiliary date
 *   - `status` / `state` -- clearing state as integer (0/1/2)
 *   - `cleared` / `X` -- true if state is CLEARED
 *   - `pending` / `Y` -- true if state is PENDING
 *   - `uncleared` -- true if state is UNCLEARED
 *   - `actual` / `L` -- true if item is not generated or temporary
 *   - `note` -- the raw note text
 *   - `comment` -- formatted comment with `;` prefixes
 *   - `has_tag(name)` / `has_meta(name)` -- test for metadata presence
 *   - `tag(name)` / `meta(name)` -- retrieve metadata value
 *   - `filename` / `filebase` / `filepath` -- source file information
 *   - `beg_line` / `end_line` / `beg_pos` / `end_pos` -- position info
 *   - `id` / `uuid` -- item identifier
 *   - `seq` -- parse-order sequence number
 *
 * Subclasses (xact_t, post_t) override this to add their own bindings
 * and fall through to item_t::lookup() for base properties.
 */
expr_t::ptr_op_t item_t::lookup(const symbol_t::kind_t kind, const string& name) {
  if (kind == symbol_t::FUNCTION)
    if (const auto* fn = find_symbol(item_functions, name))
      return WRAP_FUNCTOR(*fn);
  return nullptr;
}

bool item_t::valid() const {
  if (_state != UNCLEARED && _state != CLEARED && _state != PENDING) {
    DEBUG("ledger.validate", "item_t: state is bad");
    return false;
  }

  return true;
}

/** @brief Re-read and print the original source text of an item. */
void print_item(std::ostream& out, const item_t& item) {
  // Raw printing: stream source directly without any size limit.
  if (!item.pos || item.pos->pathname->empty())
    return;

  const std::streamoff len = item.pos->end_pos - item.pos->beg_pos;
  if (!(len > 0))
    return;

  string text = read_source(*item.pos->pathname, item.pos->beg_pos, len);
  // Drop trailing CR/LF, as source_context() does; print_xacts::flush()
  // appends the newline.
  text.erase(text.find_last_not_of("\r\n") + 1);
  out << text;
}

/**
 * @brief Build a human-readable error context string for an item.
 *
 * Produces output like:
 * @code
 *   transaction from "ledger.dat", lines 5-8:
 *   > 2024/01/01 Payee
 *   >     Expenses:Food  $10.00
 *   >     Assets:Cash
 * @endcode
 */
string item_context(const item_t& item, const string& desc) {
  if (!item.pos)
    return empty_string;

  std::streamoff len = item.pos->end_pos - item.pos->beg_pos;
  if (!(len > 0))
    return empty_string;

  std::ostringstream out;

  if (item.pos->pathname->empty()) {
    out << desc << _(" from streamed input:");
    return out.str();
  }

  out << desc << _(" from \"") << item.pos->pathname->string() << "\"";

  if (item.pos->beg_line != item.pos->end_line)
    out << _(", lines ") << item.pos->beg_line << "-" << item.pos->end_line << ":\n";
  else
    out << _(", line ") << item.pos->beg_line << ":\n";

  out << source_context(*item.pos->pathname, item.pos->beg_pos, item.pos->end_pos, "> ");

  return out.str();
}

string check_location(const item_t* item, const parse_context_t* context) {
  if (!context)
    return string();
  if (item && item->pos)
    return file_context(*item->pos->pathname, item->pos->beg_line) + " ";
  return context->location() + " ";
}

} // namespace ledger
