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

#if HAVE_GPGME
#include "gpgme.h"
#endif

namespace ledger {

bool item_t::use_aux_date = false;

/*----------------------------------------------------------------------*/
/*  Tag Parsing (date extension)                                        */
/*                                                                      */
/*  Items extend the base parser with a leading bracketed-date form     */
/*  `[date]` or `[date=auxdate]`.  Everything else is delegated to      */
/*  metadata_t::parse_metadata_tags.                                    */
/*----------------------------------------------------------------------*/

void item_t::parse_tags(const char* p, scope_t& scope, bool overwrite_existing) {
  // Skip any additional leading `;` characters (with interleaved whitespace)
  // before scanning for `[date]`.  `parse_metadata_tags` repeats this skip
  // so the colon parser sees the same starting position.
  const char* d = p;
  while (*d == ' ' || *d == '\t' || *d == ';')
    ++d;

  // Only treat `[date]` / `[=auxdate]` as a date directive when it is the
  // first non-whitespace, non-`;` token of the comment.  A bracketed date
  // appearing later in the line (e.g. as a value within typed metadata
  // such as `; Due:: [2024/01/01]`) is a value, not a directive (#3192).
  if (*d == '[' && (std::isdigit(static_cast<unsigned char>(*(d + 1))) || *(d + 1) == '=')) {
    if (const char* e = std::strchr(d, ']')) {
      char buf[256];
      std::strncpy(buf, d + 1, static_cast<std::size_t>(e - d - 1));
      buf[e - d - 1] = '\0';

      if (char* pp = std::strchr(buf, '=')) {
        *pp++ = '\0';
        _date_aux = parse_date(pp);
      }
      if (buf[0])
        _date = parse_date(buf);
    }
  }

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
    return string_value(item.pos->pathname.string());
  else
    return NULL_VALUE;
}

value_t get_filebase(item_t& item) {
  if (item.pos)
    return string_value(item.pos->pathname.filename().string());
  else
    return NULL_VALUE;
}

value_t get_filepath(item_t& item) {
  if (item.pos)
    return string_value(item.pos->pathname.parent_path().string());
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
  if (kind != symbol_t::FUNCTION)
    return nullptr;

  // NOLINTBEGIN(bugprone-branch-clone)
  switch (name[0]) {
  case 'a':
    if (name == "actual")
      return WRAP_FUNCTOR(get_wrapper<&get_actual>);
    else if (name == "actual_date")
      return WRAP_FUNCTOR(get_wrapper<&get_primary_date>);
    else if (name == "addr")
      return WRAP_FUNCTOR(get_wrapper<&get_addr>);
    else if (name == "aux_date")
      return WRAP_FUNCTOR(get_wrapper<&get_aux_date>);
    break;

  case 'b':
    if (name == "beg_line")
      return WRAP_FUNCTOR(get_wrapper<&get_beg_line>);
    else if (name == "beg_pos")
      return WRAP_FUNCTOR(get_wrapper<&get_beg_pos>);
    break;

  case 'c':
    if (name == "cleared")
      return WRAP_FUNCTOR(get_wrapper<&get_cleared>);
    else if (name == "comment")
      return WRAP_FUNCTOR(get_wrapper<&get_comment>);
    break;

  case 'd':
    if (name[1] == '\0' || name == "date")
      return WRAP_FUNCTOR(get_wrapper<&get_date>);
    else if (name == "depth")
      return WRAP_FUNCTOR(get_wrapper<&get_depth>);
    break;

  case 'e':
    if (name == "end_line")
      return WRAP_FUNCTOR(get_wrapper<&get_end_line>);
    else if (name == "end_pos")
      return WRAP_FUNCTOR(get_wrapper<&get_end_pos>);
    else if (name == "effective_date")
      return WRAP_FUNCTOR(get_wrapper<&get_aux_date>);
    break;

  case 'f':
    if (name == "filename")
      return WRAP_FUNCTOR(get_wrapper<&get_pathname>);
    else if (name == "filebase")
      return WRAP_FUNCTOR(get_wrapper<&get_filebase>);
    else if (name == "filepath")
      return WRAP_FUNCTOR(get_wrapper<&get_filepath>);
    break;

  case 'h':
    if (name == "has_tag" || name == "has_meta")
      return WRAP_FUNCTOR(item_has_tag);
    break;

  case 'i':
    if (name == "is_account")
      return WRAP_FUNCTOR(get_wrapper<&ignore>);
    else if (name == "id")
      return WRAP_FUNCTOR(get_wrapper<&get_id>);
    break;

  case 'm':
    if (name == "meta")
      return WRAP_FUNCTOR(item_get_tag);
    break;

  case 'n':
    if (name == "note")
      return WRAP_FUNCTOR(get_wrapper<&get_note>);
    break;

  case 'p':
    if (name == "pending")
      return WRAP_FUNCTOR(get_wrapper<&get_pending>);
    else if (name == "parent")
      return WRAP_FUNCTOR(get_wrapper<&ignore>);
    else if (name == "primary_date")
      return WRAP_FUNCTOR(get_wrapper<&get_primary_date>);
    break;

  case 's':
    if (name == "status" || name == "state")
      return WRAP_FUNCTOR(get_wrapper<&get_status>);
    else if (name == "seq")
      return WRAP_FUNCTOR(get_wrapper<&get_seq>);
    break;

  case 't':
    if (name == "tag")
      return WRAP_FUNCTOR(item_get_tag);
    break;

  case 'u':
    if (name == "uncleared")
      return WRAP_FUNCTOR(get_wrapper<&get_uncleared>);
    else if (name == "uuid")
      return WRAP_FUNCTOR(get_wrapper<&get_id>);
    break;

  case 'v':
    if (name == "value_date")
      return WRAP_FUNCTOR(get_wrapper<&get_date>);
    break;

  case 'L':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_actual>);
    break;

  case 'X':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_cleared>);
    break;

  case 'Y':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_pending>);
    break;
  }
  // NOLINTEND(bugprone-branch-clone)

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
void print_item(std::ostream& out, const item_t& item, const string& prefix) {
  if (!prefix.empty()) {
    out << source_context(item.pos->pathname, item.pos->beg_pos, item.pos->end_pos, prefix);
    return;
  }

  // Raw printing: stream source directly without any size limit.
  if (!item.pos || item.pos->pathname.empty())
    return;

  const std::streamoff len = item.pos->end_pos - item.pos->beg_pos;
  if (!(len > 0))
    return;

  std::unique_ptr<std::istream> in(
#if HAVE_GPGME
      decrypted_stream_t::open_stream(item.pos->pathname)
#else
      new ifstream(item.pos->pathname, std::ios::binary)
#endif
  );

  // Determine the effective end position by scanning backwards to strip
  // trailing CR/LF, matching source_context()'s line-splitting behaviour which
  // never emits trailing newlines.  (print_xacts::flush() appends one.)
  std::streamoff effective_off = static_cast<std::streamoff>(item.pos->end_pos);
  const std::streamoff beg_off = static_cast<std::streamoff>(item.pos->beg_pos);
  {
    char c;
    while (effective_off > beg_off) {
      in->seekg(effective_off - 1, std::ios::beg);
      if (!in->read(&c, 1))
        break;
      if (c != '\n' && c != '\r')
        break;
      --effective_off;
    }
  }

  const std::streamoff effective_len = effective_off - beg_off;
  if (effective_len <= 0)
    return;

  in->seekg(beg_off, std::ios::beg);

  const std::size_t CHUNK_SIZE = 8192;
  scoped_array<char> buf(new char[CHUNK_SIZE]);
  std::streamoff remaining = effective_len;

  while (remaining > 0) {
    const std::streamsize to_read = static_cast<std::streamsize>(
        remaining < static_cast<std::streamoff>(CHUNK_SIZE) ? static_cast<std::size_t>(remaining)
                                                            : CHUNK_SIZE);
    in->read(buf.get(), to_read);
    const std::streamsize got = in->gcount();
    if (got <= 0)
      break;
    out.write(buf.get(), got);
    remaining -= got;
  }
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

  if (item.pos->pathname.empty()) {
    out << desc << _(" from streamed input:");
    return out.str();
  }

  out << desc << _(" from \"") << item.pos->pathname.string() << "\"";

  if (item.pos->beg_line != item.pos->end_line)
    out << _(", lines ") << item.pos->beg_line << "-" << item.pos->end_line << ":\n";
  else
    out << _(", line ") << item.pos->beg_line << ":\n";

  print_item(out, item, "> ");

  return out.str();
}

} // namespace ledger
