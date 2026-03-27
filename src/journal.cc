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
 * @file   journal.cc
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Implementation of the central journal container
 *
 * This file implements journal lifecycle management (constructor,
 * destructor, initialize), account registration with alias expansion,
 * payee/commodity/metadata validation, transaction management
 * (add_xact with UUID deduplication), journal I/O, and xdata cleanup.
 */
#include <system.hh>

#include <unordered_set>

#include "journal.h"
#include "context.h"
#include "amount.h"
#include "commodity.h"
#include "pool.h"
#include "xact.h"
#include "post.h"
#include "account.h"

namespace ledger {

/*--- Journal Lifecycle ---*/

journal_t::journal_t() {
  initialize();
  TRACE_CTOR(journal_t, "");
}

#if 0
journal_t::journal_t(const path& pathname)
{
  initialize();
  read(pathname);
  TRACE_CTOR(journal_t, "path");
}

journal_t::journal_t(const string& str)
{
  initialize();
  read(str);
  TRACE_CTOR(journal_t, "string");
}
#endif

/**
 * @brief Destroy the journal, freeing all owned transactions and accounts.
 *
 * Cleanup order matters: regular and period xacts are deleted first,
 * then auto_xacts are cleared (their destructors call remove_post on
 * accounts), and finally the master account tree is deleted.
 */
journal_t::~journal_t() {
  TRACE_DTOR(journal_t);

  // Don't bother unhooking each xact's posts from the accounts they refer to,
  // because all accounts are about to be deleted.
  for (xact_t* xact : xacts)
    checked_delete(xact);

  for (period_xact_t* xact : period_xacts)
    checked_delete(xact);

  // Clear auto_xacts before deleting master to avoid use-after-free:
  // auto_xact_t destructors call remove_post on accounts owned by master.
  auto_xacts.clear();

  checked_delete(master);
}

void journal_t::initialize() {
  master = new account_t;
  bucket = nullptr;
  current_context = nullptr;
  was_loaded = false;
  check_payees = false;
  day_break = false;
  time_round = 0;
  checking_style = CHECK_NORMAL;
  recursive_aliases = false;
  no_aliases = false;

  // Pre-register built-in metadata tags so --strict/--pedantic don't
  // warn about them.  These are tags that ledger uses internally.
  known_tags.insert(_("Payee"));
  known_tags.insert("payee");
  known_tags.insert(_("UUID"));
  known_tags.insert(_("Value"));
  known_tags.insert("Hash");
}

/*--- Account Registration ---*/

void journal_t::add_account(account_t* acct) {
  master->add_account(acct);
}

bool journal_t::remove_account(account_t* acct) {
  return master->remove_account(acct);
}

account_t* journal_t::find_account(string_view name, bool auto_create) {
  return master->find_account(name, auto_create);
}

account_t* journal_t::find_account_re(const string& regexp) {
  return master->find_account_re(regexp);
}

std::vector<account_t*> journal_t::find_accounts_re(const string& regexp) {
  return master->find_accounts_re(regexp);
}

/**
 * @brief Resolve aliases, create the account, and enforce --strict/--pedantic.
 *
 * This is the main entry point for registering an account name encountered
 * during parsing.  The sequence is:
 *   1. Expand account aliases via expand_aliases().
 *   2. Find or create the account in the tree under @p master_account.
 *   3. If the account is named "Unknown", check payees_for_unknown_accounts
 *      for a payee-based mapping to a real account.
 *   4. When --strict or --pedantic is active, verify the account is known.
 *      Template variables ($var, %(expr)) are silently accepted because they
 *      will be resolved at runtime by automated transactions.
 */
account_t* journal_t::register_account(string_view name, post_t* post, account_t* master_account) {
  // If there are any account aliases, substitute before creating an account
  // object.
  account_t* result = expand_aliases(string(name));

  // Create the account object and associate it with the journal; this
  // is registering the account.
  if (!result)
    result = master_account->find_account(name);

  // If the account name being registered is "Unknown", check whether
  // the payee indicates an account that should be used.
  if (result->name == _("Unknown")) {
    for (account_mapping_t& value : payees_for_unknown_accounts) {
      if (post && post->xact && value.first.match(post->xact->payee)) {
        result = value.second;
        break;
      }
    }
  }

  // Now that we have an account, make certain that the account is
  // "known", if the user has requested validation of that fact.
  if (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR) {
    if (!result->has_flags(ACCOUNT_KNOWN)) {
      if (!post) { // NOLINT(bugprone-branch-clone)
        result->add_flags(ACCOUNT_KNOWN);
      }
      // If the post is generated by an automatic transaction, mark the
      // account as known to avoid pedantic errors
      else if (post->has_flags(ITEM_GENERATED)) {
        result->add_flags(ACCOUNT_KNOWN);
      } else {
        // If the account name contains template variable references ($variable
        // or %(expr)) used in automated transactions, it will be resolved at
        // runtime, so mark it as known without warning (see issue #545).
        const string& fn = result->fullname();
        bool has_template_var = contains(fn, "%(");
        if (!has_template_var) {
          for (size_t i = 0; i + 1 < fn.size(); ++i) {
            if (fn[i] == '$' && (std::isalpha((unsigned char)fn[i + 1]) || fn[i + 1] == '_')) {
              has_template_var = true;
              break;
            }
          }
        }
        if (has_template_var) {
          result->add_flags(ACCOUNT_KNOWN);
        } else if (checking_style == CHECK_WARNING) {
          current_context->warning(_f("Unknown account '%1%'") % result->fullname());
        } else if (checking_style == CHECK_ERROR) {
          throw_(parse_error, _f("Unknown account '%1%'") % result->fullname());
        }
      }
    }
  }
  return result;
}

/**
 * @brief Recursively expand account aliases with cycle detection.
 *
 * Checks the account_aliases map for a full-name match first (e.g.,
 * "Foo:Bar" -> some account), then tries matching each colon-separated
 * component of the name (e.g., "Foo", "Bar", "Baz" in "Foo:Bar:Baz").
 * The first matching component is expanded.  When recursive_aliases is
 * true, the loop repeats until no alias matches.  An already_seen set
 * tracks which names have been expanded to detect and prevent infinite
 * alias loops.
 *
 * @return The resolved account, or nullptr if no alias matched.
 */
account_t* journal_t::expand_aliases(string name) {
  // Aliases are expanded recursively, so if both alias Foo=Bar:Foo and
  // alias Bar=Baaz:Bar are in effect, first Foo will be expanded to Bar:Foo,
  // then Bar:Foo will be expanded to Baaz:Bar:Foo.
  // The expansion loop keeps a list of already expanded names in order to
  // prevent infinite excursion. Each alias may only be expanded at most once.
  account_t* result = nullptr;

  if (no_aliases)
    return result;

  const string original_name = name;
  bool keep_expanding = true;
  std::unordered_set<string> already_seen;
  // loop until no expansion can be found
  do {
    if (account_aliases.size() > 0) {
      if (auto i = account_aliases.find(name); i != account_aliases.end()) {
        if (already_seen.count(name) > 0) {
          throw_(std::runtime_error, _f("Infinite recursion on alias expansion for %1%") % name);
        }
        // there is an alias for the full account name, including colons
        already_seen.insert(name);
        result = (*i).second;
        name = result->fullname();
      } else {
        // Check each account name component for alias expansion (#836).
        // For "A:B:C", try "A", then "B", then "C" — expand the first
        // component that matches an alias.
        size_t pos = 0;
        bool found = false;
        while (pos < name.size()) {
          size_t colon = name.find(':', pos);
          string component =
              (colon != string::npos) ? name.substr(pos, colon - pos) : name.substr(pos);

          if (auto j = account_aliases.find(component);
              j != account_aliases.end() && already_seen.count(component) == 0) {
            already_seen.insert(component);

            string new_name;
            if (pos > 0)
              new_name = name.substr(0, pos); // prefix including trailing ':'
            new_name += (*j).second->fullname();
            if (colon != string::npos)
              new_name += name.substr(colon); // suffix including leading ':'
            result = find_account(new_name);
            name = result->fullname();
            found = true;
            break;
          }

          if (colon == string::npos)
            break;
          pos = colon + 1;
        }
        if (!found)
          keep_expanding = false;
      }
    } else {
      keep_expanding = false;
    }
  } while (keep_expanding && recursive_aliases);

  // Detect complete cycles: if after expansion the name is back to the
  // original, the aliases form a cycle (e.g., alias A=B + alias B=A).
  if (recursive_aliases && !already_seen.empty() && name == original_name) {
    throw_(std::runtime_error, _f("Infinite recursion on alias expansion for %1%") % name);
  }

  return result;
}

/*--- Payee Validation ---*/

string journal_t::register_payee(const string& name) {
  if (should_check_payees() && payee_not_registered(name)) {
    known_payees.insert(name);
  }

  return name;
}

string journal_t::validate_payee(const string& name_or_alias) {
  string payee = translate_payee_name(name_or_alias);

  if (should_check_payees() && payee_not_registered(payee)) {
    if (checking_style == CHECK_WARNING) {
      current_context->warning(_f("Unknown payee '%1%'") % payee);
    } else if (checking_style == CHECK_ERROR) {
      throw_(parse_error, _f("Unknown payee '%1%'") % payee);
    }
  }

  return payee;
}

bool journal_t::should_check_payees() {
  return check_payees && (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR);
}

bool journal_t::payee_not_registered(const string& name) {
  return known_payees.find(name) == known_payees.end();
}

string journal_t::translate_payee_name(const string& name) {
  string payee;

  for (payee_alias_mapping_t& value : payee_alias_mappings) {
    if (value.first.match(name)) {
      payee = value.second;
      break;
    }
  }

  return payee.empty() ? name : payee;
}

/*--- Commodity and Metadata Registration ---*/

void journal_t::register_commodity(commodity_t& comm, std::variant<int, xact_t*, post_t*> context) {
  if (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR) {
    if (!comm.has_flags(COMMODITY_KNOWN)) {
      if (context.index() == 0) {
        comm.add_flags(COMMODITY_KNOWN);
      } else if (checking_style == CHECK_WARNING) {
        current_context->warning(_f("Unknown commodity '%1%'") % comm);
      } else if (checking_style == CHECK_ERROR) {
        throw_(parse_error, _f("Unknown commodity '%1%'") % comm);
      }
    }
  }
}

/**
 * @brief Validate a metadata tag and evaluate tag_check_exprs assertions.
 *
 * First checks the tag name against known_tags when --strict/--pedantic
 * is active.  Then, if there are any check/assert expressions registered
 * for this tag (via `tag Foo assert ...`), evaluates each one against
 * the metadata value.  A failing assertion throws; a failing check warns.
 */
void journal_t::register_metadata(const string& key, const value_t& value,
                                  std::variant<int, xact_t*, post_t*> context) {
  if (checking_style == CHECK_WARNING || checking_style == CHECK_ERROR) {
    std::set<string>::iterator i = known_tags.find(key);

    // NOLINTBEGIN(bugprone-branch-clone)
    if (i == known_tags.end()) {
      if (context.index() == 0) {
        known_tags.insert(key);
      } else if (checking_style == CHECK_WARNING) {
        current_context->warning(_f("Unknown metadata tag '%1%'") % key);
      } else if (checking_style == CHECK_ERROR) {
        throw_(parse_error, _f("Unknown metadata tag '%1%'") % key);
      }
    }
    // NOLINTEND(bugprone-branch-clone)
  }

  if (!value.is_null() && context.index() != 0) {
    auto [range_begin, range_end] = tag_check_exprs.equal_range(key);

    for (auto i = range_begin; i != range_end; ++i) {
      bind_scope_t bound_scope(*current_context->scope,
                               context.index() == 1
                                   ? static_cast<scope_t&>(*std::get<xact_t*>(context))
                                   : static_cast<scope_t&>(*std::get<post_t*>(context)));
      value_scope_t val_scope(bound_scope, value);

      (*i).second.first.mark_uncompiled();
      bool holds = (*i).second.first.calc(val_scope).to_boolean();
      // Clear context immediately: val_scope is stack-allocated and will be
      // destroyed when this scope exits.  Leaving a dangling pointer behind
      // would cause a use-after-free the next time the expression is compiled.
      (*i).second.first.set_context(nullptr);
      if (!holds) {
        if ((*i).second.second == expr_t::EXPR_ASSERTION)
          throw_(parse_error, _f("Metadata assertion failed for (%1%: %2%): %3%") % key % value %
                                  (*i).second.first);
        else
          current_context->warning(_f("Metadata check failed for (%1%: %2%): %3%") % key % value %
                                   (*i).second.first);
      }
    }
  }
}

namespace {
/** @brief Iterate all metadata on a transaction or posting and validate each tag.
 *
 * Temporarily overrides the journal's current_context pathname and linenum
 * with the item's source position so that --strict warnings reference the
 * correct file and line (fixes #750 and #692).
 */
void check_all_metadata(journal_t& journal, std::variant<int, xact_t*, post_t*> context) {
  xact_t* xact = context.index() == 1 ? std::get<xact_t*>(context) : NULL;
  post_t* post = context.index() == 2 ? std::get<post_t*>(context) : NULL;

  if ((xact || post) && (xact ? xact->metadata : post->metadata)) {
    item_t* item = xact ? static_cast<item_t*>(xact) : static_cast<item_t*>(post);

    path saved_pathname;
    std::size_t saved_linenum = 0;
    if (journal.current_context && item->pos) {
      saved_pathname = journal.current_context->pathname;
      saved_linenum = journal.current_context->linenum;
      journal.current_context->pathname = item->pos->pathname;
      journal.current_context->linenum = item->pos->beg_line;
    }

    for (const item_t::string_map::value_type& pair : xact ? *xact->metadata : *post->metadata) {
      const string& key(pair.first);

      const std::optional<value_t>& value = pair.second.first;
      if (value)
        journal.register_metadata(key, *value, context);
      else
        journal.register_metadata(key, NULL_VALUE, context);
    }

    if (journal.current_context && item->pos) {
      journal.current_context->pathname = saved_pathname;
      journal.current_context->linenum = saved_linenum;
    }
  }
}
} // namespace

/*--- Transaction Management ---*/

/** @brief Compare postings by account pointer for sorting during UUID dedup. */
bool lt_posting_account(post_t* left, post_t* right) {
  return left->account < right->account;
}

/** @brief True if two postings have the same account and amount (for UUID dedup). */
bool is_equivalent_posting(post_t* left, post_t* right) {
  if (left->account != right->account)
    return false;

  if (left->amount != right->amount)
    return false;

  return true;
}

/**
 * @brief Add a parsed transaction to the journal.
 *
 * This is the main entry point for incorporating a transaction after
 * parsing.  The sequence is:
 *   1. Finalize the transaction (balance check, null-amount inference).
 *   2. Extend with auto_xacts (automated transaction matching).
 *   3. Validate all metadata on the transaction.
 *   4. Extend each posting and validate its metadata.
 *   5. UUID deduplication: if a transaction with the same UUID already
 *      exists, verify that the postings are equivalent (same accounts
 *      and amounts).  If they match, apply any deferred posts and
 *      silently discard the duplicate.  If they differ, throw an error.
 *
 * @return true if the transaction was added, false if rejected.
 */
bool journal_t::add_xact(xact_t* xact) {
  xact->journal = this;

  if (!xact->finalize()) {
    xact->journal = nullptr;
    return false;
  }

  extend_xact(xact);
  check_all_metadata(*this, xact);

  for (post_t* post : xact->posts) {
    extend_post(*post, *this);
    check_all_metadata(*this, post);
  }

  // If a transaction with this UUID has already been seen, simply do
  // not add this one to the journal.  However, all automated checks
  // will have been performed by extend_xact, so asserts can still be
  // applied to it.
  if (std::optional<value_t> ref = xact->get_tag(_("UUID"))) {
    std::string uuid = ref->to_string(); // NOLINT(bugprone-unused-local-non-trivial-variable)
    auto [iter, inserted] = checksum_map.insert(checksum_map_t::value_type(uuid, xact));
    if (!inserted) {
      // This UUID has been seen before; apply any postings which the
      // earlier version may have deferred.
      for (post_t* post : xact->posts) {
        account_t* acct = post->account;
        if (acct->deferred_posts) {
          auto i = acct->deferred_posts->find(uuid);
          if (i != acct->deferred_posts->end()) {
            for (post_t* rpost : (*i).second)
              if (acct == rpost->account)
                acct->add_post(rpost);
            acct->deferred_posts->erase(i);
          }
        }
      }

      xact_t* other = iter->second;

      // Copy the two lists of postings (which should be relatively
      // short), and make sure that the intersection is the empty set
      // (i.e., that they are the same list).
      std::vector<post_t*> this_posts(xact->posts.begin(), xact->posts.end());
      std::sort(this_posts.begin(), this_posts.end(), lt_posting_account);
      std::vector<post_t*> other_posts(other->posts.begin(), other->posts.end());
      std::sort(other_posts.begin(), other_posts.end(), lt_posting_account);
      bool match = std::equal(this_posts.begin(), this_posts.end(), other_posts.begin(),
                              is_equivalent_posting);

      if (!match || this_posts.size() != other_posts.size()) {
        add_error_context(_("While comparing this previously seen transaction:"));
        add_error_context(
            source_context(other->pos->pathname, other->pos->beg_pos, other->pos->end_pos, "> "));
        add_error_context(_("to this later transaction:"));
        add_error_context(
            source_context(xact->pos->pathname, xact->pos->beg_pos, xact->pos->end_pos, "> "));
        throw_(std::runtime_error,
               _f("Transactions with the same UUID must have equivalent postings"));
      }

      xact->journal = nullptr;
      return false;
    }
  }

  xacts.push_back(xact);

  return true;
}

void journal_t::extend_xact(xact_base_t* xact) {
  if (!current_context)
    return;
  // Run posting-generating auto_xacts first, then assertion-only ones.
  // Assertion-only auto_xacts (check_exprs but no template postings) need
  // to see posts generated by earlier auto_xacts, so they must run after
  // all posting generators have finished (fixes #1680).
  for (unique_ptr<auto_xact_t>& auto_xact : auto_xacts)
    if (!auto_xact->posts.empty())
      auto_xact->extend_xact(*xact, *current_context);
  for (unique_ptr<auto_xact_t>& auto_xact : auto_xacts)
    if (auto_xact->posts.empty())
      auto_xact->extend_xact(*xact, *current_context);
}

bool journal_t::remove_xact(xact_t* xact) {
  bool found = false;
  xacts_list::iterator i;
  for (i = xacts.begin(); i != xacts.end(); i++)
    if (*i == xact) {
      found = true;
      break;
    }
  if (!found)
    return false;

  xacts.erase(i);
  xact->journal = nullptr;

  return true;
}

/*--- Journal I/O ---*/

/**
 * @brief Parse a journal file (or stream) and add its contents.
 *
 * Sets up the parse context (scope, master account), delegates to
 * read_textual() for the actual parsing, records the source file info
 * for change detection, and clears any xdata that was set during parsing
 * (e.g., by balance assertions or valuation expressions in posting amounts).
 */
std::size_t journal_t::read(parse_context_stack_t& context, hash_type_t hash_type,
                            bool should_clear_xdata) {
  std::size_t count = 0;
  try {
    parse_context_t& current(context.get_current());
    current_context = &current;

    current.count = 0;
    if (!current.scope)
      current.scope = scope_t::default_scope;

    if (!current.scope)
      throw_(std::runtime_error,
             _f("No default scope in which to read journal file '%1%'") % current.pathname);

    if (!current.master)
      current.master = master;

    count = read_textual(context, hash_type);
    if (count > 0) {
      if (!current.pathname.empty())
        sources.push_back(fileinfo_t(current.pathname));
      else
        sources.push_back(fileinfo_t());
    }
  } catch (...) {
    clear_xdata();
    current_context = nullptr;
    throw;
  }

  // xdata may have been set for some accounts and transaction due to the use
  // of balance assertions or other calculations performed in valexpr-based
  // posting amounts.
  if (should_clear_xdata)
    clear_xdata();

  current_context = nullptr;

  return count;
}

/*--- Validation and Cleanup ---*/

bool journal_t::has_xdata() {
  for (xact_t* xact : xacts)
    if (xact->has_xdata())
      return true;

  for (unique_ptr<auto_xact_t>& xact : auto_xacts)
    if (xact->has_xdata())
      return true;

  for (period_xact_t* xact : period_xacts)
    if (xact->has_xdata())
      return true;

  if (master->has_xdata() || master->children_with_xdata())
    return true;

  return false;
}

void journal_t::clear_xdata() {
  for (xact_t* xact : xacts)
    if (!xact->has_flags(ITEM_TEMP))
      xact->clear_xdata();

  for (unique_ptr<auto_xact_t>& xact : auto_xacts)
    if (!xact->has_flags(ITEM_TEMP))
      xact->clear_xdata();

  for (period_xact_t* xact : period_xacts)
    if (!xact->has_flags(ITEM_TEMP))
      xact->clear_xdata();

  master->clear_xdata();
}

bool journal_t::valid() const {
  if (!master->valid()) {
    DEBUG("ledger.validate", "journal_t: master not valid");
    return false;
  }

  for (const xact_t* xact : xacts)
    if (!xact->valid()) {
      DEBUG("ledger.validate", "journal_t: xact not valid");
      return false;
    }

  return true;
}

} // namespace ledger
