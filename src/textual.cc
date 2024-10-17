/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "journal.h"
#include "context.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "option.h"
#include "query.h"
#include "pstream.h"
#include "pool.h"
#include "session.h"
#include <algorithm>
#if HAVE_BOOST_PYTHON
#include "pyinterp.h"
#endif

#define TIMELOG_SUPPORT 1
#if TIMELOG_SUPPORT
#include "timelog.h"
#endif

namespace ledger {

namespace {
  typedef std::pair<commodity_t *, amount_t> fixed_rate_t;

  struct application_t
  {
    string label;
    variant<optional<datetime_t>, account_t *, string, fixed_rate_t> value;

    application_t(string _label, optional<datetime_t> epoch)
      : label(_label), value(epoch) {}
    application_t(string _label, account_t * acct)
      : label(_label), value(acct) {}
    application_t(string _label, string tag)
      : label(_label), value(tag) {}
    application_t(string _label, fixed_rate_t rate)
      : label(_label), value(rate) {}
  };

  class instance_t : public noncopyable, public scope_t
  {
  public:
    parse_context_stack_t&   context_stack;
    parse_context_t&         context;
    std::istream&            in;
    instance_t *             parent;
    std::list<application_t> apply_stack;
    bool                     no_assertions;
    hash_type_t              hash_type;
#if TIMELOG_SUPPORT
    time_log_t               timelog;
#endif

    instance_t(parse_context_stack_t& _context_stack,
               parse_context_t&       _context,
               instance_t *           _parent = NULL,
               const bool             _no_assertions = false,
               const hash_type_t      _hash_type = NO_HASHES)
      : context_stack(_context_stack), context(_context),
        in(*context.stream.get()), parent(_parent),
        no_assertions(_no_assertions), hash_type(_hash_type),
        timelog(context) {}

    virtual string description() {
      return _("textual parser");
    }

    template <typename T>
    void get_applications(std::vector<T>& result) {
      foreach (application_t& state, apply_stack) {
        if (state.value.type() == typeid(T))
          result.push_back(boost::get<T>(state.value));
      }
      if (parent)
        parent->get_applications<T>(result);
    }

    template <typename T>
    optional<T> get_application() {
      foreach (application_t& state, apply_stack) {
        if (state.value.type() == typeid(T))
          return boost::get<T>(state.value);
      }
      return parent ? parent->get_application<T>() : none;
    }

    account_t * top_account() {
      if (optional<account_t *> acct = get_application<account_t *>())
        return *acct;
      else
        return NULL;
    }

    void parse();

    std::streamsize read_line(char *& line);

    bool peek_whitespace_line() {
      return (in.good() && ! in.eof() &&
              (in.peek() == ' ' || in.peek() == '\t'));
    }
#if HAVE_BOOST_PYTHON
    bool peek_blank_line() {
      return (in.good() && ! in.eof() &&
              (in.peek() == '\n' || in.peek() == '\r'));
    }
#endif

    xact_t * read_next_directive(bool& error_flag, xact_t * previous_xact);

#if TIMELOG_SUPPORT
    void clock_in_directive(char * line, bool capitalized);
    void clock_out_directive(char * line, bool capitalized);
#endif

    bool general_directive(char * line);

    void account_directive(char * line);
    void account_alias_directive(account_t * account, string alias);
    void account_payee_directive(account_t * account, string payee);
    void account_value_directive(account_t * account, string expr_str);
    void account_default_directive(account_t * account);

    void default_account_directive(char * args);
    void alias_directive(char * line);

    void payee_directive(char * line);
    void payee_alias_directive(const string& payee, string alias);
    void payee_uuid_directive(const string& payee, string uuid);

    void commodity_directive(char * line);
    void commodity_alias_directive(commodity_t& comm, string alias);
    void commodity_value_directive(commodity_t& comm, string expr_str);
    void commodity_format_directive(commodity_t& comm, string format);
    void commodity_nomarket_directive(commodity_t& comm);
    void commodity_default_directive(commodity_t& comm);

    void default_commodity_directive(char * line);

    void tag_directive(char * line);

    void apply_directive(char * line);
    void apply_account_directive(char * line);
    void apply_tag_directive(char * line);
    void apply_rate_directive(char * line);
    void apply_year_directive(char * line);
    void end_apply_directive(char * line);

    xact_t * xact_directive(char * line, std::streamsize len,
                            xact_t * previous_xact);
    void period_xact_directive(char * line);
    void automated_xact_directive(char * line);
    void price_xact_directive(char * line);
    void price_conversion_directive(char * line);
    void nomarket_directive(char * line);

    void include_directive(char * line);
    void option_directive(char * line);
    void comment_directive(char * line);

    void eval_directive(char * line);
    void assert_directive(char * line);
    void check_directive(char * line);
    void value_directive(char * line);

    void import_directive(char * line);
    void python_directive(char * line);

    post_t * parse_post(char *          line,
                        std::streamsize len,
                        account_t *     account,
                        xact_t *        xact,
                        bool            defer_expr = false);

    bool parse_posts(account_t *  account,
                     xact_base_t& xact,
                     const bool   defer_expr = false);

    xact_t * parse_xact(char *          line,
                        std::streamsize len,
                        account_t *     account,
                        xact_t *        previous_xact);

    virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                    const string& name);
  };

  void parse_amount_expr(std::istream&        in,
                         scope_t&             scope,
                         post_t&              post,
                         amount_t&            amount,
                         const parse_flags_t& flags       = PARSE_DEFAULT,
                         const bool           defer_expr  = false,
                         optional<expr_t> *   amount_expr = NULL)
  {
    expr_t expr(in, flags.plus_flags(PARSE_PARTIAL));

    DEBUG("textual.parse", "Parsed an amount expression");

    if (expr) {
      if (amount_expr)
        *amount_expr = expr;
      if (! defer_expr)
        amount = post.resolve_expr(scope, expr);
    }
  }
}

void instance_t::parse()
{
  INFO("Parsing file " << context.pathname);

  TRACE_START(instance_parse, 1, "Done parsing file " << context.pathname);

  if (! in.good() || in.eof())
    return;

  context.linenum  = 0;
  context.curr_pos = in.tellg();

  bool error_flag = false;
  xact_t * previous_xact = NULL;

  while (in.good() && ! in.eof()) {
    try {
      if (xact_t * xact = read_next_directive(error_flag, previous_xact)) {
        previous_xact = xact;
      }
    }
    catch (const std::exception& err) {
      error_flag = true;

      string current_context = error_context();

      if (parent) {
        std::list<instance_t *> instances;

        for (instance_t * instance = parent;
             instance;
             instance = instance->parent)
          instances.push_front(instance);

        foreach (instance_t * instance, instances)
          add_error_context(_f("In file included from %1%")
                            % instance->context.location());
      }
      add_error_context(_f("While parsing file %1%") % context.location());

      if (caught_signal != NONE_CAUGHT)
        throw;

      string err_context = error_context();
      if (! err_context.empty())
        std::cerr << err_context << std::endl;

      if (! current_context.empty())
        std::cerr << current_context << std::endl;

      std::cerr << _("Error: ") << err.what() << std::endl;
      context.errors++;
      if (! current_context.empty())
          context.last = current_context + "\n" + err.what();
      else
          context.last = err.what();
    }
  }

  if (apply_stack.front().value.type() == typeid(optional<datetime_t>))
    epoch = boost::get<optional<datetime_t> >(apply_stack.front().value);

  apply_stack.pop_front();

#if TIMELOG_SUPPORT
  timelog.close();
#endif // TIMELOG_SUPPORT

  TRACE_STOP(instance_parse, 1);
}

std::streamsize instance_t::read_line(char *& line)
{
  assert(in.good());
  assert(! in.eof());           // no one should call us in that case

  context.line_beg_pos = context.curr_pos;

  check_for_signal();

  const size_t maxLine = parse_context_t::MAX_LINE;
  in.getline(context.linebuf, maxLine);
  std::streamsize len = in.gcount();

  if (in.fail() && len == (parse_context_t::MAX_LINE - 1)) {
      throw_(parse_error, _f("Line exceeds %1% characters") % maxLine);
  }

  if (len > 0) {
    context.linenum++;

    context.curr_pos  = context.line_beg_pos;
    context.curr_pos += len;

    if (context.linenum == 0 &&
        utf8::starts_with_bom(
          context.linebuf, context.linebuf + sizeof(context.linebuf))) {
      line = &context.linebuf[3];
      len -= 3;
    } else {
      line = context.linebuf;
    }

    if (!in.eof()) {
        // if we are not at the end of the file, len includes the new line character,
        // even through it does not appear in linebuf
        --len;
    }

    // strip trailing whitespace
    while (len > 0 && std::isspace(static_cast<unsigned char>(line[len - 1])))
      line[--len] = '\0';

    return len;
  }
  return 0;
}

xact_t * instance_t::read_next_directive(bool& error_flag, xact_t * previous_xact)
{
  char * line;
  std::streamsize len = read_line(line);
  if (len == 0 || line == NULL)
    return NULL;

  if (! std::isspace(static_cast<unsigned char>(line[0])))
    error_flag = false;

  switch (line[0]) {
  case '\0':
    assert(false);              // shouldn't ever reach here
    break;

  case ' ':
  case '\t':
    if (! error_flag)
      throw parse_error(_("Unexpected whitespace at beginning of line"));
    break;

  case ';':                     // comments
  case '#':
  case '*':
  case '|':
    break;

  case '-':                     // option setting
    option_directive(line);
    break;

  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    return xact_directive(line, len, previous_xact);
  case '=':                     // automated xact
    automated_xact_directive(line);
    break;
  case '~':                     // period xact
    period_xact_directive(line);
    break;

  case '@':
  case '!':
    line++;
    // fall through...
  default:                      // some other directive
    if (! general_directive(line)) {
      switch (line[0]) {
#if TIMELOG_SUPPORT
      case 'i':
        clock_in_directive(line, false);
        break;
      case 'I':
        clock_in_directive(line, true);
        break;

      case 'o':
        clock_out_directive(line, false);
        break;
      case 'O':
        clock_out_directive(line, true);
        break;

      case 'h':
      case 'b':
        break;
#endif // TIMELOG_SUPPORT

      case 'A':                 // a default account for unbalanced posts
        default_account_directive(line + 1);
        break;
      case 'C':                 // a set of conversions
        price_conversion_directive(line);
        break;
      case 'D':                 // a default commodity for "xact"
        default_commodity_directive(line);
        break;
      case 'N':                 // don't download prices
        nomarket_directive(line);
        break;
      case 'P':                 // a pricing xact
        price_xact_directive(line);
        break;
      case 'Y':                 // set the current year
        if (std::strlen(line+1) == 0)
          throw_(parse_error, _f("Directive '%1%' requires an argument") % line[0]);
        apply_year_directive(line+1);
        break;
      }
    }
    break;
  }

  return NULL;
}

#if TIMELOG_SUPPORT

void instance_t::clock_in_directive(char * line, bool capitalized)
{
  string datetime(line, 2, 19);

  char * p   = skip_ws(line + 22);
  char * n   = next_element(p, true);
  char * end = n ? next_element(n, true) : NULL;

  if (end && *end == ';')
    end = skip_ws(end + 1);
  else
    end = NULL;

  position_t position;
  position.pathname = context.pathname;
  position.beg_pos  = context.line_beg_pos;
  position.beg_line = context.linenum;
  position.end_pos  = context.curr_pos;
  position.end_line = context.linenum;
  position.sequence = context.sequence++;

  time_xact_t event(position, parse_datetime(datetime), capitalized,
                    p ? top_account()->find_account(p) : NULL,
                    n ? n : "",
                    end ? end : "");

  timelog.clock_in(event);
}

void instance_t::clock_out_directive(char * line, bool capitalized)
{
  string datetime(line, 2, 19);

  char * p = skip_ws(line + 22);
  char * n = next_element(p, true);
  char * end = n ? next_element(n, true) : NULL;

  if (end && *end == ';')
    end = skip_ws(end + 1);
  else
    end = NULL;

  position_t position;
  position.pathname = context.pathname;
  position.beg_pos  = context.line_beg_pos;
  position.beg_line = context.linenum;
  position.end_pos  = context.curr_pos;
  position.end_line = context.linenum;
  position.sequence = context.sequence++;

  time_xact_t event(position, parse_datetime(datetime), capitalized,
                    p ? top_account()->find_account(p) : NULL,
                    n ? n : "",
                    end ? end : "");

  context.count += timelog.clock_out(event);
}

#endif // TIMELOG_SUPPORT

void instance_t::default_commodity_directive(char * line)
{
  amount_t amt(skip_ws(line + 1));
  VERIFY(amt.valid());
  commodity_pool_t::current_pool->default_commodity = &amt.commodity();
  amt.commodity().add_flags(COMMODITY_KNOWN);
}

void instance_t::default_account_directive(char * line)
{
  context.journal->bucket = top_account()->find_account(skip_ws(line));
  context.journal->bucket->add_flags(ACCOUNT_KNOWN);
}

void instance_t::price_conversion_directive(char * line)
{
  if (char * p = std::strchr(line + 1, '=')) {
    *p++ = '\0';
    amount_t::parse_conversion(line + 1, p);
  }
}

void instance_t::price_xact_directive(char * line)
{
  optional<std::pair<commodity_t *, price_point_t> > point =
    commodity_pool_t::current_pool->parse_price_directive(skip_ws(line + 1));
  if (! point)
    throw parse_error(_("Pricing entry failed to parse"));
}

void instance_t::nomarket_directive(char * line)
{
  char * p = skip_ws(line + 1);
  string symbol;
  commodity_t::parse_symbol(p, symbol);

  if (commodity_t * commodity =
      commodity_pool_t::current_pool->find_or_create(symbol))
    commodity->add_flags(COMMODITY_NOMARKET | COMMODITY_KNOWN);
}

void instance_t::option_directive(char * line)
{
  char * p = next_element(line);
  if (! p) {
    p = std::strchr(line, '=');
    if (p)
      *p++ = '\0';
  }

  if (! process_option(context.pathname.string(), line + 2, *context.scope,
                       p, line))
    throw_(option_error, _f("Illegal option --%1%") % (line + 2));
}

void instance_t::automated_xact_directive(char * line)
{
  std::istream::pos_type pos = context.line_beg_pos;

  bool reveal_context = true;

  try {
    query_t          query;
    keep_details_t   keeper(true, true, true);
    expr_t::ptr_op_t expr =
      query.parse_args(string_value(skip_ws(line + 1)).to_sequence(),
                       keeper, false, true);
    if (!expr) {
      throw parse_error(_("Expected predicate after '='"));
    }

    unique_ptr<auto_xact_t> ae(new auto_xact_t(predicate_t(expr, keeper)));
    ae->pos           = position_t();
    ae->pos->pathname = context.pathname;
    ae->pos->beg_pos  = context.line_beg_pos;
    ae->pos->beg_line = context.linenum;
    ae->pos->sequence = context.sequence++;

    post_t * last_post = NULL;

    while (peek_whitespace_line()) {
      std::streamsize len = read_line(line);
      char * p = skip_ws(line);
      if (! *p)
        break;

      const std::size_t remlen = std::strlen(p);

      if (*p == ';') {
        item_t * item;
        if (last_post)
          item = last_post;
        else
          item = ae.get();

        // This is a trailing note, and possibly a metadata info tag
        ae->append_note(p + 1, *context.scope, true);
        item->add_flags(ITEM_NOTE_ON_NEXT_LINE);
        item->pos->end_pos = context.curr_pos;
        item->pos->end_line++;
      }
      else if ((remlen > 7 && *p == 'a' &&
                std::strncmp(p, "assert", 6) == 0 &&
                std::isspace(static_cast<unsigned char>(p[6]))) ||
               (remlen > 6 && *p == 'c' &&
                std::strncmp(p, "check", 5) == 0 &&
                std::isspace(static_cast<unsigned char>(p[5]))) ||
               (remlen > 5 && *p == 'e' &&
                ((std::strncmp(p, "expr", 4) == 0 &&
                  std::isspace(static_cast<unsigned char>(p[4]))) ||
                 (std::strncmp(p, "eval", 4) == 0 &&
                  std::isspace(static_cast<unsigned char>(p[4])))))) {
        const char c = *p;
        p = skip_ws(&p[*p == 'a' ? 6 : (*p == 'c' ? 5 : 4)]);
        if (! ae->check_exprs)
          ae->check_exprs = expr_t::check_expr_list();
        ae->check_exprs->push_back
          (expr_t::check_expr_pair(expr_t(p),
                                   c == 'a' ?
                                   expr_t::EXPR_ASSERTION :
                                   (c == 'c' ?
                                    expr_t::EXPR_CHECK :
                                    expr_t::EXPR_GENERAL)));
      }
      else {
        reveal_context = false;

        if (post_t * post =
            parse_post(p, len - (p - line), top_account(), NULL, true)) {
          reveal_context = true;
          ae->add_post(post);
          ae->active_post = last_post = post;
        }
        reveal_context = true;
      }
    }

    context.journal->auto_xacts.push_back(ae.get());

    ae->journal       = context.journal;
    ae->pos->end_pos  = context.curr_pos;
    ae->pos->end_line = context.linenum;

    ae.release();
  }
  catch (const std::exception&) {
    if (reveal_context) {
      add_error_context(_("While parsing automated transaction:"));
      add_error_context(source_context(context.pathname, pos,
                                       context.curr_pos, "> "));
    }
    throw;
  }
}

void instance_t::period_xact_directive(char * line)
{
  std::istream::pos_type pos = context.line_beg_pos;

  bool reveal_context = true;

  try {

  unique_ptr<period_xact_t> pe(new period_xact_t(skip_ws(line + 1)));
  pe->pos           = position_t();
  pe->pos->pathname = context.pathname;
  pe->pos->beg_pos  = context.line_beg_pos;
  pe->pos->beg_line = context.linenum;
  pe->pos->sequence = context.sequence++;

  reveal_context = false;

  if (parse_posts(top_account(), *pe.get())) {
    reveal_context = true;
    pe->journal = context.journal;

    if (pe->finalize()) {
      context.journal->extend_xact(pe.get());
      context.journal->period_xacts.push_back(pe.get());

      pe->pos->end_pos  = context.curr_pos;
      pe->pos->end_line = context.linenum;

      pe.release();
    } else {
      reveal_context = true;
      pe->journal = NULL;
      throw parse_error(_("Period transaction failed to balance"));
    }
  }

  }
  catch (const std::exception&) {
    if (reveal_context) {
      add_error_context(_("While parsing periodic transaction:"));
      add_error_context(source_context(context.pathname, pos,
                                       context.curr_pos, "> "));
    }
    throw;
  }
}

xact_t * instance_t::xact_directive(char * line, std::streamsize len,
                                    xact_t * previous_xact)
{
  TRACE_START(xacts, 1, "Time spent handling transactions:");

  if (xact_t * xact = parse_xact(line, len, top_account(), previous_xact)) {
    unique_ptr<xact_t> manager(xact);

    if (context.journal->add_xact(xact)) {
      context.count++;
      return manager.release(); // it's owned by the journal now
    }
    // It's perfectly valid for the journal to reject the xact, which it
    // will do if the xact has no substantive effect (for example, a
    // checking xact, all of whose postings have null amounts).
  } else {
    throw parse_error(_("Failed to parse transaction"));
  }

  TRACE_STOP(xacts, 1);

  return NULL;
}

void instance_t::include_directive(char * line)
{
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
  path   parent_path = filename.parent_path();
  glob.assign_glob('^' + filename.filename().string() + '$');

  bool files_found = false;
  if (exists(parent_path)) {
    filesystem::directory_iterator end;

    // Sort parent_path since on some file systems it is unsorted.
    std::vector<path> sorted_parent_path;
    std::copy(filesystem::directory_iterator(parent_path),
              filesystem::directory_iterator(),
              std::back_inserter(sorted_parent_path));
    std::sort(sorted_parent_path.begin(), sorted_parent_path.end());

		for (std::vector<path>::const_iterator iter(sorted_parent_path.begin()),
					 it_end(sorted_parent_path.end()); iter != it_end; ++iter) {
      if (is_regular_file(*iter))
        {
        string base = (*iter).filename().string();
        if (glob.match(base)) {
          journal_t *  journal  = context.journal;
          account_t *  master   = top_account();
          scope_t *    scope    = context.scope;
          std::size_t& errors   = context.errors;
          std::size_t& count    = context.count;
          std::size_t& sequence = context.sequence;

          DEBUG("textual.include", "Including: " << *iter);
          DEBUG("textual.include", "Master account: " << master->fullname());

          context_stack.push(*iter);

          context_stack.get_current().journal = journal;
          context_stack.get_current().master  = master;
          context_stack.get_current().scope   = scope;
          try {
            instance_t instance(context_stack, context_stack.get_current(),
                                this, no_assertions, hash_type);
            instance.apply_stack.push_front(application_t("account", master));
            instance.parse();
          }
          catch (...) {
            errors   += context_stack.get_current().errors;
            count    += context_stack.get_current().count;
            sequence += context_stack.get_current().sequence;

            context_stack.pop();
            throw;
          }

          errors   += context_stack.get_current().errors;
          count    += context_stack.get_current().count;
          sequence += context_stack.get_current().sequence;

          context_stack.pop();

          files_found = true;
        }
      }
    }
  }

  if (! files_found)
    throw_(std::runtime_error,
           _f("File to include was not found: %1%") % filename);

}

void instance_t::apply_directive(char * line)
{
  char * b = next_element(line);
  string keyword(line);
  if (! b)
    throw_(parse_error, _f("Directive 'apply %1%' requires an argument") % keyword);
  if (keyword == "account")
    apply_account_directive(b);
  else if (keyword == "tag")
    apply_tag_directive(b);
  else if (keyword == "fixed" || keyword == "rate")
    apply_rate_directive(b);
  else if (keyword == "year")
    apply_year_directive(b);
}

void instance_t::apply_account_directive(char * line)
{
  if (account_t * acct = top_account()->find_account(line))
    apply_stack.push_front(application_t("account", acct));
  else
    assert("Failed to create account" == NULL);
}

void instance_t::apply_tag_directive(char * line)
{
  string tag(trim_ws(line));

  if (tag.find(':') == string::npos)
    tag = string(":") + tag + ":";

  apply_stack.push_front(application_t("tag", tag));
}

void instance_t::apply_rate_directive(char * line)
{
  if (optional<std::pair<commodity_t *, price_point_t> > price_point =
      commodity_pool_t::current_pool->parse_price_directive
        (trim_ws(line), true, true)) {
    apply_stack.push_front
      (application_t("fixed", fixed_rate_t(price_point->first,
                                           price_point->second.price)));
  } else {
    throw_(std::runtime_error, _("Error in fixed directive"));
  }
}

void instance_t::apply_year_directive(char * line)
{
  try {
    unsigned short year(lexical_cast<unsigned short>(skip_ws(line)));
    apply_stack.push_front(application_t("year", epoch));
    DEBUG("times.epoch", "Setting current year to " << year);
    // This must be set to the last day of the year, otherwise partial
    // dates like "11/01" will refer to last year's November, not the
    // current year.
    epoch = datetime_t(date_t(year, 12, 31));
  } catch(bad_lexical_cast &) {
    throw_(parse_error, _f("Argument '%1%' not a valid year") % skip_ws(line));
  }
}

void instance_t::end_apply_directive(char * kind)
{
  char * b = kind ? next_element(kind) : NULL;
  string name(b ? b : "");

  if (apply_stack.size() <= 1) {
    if (name.empty()) {
      throw_(std::runtime_error,
             _("'end' or 'end apply' found, but no enclosing 'apply' directive"));
    } else {
      throw_(std::runtime_error,
             _f("'end apply %1%' found, but no enclosing 'apply' directive")
             % name);
    }
  }

  if (! name.empty() && name != apply_stack.front().label)
    throw_(std::runtime_error,
           _f("'end apply %1%' directive does not match 'apply %2%' directive")
           % name % apply_stack.front().label);

  if (apply_stack.front().value.type() == typeid(optional<datetime_t>))
    epoch = boost::get<optional<datetime_t> >(apply_stack.front().value);

  apply_stack.pop_front();
}

void instance_t::account_directive(char * line)
{
  std::istream::pos_type beg_pos     = context.line_beg_pos;
  std::size_t            beg_linenum = context.linenum;

  char * p = skip_ws(line);
  account_t * account =
    context.journal->register_account(p, NULL, top_account());
  unique_ptr<auto_xact_t> ae;

  while (peek_whitespace_line()) {
    read_line(line);
    char * q = skip_ws(line);
    if (! *q)
      break;

    char * b = next_element(q);
    string keyword(q);
    // Ensure there's an argument for the directives that need one.
    if (! b && keyword != "default")
      throw_(parse_error, _f("Account directive '%1%' requires an argument") % keyword);

    if (keyword == "alias") {
      account_alias_directive(account, b);
    }
    else if (keyword == "payee") {
      account_payee_directive(account, b);
    }
    else if (keyword == "value") {
      account_value_directive(account, b);
    }
    else if (keyword == "default") {
      account_default_directive(account);
    }
    else if (keyword == "assert" || keyword == "check") {
      keep_details_t keeper(true, true, true);
      expr_t         expr(string("account == \"") + account->fullname() + "\"");
      predicate_t    pred(expr.get_op(), keeper);

      if (! ae.get()) {
        ae.reset(new auto_xact_t(pred));

        ae->pos           = position_t();
        ae->pos->pathname = context.pathname;
        ae->pos->beg_pos  = beg_pos;
        ae->pos->beg_line = beg_linenum;
        ae->pos->sequence = context.sequence++;
        ae->check_exprs   = expr_t::check_expr_list();
      }

      ae->check_exprs->push_back
        (expr_t::check_expr_pair(expr_t(b),
                                 keyword == "assert" ?
                                 expr_t::EXPR_ASSERTION :
                                 expr_t::EXPR_CHECK));
    }
    else if (keyword == "eval" || keyword == "expr") {
      // jww (2012-02-27): Make account into symbol scopes so that this
      // can be used to override definitions within the account.
      bind_scope_t bound_scope(*context.scope, *account);
      expr_t(b).calc(bound_scope);
    }
    else if (keyword == "note") {
      account->note = b;
    }
  }

  if (ae.get()) {
    context.journal->auto_xacts.push_back(ae.get());

    ae->journal       = context.journal;
    ae->pos->end_pos  = in.tellg();
    ae->pos->end_line = context.linenum;

    ae.release();
  }
}

void instance_t::account_alias_directive(account_t * account, string alias)
{
  // Once we have an alias name (alias) and the target account
  // (account), add a reference to the account in the `account_aliases'
  // map, which is used by the post parser to resolve alias references.
  trim(alias);
  // Ensure that no alias like "alias Foo=Foo" is registered.
  if ( alias == account->fullname()) {
    throw_(parse_error, _f("Illegal alias %1%=%2%")
           % alias % account->fullname());
  }
  std::pair<accounts_map::iterator, bool> result =
    context.journal->account_aliases.insert
      (accounts_map::value_type(alias, account));
  if (! result.second)
    (*result.first).second = account;
}

void instance_t::alias_directive(char * line)
{
  if (char * e = std::strchr(line, '=')) {
    char * z = e - 1;
    while (std::isspace(static_cast<unsigned char>(*z)))
      *z-- = '\0';
    *e++ = '\0';
    e = skip_ws(e);

    account_alias_directive(top_account()->find_account(e), line);
  }
}

void instance_t::account_payee_directive(account_t * account, string payee)
{
  trim(payee);
  context.journal->payees_for_unknown_accounts
    .push_back(account_mapping_t(mask_t(payee), account));
}

void instance_t::account_default_directive(account_t * account)
{
  context.journal->bucket = account;
}

void instance_t::account_value_directive(account_t * account, string expr_str)
{
  account->value_expr = expr_t(expr_str);
}

void instance_t::payee_directive(char * line)
{
  string payee = context.journal->register_payee(line);

  while (peek_whitespace_line()) {
    read_line(line);
    char * p = skip_ws(line);
    if (! *p)
      break;

    char * b = next_element(p);
    string keyword(p);
    if (! b)
      throw_(parse_error, _f("Payee directive '%1%' requires an argument") % keyword);

    if (keyword == "alias")
      payee_alias_directive(payee, b);
    else if (keyword == "uuid")
      payee_uuid_directive(payee, b);
  }
}

void instance_t::payee_alias_directive(const string& payee, string alias)
{
  trim(alias);
  context.journal->payee_alias_mappings
    .push_back(payee_alias_mapping_t(mask_t(alias), payee));
}

void instance_t::payee_uuid_directive(const string& payee, string uuid)
{
  trim(uuid);
  context.journal->payee_uuid_mappings
    .push_back(payee_uuid_mapping_t(uuid, payee));
}

void instance_t::commodity_directive(char * line)
{
  char * p = skip_ws(line);
  string symbol;
  commodity_t::parse_symbol(p, symbol);

  if (commodity_t * commodity =
      commodity_pool_t::current_pool->find_or_create(symbol)) {
    context.journal->register_commodity(*commodity, 0);

    while (peek_whitespace_line()) {
      read_line(line);
      char * q = skip_ws(line);
      if (! *q)
        break;

      char * b = next_element(q);
      string keyword(q);
      // Ensure there's an argument for the directives that need one.
      if (! b && keyword != "nomarket" && keyword != "default")
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

void instance_t::commodity_alias_directive(commodity_t& comm, string alias)
{
  trim(alias);
  commodity_pool_t::current_pool->alias(alias, comm);
}

void instance_t::commodity_value_directive(commodity_t& comm, string expr_str)
{
  comm.set_value_expr(expr_t(expr_str));
}

void instance_t::commodity_format_directive(commodity_t& comm, string format)
{
  // jww (2012-02-27): A format specified this way should turn off
  // observational formatting.
  trim(format);
  amount_t amt;
  amt.parse(format, PARSE_NO_REDUCE);
  if (amt.commodity() != comm)
    throw_(parse_error,
           _f("commodity directive symbol %1% and format directive symbol %2% should be the same")
             % comm.symbol()
             % amt.commodity().symbol());
  amt.commodity().add_flags(COMMODITY_STYLE_NO_MIGRATE);
  VERIFY(amt.valid());
}

void instance_t::commodity_nomarket_directive(commodity_t& comm)
{
  comm.add_flags(COMMODITY_NOMARKET);
}

void instance_t::commodity_default_directive(commodity_t& comm)
{
  commodity_pool_t::current_pool->default_commodity = &comm;
}

void instance_t::tag_directive(char * line)
{
  char * p = skip_ws(line);
  context.journal->register_metadata(p, NULL_VALUE, 0);

  while (peek_whitespace_line()) {
    read_line(line);
    char * q = skip_ws(line);
    if (! *q)
      break;

    char * b = next_element(q);
    string keyword(q);
    if (keyword == "assert" || keyword == "check") {
      context.journal->tag_check_exprs.insert
        (tag_check_exprs_map::value_type
         (string(p), expr_t::check_expr_pair(expr_t(b),
                                             keyword == "assert" ?
                                             expr_t::EXPR_ASSERTION :
                                             expr_t::EXPR_CHECK)));
    }
  }
}

void instance_t::eval_directive(char * line)
{
  expr_t expr(line);
  expr.calc(*context.scope);
}

void instance_t::assert_directive(char * line)
{
  expr_t expr(line);
  if (! expr.calc(*context.scope).to_boolean())
    throw_(parse_error, _f("Assertion failed: %1%") % line);
}

void instance_t::check_directive(char * line)
{
  expr_t expr(line);
  if (! expr.calc(*context.scope).to_boolean())
    context.warning(_f("Check failed: %1%") % line);
}

void instance_t::value_directive(char * line)
{
  context.journal->value_expr = expr_t(line);
}

void instance_t::comment_directive(char * line)
{
  while (in.good() && ! in.eof()) {
    if (read_line(line) > 0) {
      std::string buf(line);
      if (starts_with(buf, "end comment") || starts_with(buf, "end test"))
        break;
    }
  }
}

#if HAVE_BOOST_PYTHON

void instance_t::import_directive(char * line)
{
  string module_name(line);
  trim(module_name);
  python_session->import_option(module_name);
}

void instance_t::python_directive(char * line)
{
  std::ostringstream script;

  if (line)
    script << skip_ws(line) << '\n';

  std::size_t indent = 0;

  while (peek_whitespace_line() || peek_blank_line()) {
    if (read_line(line) > 0) {
      if (! indent) {
        const char * p = line;
        while (*p && std::isspace(static_cast<unsigned char>(*p))) {
          ++indent;
          ++p;
        }
      }

      const char * p = line;
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

  if (! python_session->is_initialized)
    python_session->initialize();

  python_session->main_module->define_global
    ("journal", boost::python::object(boost::python::ptr(context.journal)));
  python_session->eval(script.str(), python_interpreter_t::PY_EVAL_MULTI);
}

#else

void instance_t::import_directive(char *)
{
  throw_(parse_error,
         _("'import' directive seen, but Python support is missing"));
}

void instance_t::python_directive(char *)
{
  throw_(parse_error,
         _("'python' directive seen, but Python support is missing"));
}

#endif // HAVE_BOOST_PYTHON

bool instance_t::general_directive(char * line)
{
  char buf[8192];

  std::strcpy(buf, line);

  char * p   = buf;
  char * arg = next_element(buf);

  if (*p == '@' || *p == '!')
    p++;

  // Ensure there's an argument for all directives that need one.
  if (! arg &&
      std::strcmp(p, "comment") != 0 && std::strcmp(p, "end") != 0
      && std::strcmp(p, "python") != 0 && std::strcmp(p, "test") != 0 &&
      *p != 'Y') {
    throw_(parse_error, _f("Directive '%1%' requires an argument") % p);
  }

  switch (*p) {
  case 'a':
    if (std::strcmp(p, "account") == 0) {
      account_directive(arg);
      return true;
    }
    else if (std::strcmp(p, "alias") == 0) {
      alias_directive(arg);
      return true;
    }
    else if (std::strcmp(p, "apply") == 0) {
      apply_directive(arg);
      return true;
    }
    else if (std::strcmp(p, "assert") == 0) {
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
    }
    else if (std::strcmp(p, "comment") == 0) {
      comment_directive(arg);
      return true;
    }
    else if (std::strcmp(p, "commodity") == 0) {
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
    }
    else if (std::strcmp(p, "expr") == 0 || std::strcmp(p, "eval") == 0) {
      eval_directive(arg);
      return true;
    }
    break;

  case 'i':
    if (std::strcmp(p, "include") == 0) {
      include_directive(arg);
      return true;
    }
    else if (std::strcmp(p, "import") == 0) {
      import_directive(arg);
      return true;
    }
    break;

  case 'p':
    if (std::strcmp(p, "payee") == 0) {
      payee_directive(arg);
      return true;
    }
    else if (std::strcmp(p, "python") == 0) {
      python_directive(arg);
      return true;
    }
    break;

  case 't':
    if (std::strcmp(p, "tag") == 0) {
      tag_directive(arg);
      return true;
    }
    else if (std::strcmp(p, "test") == 0) {
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

post_t * instance_t::parse_post(char *          line,
                                std::streamsize len,
                                account_t *     account,
                                xact_t *        xact,
                                bool            defer_expr)
{
  TRACE_START(post_details, 1, "Time spent parsing postings:");

  unique_ptr<post_t> post(new post_t);

  post->xact          = xact;   // this could be NULL
  post->pos           = position_t();
  post->pos->pathname = context.pathname;
  post->pos->beg_pos  = context.line_beg_pos;
  post->pos->beg_line = context.linenum;
  post->pos->sequence = context.sequence++;

  char buf[parse_context_t::MAX_LINE + 1];
  std::strcpy(buf, line);
  std::streamsize beg = 0;

  try {

  // Parse the state flag

  assert(line);
  assert(*line);

  char * p = skip_ws(line);

  switch (*p) {
  case '*':
    post->set_state(item_t::CLEARED);
    p = skip_ws(p + 1);
    DEBUG("textual.parse", "line " << context.linenum << ": "
          << "Parsed the CLEARED flag");
    break;

  case '!':
    post->set_state(item_t::PENDING);
    p = skip_ws(p + 1);
    DEBUG("textual.parse", "line " << context.linenum << ": "
          << "Parsed the PENDING flag");
    break;
  }

  if (xact &&
      (xact->_state != item_t::UNCLEARED && post->_state == item_t::UNCLEARED))
    post->set_state(xact->_state);

  // Parse the account name

  if (! *p || *p == ';')
    throw parse_error(_("Posting has no account"));

  char * next = next_element(p, true);
  char * e = p + std::strlen(p);

  while (e > p && std::isspace(static_cast<unsigned char>(*(e - 1))))
    e--;

  if ((*p == '[' && *(e - 1) == ']') || (*p == '(' && *(e - 1) == ')')) {
    post->add_flags(POST_VIRTUAL);
    DEBUG("textual.parse", "line " << context.linenum << ": "
          << "Parsed a virtual account name");

    if (*p == '[') {
      post->add_flags(POST_MUST_BALANCE);
      DEBUG("textual.parse", "line " << context.linenum << ": "
            << "Posting must balance");
    }
    p++; e--;
  }
  else if (*p == '<' && *(e - 1) == '>') {
    post->add_flags(POST_DEFERRED);
    DEBUG("textual.parse", "line " << context.linenum << ": "
          << "Parsed a deferred account name");
    p++; e--;
  }

  string name(p, static_cast<string::size_type>(e - p));
  DEBUG("textual.parse", "line " << context.linenum << ": "
        << "Parsed account name " << name);

  post->account =
    context.journal->register_account(name, post.get(), account);

  // Parse the optional amount

  if (next && *next && (*next != ';' && *next != '=')) {
    beg = static_cast<std::streamsize>(next - line);
    ptristream stream(next, static_cast<std::size_t>(len - beg));

    if (*next != '(')           // indicates a value expression
      post->amount.parse(stream, PARSE_NO_REDUCE);
    else
      parse_amount_expr(stream, *context.scope, *post.get(), post->amount,
                        PARSE_NO_REDUCE | PARSE_SINGLE | PARSE_NO_ASSIGN,
                        defer_expr, &post->amount_expr);

    DEBUG("textual.parse", "line " << context.linenum << ": "
          << "post amount = " << post->amount);

    if (! post->amount.is_null() && post->amount.has_commodity()) {
      context.journal->register_commodity(post->amount.commodity(), post.get());

      if (! post->amount.has_annotation()) {
        std::vector<fixed_rate_t> rates;
        get_applications<fixed_rate_t>(rates);
        foreach (fixed_rate_t& rate, rates) {
          if (*rate.first == post->amount.commodity()) {
            annotation_t details(rate.second);
            details.add_flags(ANNOTATION_PRICE_FIXATED);
            post->amount.annotate(details);
            DEBUG("textual.parse", "line " << context.linenum << ": "
                  << "applied rate = " << post->amount);
            break;
          }
        }
      }
    }

    if (stream.eof()) {
      next = NULL;
    } else {
      next = skip_ws(next + static_cast<std::ptrdiff_t>(stream.tellg()));

      // Parse the optional cost (@ PER-UNIT-COST, @@ TOTAL-COST)

      if (*next == '@' || (*next == '(' && *(next + 1) == '@')) {
        DEBUG("textual.parse", "line " << context.linenum << ": "
              << "Found a price indicator");

        if (*next == '(') {
          post->add_flags(POST_COST_VIRTUAL);
          ++next;
        }

        bool per_unit = true;
        if (*++next == '@') {
          per_unit = false;
          post->add_flags(POST_COST_IN_FULL);
          DEBUG("textual.parse", "line " << context.linenum << ": "
                << "And it's for a total price");
          next++;
        }

        if (post->has_flags(POST_COST_VIRTUAL) && *next == ')')
          ++next;

        p = skip_ws(next);
        if (*p) {
          post->cost = amount_t();

          bool fixed_cost = false;
          if (*p == '=') {
            p++;
            fixed_cost = true;
            if (*p == '\0')
              throw parse_error(_("Posting is missing a cost amount"));
          }

          beg = static_cast<std::streamsize>(p - line);
          ptristream cstream(p, static_cast<std::size_t>(len - beg));

          if (*p != '(')                // indicates a value expression
            post->cost->parse(cstream, PARSE_NO_MIGRATE);
          else
            parse_amount_expr(cstream, *context.scope, *post.get(), *post->cost,
                              PARSE_NO_MIGRATE | PARSE_SINGLE | PARSE_NO_ASSIGN);

          if (post->cost->sign() < 0)
            throw parse_error(_("A posting's cost may not be negative"));

          post->cost->in_place_unround();

          if (per_unit) {
            // For the sole case where the cost might be uncommoditized,
            // guarantee that the commodity of the cost after multiplication
            // is the same as it was before.
            commodity_t& cost_commodity(post->cost->commodity());
            *post->cost *= post->amount;
            post->cost->set_commodity(cost_commodity);
          }
          else if (post->amount.sign() < 0) {
            post->cost->in_place_negate();
          }

          if (fixed_cost)
            post->add_flags(POST_COST_FIXATED);

          post->given_cost = post->cost;

          DEBUG("textual.parse", "line " << context.linenum << ": "
                << "Total cost is " << *post->cost);
          DEBUG("textual.parse", "line " << context.linenum << ": "
                << "Annotated amount is " << post->amount);

          if (cstream.eof())
            next = NULL;
          else
            next = skip_ws(p + static_cast<std::ptrdiff_t>(cstream.tellg()));
        } else {
          throw parse_error(_("Expected a cost amount"));
        }
      }
    }
  }

  // Parse the optional balance assignment

  if (xact && next && *next == '=') {
    DEBUG("textual.parse", "line " << context.linenum << ": "
          << "Found a balance assignment indicator");

    beg = static_cast<std::streamsize>(++next - line);

    p = skip_ws(next);
    if (*p) {
      post->assigned_amount = amount_t();

      beg = static_cast<std::streamsize>(p - line);
      ptristream stream(p, static_cast<std::size_t>(len - beg));

      if (*p != '(')            // indicates a value expression
        post->assigned_amount->parse(stream);
      else
        parse_amount_expr(stream, *context.scope, *post.get(),
                          *post->assigned_amount,
                          PARSE_SINGLE | PARSE_NO_MIGRATE);

      if (post->assigned_amount->is_null()) {
        if (post->amount.is_null())
          throw parse_error(_("Balance assignment must evaluate to a constant"));
        else
          throw parse_error(_("Balance assertion must evaluate to a constant"));
      }

      DEBUG("textual.parse", "line " << context.linenum << ": "
            << "POST assign: parsed balance amount = " << *post->assigned_amount);

      const amount_t& amt(*post->assigned_amount);
      value_t account_total
        (post->account->amount(!post->has_flags(POST_VIRTUAL)).strip_annotations(keep_details_t()));

      DEBUG("post.assign", "line " << context.linenum << ": "
            << "account balance = " << account_total);
      DEBUG("post.assign", "line " << context.linenum << ": "
            << "post amount = " << amt << " (is_zero = " << amt.is_zero() << ")");

      balance_t diff = amt;

      switch (account_total.type()) {
      case value_t::AMOUNT: {
        amount_t amt(account_total.as_amount().strip_annotations(keep_details_t()));
        diff -= amt;
        DEBUG("textual.parse", "line " << context.linenum << ": "
              << "Subtracting amount " << amt << " from diff, yielding " << diff);
        break;
      }
      case value_t::BALANCE: {
        balance_t bal(account_total.as_balance().strip_annotations(keep_details_t()));
        diff -= bal;
        DEBUG("textual.parse", "line " << context.linenum << ": "
              << "Subtracting balance " << bal << " from diff, yielding " << diff);
        break;
      }
      default:
        break;
      }

      DEBUG("post.assign",
            "line " << context.linenum << ": " << "diff = " << diff);
      DEBUG("textual.parse", "line " << context.linenum << ": "
            << "POST assign: diff = " << diff);

      // Subtract amounts from previous posts to this account in the xact.
      for (post_t* p : xact->posts) {
        if (p->account == post->account && p->has_flags(POST_VIRTUAL) == post->has_flags(POST_VIRTUAL)) {
          amount_t amt(p->amount.strip_annotations(keep_details_t()));
          diff -= amt;
          DEBUG("textual.parse", "line " << context.linenum << ": "
                << "Subtracting " << amt << ", diff = " << diff);
        }
      }

      // If amt has a commodity, restrict balancing to that. Otherwise, it's the blanket '0' and
      // check that all of them are zero.
      if (amt.has_commodity()) {
        DEBUG("textual.parse", "line " << context.linenum << ": "
              << "Finding commodity " << amt.commodity() << " (" << amt << ") in balance " << diff);
        optional<amount_t> wanted_commodity = diff.commodity_amount(amt.commodity());
        if (!wanted_commodity) {
          diff = amt - amt;  // this is '0' with the correct commodity.
        } else {
          diff = *wanted_commodity;
        }
        DEBUG("textual.parse", "line " << context.linenum << ": "
              << "Diff is now " << diff);
      }

      if (post->amount.is_null()) {
        // balance assignment
        if (! diff.is_zero()) {
          // This will fail if there are more than 1 commodity in diff, which is wanted,
          // as amount cannot store more than 1 commodity.
          post->amount = diff.to_amount();
          DEBUG("textual.parse", "line " << context.linenum << ": "
                << "Overwrite null posting with " << diff.to_amount());
        } else {
          post->amount = amt - amt;  // this is '0' with the correct commodity.
          DEBUG("textual.parse", "line " << context.linenum << ": "
                << "Overwrite null posting with zero diff with " << amt - amt);
        }
      } else {
        // balance assertion
        diff -= post->amount.strip_annotations(keep_details_t());
        if (! no_assertions && ! diff.is_zero()) {
          balance_t tot = (-diff + amt).strip_annotations(keep_details_t());
          DEBUG("textual.parse", "Balance assertion: off by " << diff << " (expected to see " << tot << ")");
          throw_(parse_error,
                  _f("Balance assertion off by %1% (expected to see %2%)")
                  % diff.to_string() % tot.to_string());
        }
      }

      if (stream.eof())
        next = NULL;
      else
        next = skip_ws(p + static_cast<std::ptrdiff_t>(stream.tellg()));
    } else {
      throw parse_error(_("Expected an balance assignment/assertion amount"));
    }
  }

  // Parse the optional note

  if (next && *next == ';') {
    post->append_note(++next, *context.scope, true);
    next = line + len;
    DEBUG("textual.parse", "line " << context.linenum << ": "
          << "Parsed a posting note");
  }

  // There should be nothing more to read

  if (next && *next)
    throw_(parse_error,
           _f("Unexpected char '%1%' (Note: inline math requires parentheses)")
           % *next);

  post->pos->end_pos  = context.curr_pos;
  post->pos->end_line = context.linenum;

  std::vector<string> tags;
  get_applications<string>(tags);
  foreach (string& tag, tags)
    post->parse_tags(tag.c_str(), *context.scope, true);

  string post_payee = post->payee_from_tag();
  if (post_payee != "")
    post->set_payee(context.journal->validate_payee(post_payee));

  TRACE_STOP(post_details, 1);

  return post.release();

  }
  catch (const std::exception&) {
    add_error_context(_("While parsing posting:"));
    add_error_context(line_context(buf, static_cast<string::size_type>(beg),
                                   static_cast<string::size_type>(len)));
    throw;
  }
}

bool instance_t::parse_posts(account_t *  account,
                             xact_base_t& xact,
                             const bool   defer_expr)
{
  TRACE_START(xact_posts, 1, "Time spent parsing postings:");

  bool added = false;

  while (peek_whitespace_line()) {
    char * line;
    std::streamsize len = read_line(line);
    char * p = skip_ws(line);
    if (*p != ';') {
      if (post_t * post = parse_post(line, len, account, NULL, defer_expr)) {
        xact.add_post(post);
        added = true;
      }
    }
  }

  TRACE_STOP(xact_posts, 1);

  return added;
}

xact_t * instance_t::parse_xact(char *          line,
                                std::streamsize len,
                                account_t *     account,
                                xact_t *        previous_xact)
{
  TRACE_START(xact_text, 1, "Time spent parsing transaction text:");

  unique_ptr<xact_t> xact(new xact_t);

  xact->pos           = position_t();
  xact->pos->pathname = context.pathname;
  xact->pos->beg_pos  = context.line_beg_pos;
  xact->pos->beg_line = context.linenum;
  xact->pos->sequence = context.sequence++;

  bool reveal_context = true;

  try {

  // Parse the date

  char * next = next_element(line);

  if (char * p = std::strchr(line, '=')) {
    *p++ = '\0';
    xact->_date_aux = parse_date(p);
  }
  xact->_date = parse_date(line);

  // Parse the optional cleared flag: *

  if (next) {
    switch (*next) {
    case '*':
      xact->_state = item_t::CLEARED;
      next = skip_ws(++next);
      break;
    case '!':
      xact->_state = item_t::PENDING;
      next = skip_ws(++next);
      break;
    }
  }

  // Parse the optional code: (TEXT)

  if (next && *next == '(') {
    if (char * p = std::strchr(next++, ')')) {
      *p++ = '\0';
      xact->code = next;
      next = skip_ws(p);
    }
  }

  // Parse the description text

  if (next && *next) {
    char * p = next;
    std::size_t spaces = 0;
    std::size_t tabs = 0;
    while (*p) {
      if (*p == ' ') {
        ++spaces;
      }
      else if (*p == '\t') {
        ++tabs;
      }
      else if (*p == ';' && (tabs > 0 || spaces > 1)) {
        char *q = p - 1;
        while (q > next && std::isspace(static_cast<unsigned char>(*q)))
          --q;
        if (q >= next)
          *(q + 1) = '\0';
        break;
      }
      else {
        spaces = 0;
        tabs = 0;
      }
      ++p;
    }
    xact->payee = context.journal->validate_payee(next);
    next = p;
  } else {
    xact->payee = _("<Unspecified payee>");
  }

  // Parse the xact note

  if (next && *next == ';')
    xact->append_note(++next, *context.scope, false);

  TRACE_STOP(xact_text, 1);

  // Parse all of the posts associated with this xact

  TRACE_START(xact_details, 1, "Time spent parsing transaction details:");

  post_t * last_post = NULL;

  while (peek_whitespace_line()) {
    len = read_line(line);
    char * p = skip_ws(line);
    if (! *p)
      break;

    const std::size_t remlen = std::strlen(p);

    item_t * item;
    if (last_post)
      item = last_post;
    else
      item = xact.get();

    if (*p == ';') {
      // This is a trailing note, and possibly a metadata info tag
      item->append_note(p + 1, *context.scope, true);
      item->add_flags(ITEM_NOTE_ON_NEXT_LINE);
      item->pos->end_pos = context.curr_pos;
      item->pos->end_line++;
    }
    else if ((remlen > 7 && *p == 'a' &&
              std::strncmp(p, "assert", 6) == 0 &&
              std::isspace(static_cast<unsigned char>(p[6]))) ||
             (remlen > 6 && *p == 'c' &&
              std::strncmp(p, "check", 5) == 0 &&
              std::isspace(static_cast<unsigned char>(p[5]))) ||
             (remlen > 5 && *p == 'e' &&
              std::strncmp(p, "expr", 4) == 0 &&
              std::isspace(static_cast<unsigned char>(p[4])))) {
      const char c = *p;
      p = skip_ws(&p[*p == 'a' ? 6 : (*p == 'c' ? 5 : 4)]);
      expr_t expr(p);
      bind_scope_t bound_scope(*context.scope, *item);
      if (c == 'e') {
        expr.calc(bound_scope);
      }
      else if (! expr.calc(bound_scope).to_boolean()) {
        if (c == 'a') {
          throw_(parse_error, _f("Transaction assertion failed: %1%") % p);
        } else {
          context.warning(_f("Transaction check failed: %1%") % p);
        }
      }
    }
    else {
      reveal_context = false;

      if (!last_post) {
        if (xact->has_tag(_("UUID"))) {
          string uuid = xact->get_tag(_("UUID"))->to_string();
          foreach (payee_uuid_mapping_t value, context.journal->payee_uuid_mappings) {
            if (value.first.compare(uuid) == 0) {
              xact->payee = value.second;
            }
          }
        }
      }

      if (post_t * post =
          parse_post(p, len - (p - line), account, xact.get())) {
        reveal_context = true;
        xact->add_post(post);
        last_post = post;
      }
      reveal_context = true;
    }
  }

#if 0
  if (xact->_state == item_t::UNCLEARED) {
    item_t::application_t result = item_t::CLEARED;

    foreach (post_t * post, xact->posts) {
      if (post->_state == item_t::UNCLEARED) {
        result = item_t::UNCLEARED;
        break;
      }
      else if (post->_state == item_t::PENDING) {
        result = item_t::PENDING;
      }
    }
  }
#endif

  xact->pos->end_pos  = context.curr_pos;
  xact->pos->end_line = context.linenum;

  std::vector<string> tags;
  get_applications<string>(tags);
  foreach (string& tag, tags)
    xact->parse_tags(tag.c_str(), *context.scope, false);

  TRACE_STOP(xact_details, 1);

  if (hash_type != NO_HASHES) {
    string expected_hash =
      xact->hash(previous_xact &&
                 previous_xact->has_tag("Hash") ?
                 previous_xact->get_tag("Hash")->to_string() : "",
                 hash_type);
    if (xact->has_tag("Hash")) {
      string current_hash = xact->get_tag("Hash")->to_string();
      if (! std::equal(expected_hash.begin(),
                       expected_hash.begin() +
                       std::min(expected_hash.size(), current_hash.size()),
                       current_hash.begin()))
        throw_(parse_error, _f("Expected hash %1% != %2%") %
               expected_hash % current_hash);
    } else {
      xact->set_tag("Hash", string_value(expected_hash));
    }
  }

  return xact.release();

  }
  catch (const std::exception&) {
    if (reveal_context) {
      add_error_context(_("While parsing transaction:"));
      add_error_context(source_context(xact->pos->pathname,
                                       xact->pos->beg_pos,
                                       context.curr_pos, "> "));
    }
    throw;
  }
}

expr_t::ptr_op_t instance_t::lookup(const symbol_t::kind_t kind,
                                    const string& name)
{
  return context.scope->lookup(kind, name);
}

std::size_t journal_t::read_textual(parse_context_stack_t& context_stack,
                                    hash_type_t hash_type)
{
  TRACE_START(parsing_total, 1, "Total time spent parsing text:");
  {
    instance_t instance(context_stack, context_stack.get_current(), NULL,
                        checking_style == journal_t::CHECK_PERMISSIVE,
                        hash_type);
    instance.apply_stack.push_front
      (application_t("account", context_stack.get_current().master));
    instance.parse();
  }
  TRACE_STOP(parsing_total, 1);

  // Apply any deferred postings at this time
  master->apply_deferred_posts();

  // These tracers were started in textual.cc
  TRACE_FINISH(xact_text, 1);
  TRACE_FINISH(xact_details, 1);
  TRACE_FINISH(xact_posts, 1);
  TRACE_FINISH(xacts, 1);
  TRACE_FINISH(instance_parse, 1); // report per-instance timers
  TRACE_FINISH(parsing_total, 1);

  if (context_stack.get_current().errors > 0)
    throw error_count(context_stack.get_current().errors,
                      context_stack.get_current().last);

  return context_stack.get_current().count;
}

} // namespace ledger
