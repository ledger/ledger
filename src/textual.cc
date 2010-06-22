/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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
#include "xact.h"
#include "post.h"
#include "account.h"
#include "option.h"
#include "query.h"
#include "pstream.h"
#include "pool.h"
#include "session.h"

#define TIMELOG_SUPPORT 1
#if defined(TIMELOG_SUPPORT)
#include "timelog.h"
#endif

namespace ledger {

namespace {
  typedef std::pair<commodity_t *, amount_t>         fixed_rate_t;
  typedef variant<account_t *, string, fixed_rate_t> state_t;

  class parse_context_t : public noncopyable
  {
  public:
    journal_t&         journal;
    scope_t&           scope;
    std::list<state_t> state_stack;
#if defined(TIMELOG_SUPPORT)
    time_log_t         timelog;
#endif
    bool               strict;
    std::size_t        count;
    std::size_t        errors;
    std::size_t        sequence;

    parse_context_t(journal_t& _journal, scope_t& _scope)
      : journal(_journal), scope(_scope), timelog(journal, scope),
        strict(false), count(0), errors(0), sequence(1) {}

    bool front_is_account() {
      return state_stack.front().type() == typeid(account_t *);
    }
    bool front_is_string() {
      return state_stack.front().type() == typeid(string);
    }
    bool front_is_fixed_rate() {
      return state_stack.front().type() == typeid(fixed_rate_t);
    }

    account_t * top_account() {
      foreach (state_t& state, state_stack)
        if (state.type() == typeid(account_t *))
          return boost::get<account_t *>(state);
      return NULL;
    }
  };

  class instance_t : public noncopyable, public scope_t
  {
    static const std::size_t MAX_LINE = 1024;

  public:
    parse_context_t&     context;
    instance_t *         parent;
    accounts_map         account_aliases;
    const path *         original_file;
    path                 pathname;
    std::istream&        in;
    char                 linebuf[MAX_LINE + 1];
    std::size_t          linenum;
    istream_pos_type     line_beg_pos;
    istream_pos_type     curr_pos;
    optional<datetime_t> prev_epoch;

    instance_t(parse_context_t& _context,
               std::istream&    _in,
               const path *     _original_file = NULL,
               instance_t *     _parent        = NULL);

    ~instance_t();

    void parse();
    std::streamsize read_line(char *& line);
    bool peek_whitespace_line() {
      return (in.good() && ! in.eof() &&
              (in.peek() == ' ' || in.peek() == '\t'));
    }

    void read_next_directive(); 

#if defined(TIMELOG_SUPPORT)
    void clock_in_directive(char * line, bool capitalized);
    void clock_out_directive(char * line, bool capitalized);
#endif

    void default_commodity_directive(char * line);
    void default_account_directive(char * line);
    void price_conversion_directive(char * line);
    void price_xact_directive(char * line);
    void nomarket_directive(char * line);
    void year_directive(char * line);
    void option_directive(char * line);
    void automated_xact_directive(char * line);
    void period_xact_directive(char * line);
    void xact_directive(char * line, std::streamsize len);
    void include_directive(char * line);
    void master_account_directive(char * line);
    void end_directive(char * line);
    void alias_directive(char * line);
    void fixed_directive(char * line);
    void payee_mapping_directive(char * line);
    void account_mapping_directive(char * line);
    void tag_directive(char * line);
    void define_directive(char * line);
    void assert_directive(char * line);
    void check_directive(char * line);
    void expr_directive(char * line);
    bool general_directive(char * line);

    post_t * parse_post(char *          line,
                        std::streamsize len,
                        account_t *     account,
                        xact_t *        xact,
                        bool            defer_expr   = false);

    bool parse_posts(account_t *  account,
                     xact_base_t& xact,
                     const bool   defer_expr = false);

    xact_t * parse_xact(char *          line,
                        std::streamsize len,
                        account_t *     account);

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

#if defined(DEBUG_ENABLED)
    DEBUG_IF("textual.parse") {
      if (_debug_stream) {
        ledger::dump_value_expr(*_debug_stream, expr);
        *_debug_stream << std::endl;
      }
    }
#endif

    if (expr) {
      if (amount_expr)
        *amount_expr = expr;
      if (! defer_expr)
        amount = post.resolve_expr(scope, expr);
    }
  }
}

instance_t::instance_t(parse_context_t& _context,
                       std::istream&    _in,
                       const path *     _original_file,
                       instance_t *     _parent)
  : context(_context), parent(_parent), original_file(_original_file),
    pathname(original_file ? *original_file : "/dev/stdin"), in(_in)
{
  TRACE_CTOR(instance_t, "...");
  DEBUG("times.epoch", "Saving epoch " << epoch);
  prev_epoch = epoch;           // declared in times.h
}

instance_t::~instance_t()
{
  TRACE_DTOR(instance_t);
  epoch = prev_epoch;
  DEBUG("times.epoch", "Restored epoch to " << epoch);
}

void instance_t::parse()
{
  INFO("Parsing file '" << pathname.string() << "'");

  TRACE_START(instance_parse, 1,
              "Done parsing file '" << pathname.string() << "'");

  if (! in.good() || in.eof())
    return;

  linenum  = 0;
  curr_pos = in.tellg();

  while (in.good() && ! in.eof()) {
    try {
      read_next_directive();
    }
    catch (const std::exception& err) {
      string current_context = error_context();

      if (parent) {
        std::list<instance_t *> instances;

        for (instance_t * instance = parent;
             instance;
             instance = instance->parent)
          instances.push_front(instance);

        foreach (instance_t * instance, instances)
          add_error_context(_("In file included from %1")
                            << file_context(instance->pathname,
                                            instance->linenum));
      }
      add_error_context(_("While parsing file %1")
                        << file_context(pathname, linenum));

      if (caught_signal != NONE_CAUGHT)
        throw;

      string err_context = error_context();
      if (! err_context.empty())
        std::cerr << err_context << std::endl;
    
      if (! current_context.empty())
        std::cerr << current_context << std::endl;

      std::cerr << _("Error: ") << err.what() << std::endl;
      context.errors++;
    }
  }

  TRACE_STOP(instance_parse, 1);
}

std::streamsize instance_t::read_line(char *& line)
{
  assert(in.good());
  assert(! in.eof());           // no one should call us in that case

  line_beg_pos = curr_pos;

  check_for_signal();

  in.getline(linebuf, MAX_LINE);
  std::streamsize len = in.gcount();

  if (len > 0) {
    if (linenum == 0 && utf8::is_bom(linebuf))
      line = &linebuf[3];
    else
      line = linebuf;

    if (line[len - 1] == '\r')  // strip Windows CRLF down to LF
      line[--len] = '\0';

    linenum++;

    curr_pos  = line_beg_pos;
    curr_pos += len;

    return len - 1;             // LF is being silently dropped
  }
  return 0;
}

void instance_t::read_next_directive()
{
  char * line;
  std::streamsize len = read_line(line);

  if (len == 0 || line == NULL)
    return;

  switch (line[0]) {
  case '\0':
    assert(false);              // shouldn't ever reach here
    break;

  case ' ':
  case '\t': {
    break;
  }

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
    xact_directive(line, len);
    break;
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
#if defined(TIMELOG_SUPPORT)
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
        default_account_directive(line);
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
        year_directive(line);
        break;
      }
    }
    break;
  }
}

#if defined(TIMELOG_SUPPORT)

void instance_t::clock_in_directive(char * line, bool /*capitalized*/)
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
  position.pathname = pathname;
  position.beg_pos  = line_beg_pos;
  position.beg_line = linenum;
  position.end_pos  = curr_pos;
  position.end_line = linenum;
  position.sequence = context.sequence++;

  time_xact_t event(position, parse_datetime(datetime),
                    p ? context.top_account()->find_account(p) : NULL,
                    n ? n : "",
                    end ? end : "");

  context.timelog.clock_in(event);
}

void instance_t::clock_out_directive(char * line, bool /*capitalized*/)
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
  position.pathname = pathname;
  position.beg_pos  = line_beg_pos;
  position.beg_line = linenum;
  position.end_pos  = curr_pos;
  position.end_line = linenum;
  position.sequence = context.sequence++;

  time_xact_t event(position, parse_datetime(datetime),
                    p ? context.top_account()->find_account(p) : NULL,
                    n ? n : "",
                    end ? end : "");

  context.timelog.clock_out(event);
  context.count++;
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
  context.journal.bucket = context.top_account()->find_account(skip_ws(line + 1));
  context.journal.bucket->add_flags(ACCOUNT_KNOWN);
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

void instance_t::year_directive(char * line)
{
  unsigned short year(lexical_cast<unsigned short>(skip_ws(line + 1)));
  DEBUG("times.epoch", "Setting current year to " << year);
  // This must be set to the last day of the year, otherwise partial
  // dates like "11/01" will refer to last year's november, not the
  // current year.
  epoch = datetime_t(date_t(year, 12, 31));
}

void instance_t::option_directive(char * line)
{
  char * p = next_element(line);
  if (! p) {
    p = std::strchr(line, '=');
    if (p)
      *p++ = '\0';
  }

  if (! process_option(pathname.string(), line + 2, context.scope, p, line))
    throw_(option_error, _("Illegal option --%1") << line + 2);
}

void instance_t::automated_xact_directive(char * line)
{
  istream_pos_type pos= line_beg_pos;

  bool reveal_context = true;

  try {
    query_t          query;
    keep_details_t   keeper(true, true, true);
    expr_t::ptr_op_t expr = 
      query.parse_args(string_value(skip_ws(line + 1)).to_sequence(),
                       keeper, false, true);

    std::auto_ptr<auto_xact_t> ae(new auto_xact_t(predicate_t(expr, keeper)));
    ae->pos           = position_t();
    ae->pos->pathname = pathname;
    ae->pos->beg_pos  = line_beg_pos;
    ae->pos->beg_line = linenum;
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
        item->append_note(p + 1, context.scope, true);
        item->pos->end_pos = curr_pos;
        item->pos->end_line++;

        // If there was no last_post yet, then deferred notes get applied to
        // the matched posting.  Other notes get applied to the auto-generated
        // posting.
        ae->deferred_notes->back().apply_to_post = last_post;
      }
      else if ((remlen > 7 && *p == 'a' &&
                std::strncmp(p, "assert", 6) == 0 && std::isspace(p[6])) ||
               (remlen > 6 && *p == 'c' &&
                std::strncmp(p, "check", 5) == 0 && std::isspace(p[5])) ||
               (remlen > 5 && *p == 'e' &&
                std::strncmp(p, "expr", 4) == 0 && std::isspace(p[4]))) {
        const char c = *p;
        p = skip_ws(&p[*p == 'a' ? 6 : (*p == 'c' ? 5 : 4)]);
        if (! ae->check_exprs)
          ae->check_exprs = auto_xact_t::check_expr_list();
        ae->check_exprs->push_back
          (auto_xact_t::check_expr_pair(expr_t(p),
                                        c == 'a' ?
                                        auto_xact_t::EXPR_ASSERTION :
                                        (c == 'c' ?
                                         auto_xact_t::EXPR_CHECK :
                                         auto_xact_t::EXPR_GENERAL)));
      }
      else {
        reveal_context = false;

        if (post_t * post =
            parse_post(p, len - (p - line), context.top_account(),
                       NULL, true)) {
          reveal_context = true;
          ae->add_post(post);
          last_post = post;
        }
        reveal_context = true;
      }
    }

    context.journal.auto_xacts.push_back(ae.get());

    ae->journal       = &context.journal;
    ae->pos->end_pos  = curr_pos;
    ae->pos->end_line = linenum;

    ae.release();
  }
  catch (const std::exception&) {
    if (reveal_context) {
      add_error_context(_("While parsing automated transaction:"));
      add_error_context(source_context(pathname, pos, curr_pos, "> "));
    }
    throw;
  }
}

void instance_t::period_xact_directive(char * line)
{
  istream_pos_type pos = line_beg_pos;

  bool reveal_context = true;

  try {

  std::auto_ptr<period_xact_t> pe(new period_xact_t(skip_ws(line + 1)));
  pe->pos           = position_t();
  pe->pos->pathname = pathname;
  pe->pos->beg_pos  = line_beg_pos;
  pe->pos->beg_line = linenum;
  pe->pos->sequence = context.sequence++;

  reveal_context = false;

  if (parse_posts(context.top_account(), *pe.get())) {
    reveal_context = true;
    pe->journal = &context.journal;

    if (pe->finalize()) {
      context.journal.extend_xact(pe.get());
      context.journal.period_xacts.push_back(pe.get());

      pe->pos->end_pos  = curr_pos;
      pe->pos->end_line = linenum;

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
      add_error_context(source_context(pathname, pos, curr_pos, "> "));
    }
    throw;
  }
}

void instance_t::xact_directive(char * line, std::streamsize len)
{
  TRACE_START(xacts, 1, "Time spent handling transactions:");

  if (xact_t * xact = parse_xact(line, len, context.top_account())) {
    std::auto_ptr<xact_t> manager(xact);

    if (context.journal.add_xact(xact)) {
      manager.release();        // it's owned by the journal now
      context.count++;
    }
    // It's perfectly valid for the journal to reject the xact, which it
    // will do if the xact has no substantive effect (for example, a
    // checking xact, all of whose postings have null amounts).
  } else {
    throw parse_error(_("Failed to parse transaction"));
  }

  TRACE_STOP(xacts, 1);
}

void instance_t::include_directive(char * line)
{
  path filename;

  DEBUG("textual.include", "include: " << line);

  if (line[0] != '/' && line[0] != '\\' && line[0] != '~') {
    DEBUG("textual.include", "received a relative path");
    DEBUG("textual.include", "parent file path: " << pathname.string());
    string::size_type pos = pathname.string().rfind('/');
    if (pos == string::npos)
      pos = pathname.string().rfind('\\');
    if (pos != string::npos) {
      filename = path(string(pathname.string(), 0, pos + 1)) / line;
      DEBUG("textual.include", "normalized path: " << filename.string());
    } else {
      filename = path(string(".")) / line;
    }
  } else {
    filename = line;
  }

  filename = resolve_path(filename);
  DEBUG("textual.include", "resolved path: " << filename.string());

  mask_t glob;
#if BOOST_VERSION >= 103700
  path   parent_path = filename.parent_path();
  glob.assign_glob('^' + filename.filename() + '$');
#else // BOOST_VERSION >= 103700
  path   parent_path = filename.branch_path();
  glob.assign_glob('^' + filename.leaf() + '$');
#endif // BOOST_VERSION >= 103700

  bool files_found = false;
  if (exists(parent_path)) {
    filesystem::directory_iterator end;
    for (filesystem::directory_iterator iter(parent_path);
         iter != end;
         ++iter) {
#if BOOST_VERSION <= 103500
      if (is_regular(*iter))
#else
      if (is_regular_file(*iter))
#endif
        {
#if BOOST_VERSION >= 103700
        string base = (*iter).filename();
#else // BOOST_VERSION >= 103700
        string base = (*iter).leaf();
#endif // BOOST_VERSION >= 103700
        if (glob.match(base)) {
          path inner_file(*iter);
          ifstream stream(inner_file);
          instance_t instance(context, stream, &inner_file, this);
          instance.parse();
          files_found = true;
        }
      }
    }
  }

  if (! files_found)
    throw_(std::runtime_error,
           _("File to include was not found: '%1'") << filename);

}

void instance_t::master_account_directive(char * line)
{
  if (account_t * acct = context.top_account()->find_account(line))
    context.state_stack.push_front(acct);
#if !defined(NO_ASSERTS)
  else
    assert(! "Failed to create account");
#endif
}

void instance_t::end_directive(char * kind)
{
  string name(kind ? kind : "");

  if ((name.empty() || name == "account") && ! context.front_is_account())
    throw_(std::runtime_error,
           _("'end account' directive does not match open directive"));
  else if (name == "tag" && ! context.front_is_string())
    throw_(std::runtime_error,
           _("'end tag' directive does not match open directive"));
  else if (name == "fixed" && ! context.front_is_fixed_rate())
    throw_(std::runtime_error,
           _("'end fixed' directive does not match open directive"));

  if (context.state_stack.size() <= 1)
    throw_(std::runtime_error,
           _("'end' found, but no enclosing tag or account directive"));
  else
    context.state_stack.pop_front();
}

void instance_t::alias_directive(char * line)
{
  char * b = skip_ws(line);
  if (char * e = std::strchr(b, '=')) {
    char * z = e - 1;
    while (std::isspace(*z))
      *z-- = '\0';
    *e++ = '\0';
    e = skip_ws(e);

    // Once we have an alias name (b) and the target account
    // name (e), add a reference to the account in the
    // `account_aliases' map, which is used by the post
    // parser to resolve alias references.
    account_t * acct = context.top_account()->find_account(e);
    std::pair<accounts_map::iterator, bool> result
      = account_aliases.insert(accounts_map::value_type(b, acct));
    assert(result.second);
  }
}

void instance_t::fixed_directive(char * line)
{
  if (optional<std::pair<commodity_t *, price_point_t> > price_point = 
      commodity_pool_t::current_pool->parse_price_directive(trim_ws(line),
                                                            true)) {
    context.state_stack.push_front(fixed_rate_t(price_point->first,
                                                price_point->second.price));
  } else {
    throw_(std::runtime_error, _("Error in fixed directive"));
  }
}

void instance_t::payee_mapping_directive(char * line)
{
  char * payee = skip_ws(line);
  char * regex = next_element(payee, true);

  if (regex)
    context.journal.payee_mappings.push_back
      (payee_mapping_t(mask_t(regex), payee));

  while (peek_whitespace_line()) {
#if defined(NO_ASSERTS)
    read_line(line);
#else
    std::streamsize len = read_line(line);
    assert(len > 0);
#endif

    regex = skip_ws(line);
    if (! *regex)
      break;

    context.journal.payee_mappings.push_back
      (payee_mapping_t(mask_t(regex), payee));
  }
}

void instance_t::account_mapping_directive(char * line)
{
  char * account_name = skip_ws(line);
  char * payee_regex  = next_element(account_name, true);

  if (payee_regex)
    context.journal.account_mappings.push_back
      (account_mapping_t(mask_t(payee_regex),
                         context.top_account()->find_account(account_name)));

  while (peek_whitespace_line()) {
#if defined(NO_ASSERTS)
    read_line(line);
#else
    std::streamsize len = read_line(line);
    assert(len > 0);
#endif

    payee_regex = skip_ws(line);
    if (! *payee_regex)
      break;

    context.journal.account_mappings.push_back
      (account_mapping_t(mask_t(payee_regex),
                         context.top_account()->find_account(account_name)));
  }
}

void instance_t::tag_directive(char * line)
{
  string tag(trim_ws(line));

  if (tag.find(':') == string::npos)
    tag = string(":") + tag + ":";

  context.state_stack.push_front(tag);
}

void instance_t::define_directive(char * line)
{
  expr_t def(skip_ws(line));
  def.compile(context.scope);   // causes definitions to be established
}

void instance_t::assert_directive(char * line)
{
  expr_t expr(line);
  if (! expr.calc(context.scope).to_boolean())
    throw_(parse_error, _("Assertion failed: %1") << line);
}

void instance_t::check_directive(char * line)
{
  expr_t expr(line);
  if (! expr.calc(context.scope).to_boolean())
    warning_(_("Check failed: %1") << line);
}

void instance_t::expr_directive(char * line)
{
  expr_t expr(line);
  expr.calc(context.scope);
}

bool instance_t::general_directive(char * line)
{
  char buf[8192];

  std::strcpy(buf, line);

  char * p   = buf;
  char * arg = next_element(buf);

  if (*p == '@' || *p == '!')
    p++;

  switch (*p) {
  case 'a':
    if (std::strcmp(p, "account") == 0) {
      master_account_directive(arg);
      return true;
    }
    else if (std::strcmp(p, "alias") == 0) {
      alias_directive(arg);
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
    if (std::strcmp(p, "capture") == 0) {
      account_mapping_directive(arg);
      return true;
    }
    else if (std::strcmp(p, "check") == 0) {
      check_directive(arg);
      return true;
    }
    break;

  case 'd':
    if (std::strcmp(p, "def") == 0 || std::strcmp(p, "define") == 0) {
      define_directive(arg);
      return true;
    }
    break;

  case 'e':
    if (std::strcmp(p, "end") == 0) {
      end_directive(arg);
      return true;
    }
    else if (std::strcmp(p, "expr") == 0) {
      expr_directive(arg);
      return true;
    }
    break;

  case 'f':
    if (std::strcmp(p, "fixed") == 0) {
      fixed_directive(arg);
      return true;
    }
    break;

  case 'i':
    if (std::strcmp(p, "include") == 0) {
      include_directive(arg);
      return true;
    }
    break;

  case 'p':
    if (std::strcmp(p, "payee") == 0) {
      payee_mapping_directive(arg);
      return true;
    }
    break;

  case 't':
    if (std::strcmp(p, "tag") == 0) {
      tag_directive(arg);
      return true;
    }
    break;

  case 'y':
    if (std::strcmp(p, "year") == 0) {
      year_directive(arg);
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

  std::auto_ptr<post_t> post(new post_t);

  post->xact          = xact;   // this could be NULL
  post->pos           = position_t();
  post->pos->pathname = pathname;
  post->pos->beg_pos  = line_beg_pos;
  post->pos->beg_line = linenum;
  post->pos->sequence = context.sequence++;

  char buf[MAX_LINE + 1];
  std::strcpy(buf, line);
  std::size_t beg = 0;

  try {

  // Parse the state flag

  assert(line);
  assert(*line);

  char * p = skip_ws(line);

  switch (*p) {
  case '*':
    post->set_state(item_t::CLEARED);
    p = skip_ws(p + 1);
    DEBUG("textual.parse", "line " << linenum << ": "
          << "Parsed the CLEARED flag");
    break;

  case '!':
    post->set_state(item_t::PENDING);
    p = skip_ws(p + 1);
    DEBUG("textual.parse", "line " << linenum << ": "
          << "Parsed the PENDING flag");
    break;
  }

  if (xact &&
      ((xact->_state == item_t::CLEARED && post->_state != item_t::CLEARED) ||
       (xact->_state == item_t::PENDING && post->_state == item_t::UNCLEARED)))
    post->set_state(xact->_state);

  // Parse the account name

  if (! *p)
    throw parse_error(_("Posting has no account"));

  char * next = next_element(p, true);
  char * e = p + std::strlen(p);

  while (e > p && std::isspace(*(e - 1)))
    e--;

  if ((*p == '[' && *(e - 1) == ']') || (*p == '(' && *(e - 1) == ')')) {
    post->add_flags(POST_VIRTUAL);
    DEBUG("textual.parse", "line " << linenum << ": "
          << "Parsed a virtual account name");

    if (*p == '[') {
      post->add_flags(POST_MUST_BALANCE);
      DEBUG("textual.parse", "line " << linenum << ": "
            << "Posting must balance");
    }
    p++; e--;
  }

  string name(p, e - p);
  DEBUG("textual.parse", "line " << linenum << ": "
        << "Parsed account name " << name);

  if (account_aliases.size() > 0) {
    accounts_map::const_iterator i = account_aliases.find(name);
    if (i != account_aliases.end())
      post->account = (*i).second;
  }
  if (! post->account)
    post->account = account->find_account(name);

  if (context.strict && ! post->account->has_flags(ACCOUNT_KNOWN)) {
    if (post->_state == item_t::UNCLEARED)
      warning_(_("\"%1\", line %2: Unknown account '%3'")
               << pathname << linenum << post->account->fullname());
    post->account->add_flags(ACCOUNT_KNOWN);
  }

  if (post->account->name == _("Unknown")) {
    foreach (account_mapping_t& value, context.journal.account_mappings) {
      if (value.first.match(xact->payee)) {
        post->account = value.second;
        break;
      }
    }
  }

  // Parse the optional amount

  bool saw_amount = false;

  if (next && *next && (*next != ';' && *next != '=')) {
    saw_amount = true;

    beg = next - line;
    ptristream stream(next, len - beg);

    if (*next != '(')           // indicates a value expression
      post->amount.parse(stream, PARSE_NO_REDUCE);
    else
      parse_amount_expr(stream, context.scope, *post.get(), post->amount,
                        PARSE_NO_REDUCE | PARSE_SINGLE | PARSE_NO_ASSIGN,
                        defer_expr, &post->amount_expr);

    if (! post->amount.is_null() && post->amount.has_commodity()) {
      if (context.strict &&
          ! post->amount.commodity().has_flags(COMMODITY_KNOWN)) {
        if (post->_state == item_t::UNCLEARED)
          warning_(_("\"%1\", line %2: Unknown commodity '%3'")
                   << pathname << linenum << post->amount.commodity());
        post->amount.commodity().add_flags(COMMODITY_KNOWN);
      }

      if (! post->amount.has_annotation()) {
        foreach (state_t& state, context.state_stack) {
          if (state.type() == typeid(fixed_rate_t)) {
            fixed_rate_t& rate(boost::get<fixed_rate_t>(state));
            if (*rate.first == post->amount.commodity()) {
              annotation_t details(rate.second);
              details.add_flags(ANNOTATION_PRICE_FIXATED);
              post->amount.annotate(details);
              break;
            }
          }
        }
      }
    }

    DEBUG("textual.parse", "line " << linenum << ": "
          << "post amount = " << post->amount);

    if (stream.eof()) {
      next = NULL;
    } else {
      next = skip_ws(next + static_cast<std::ptrdiff_t>(stream.tellg()));

      // Parse the optional cost (@ PER-UNIT-COST, @@ TOTAL-COST)

      if (*next == '@') {
        DEBUG("textual.parse", "line " << linenum << ": "
              << "Found a price indicator");

        bool per_unit = true;

        if (*++next == '@') {
          per_unit = false;
          post->add_flags(POST_COST_IN_FULL);
          DEBUG("textual.parse", "line " << linenum << ": "
                << "And it's for a total price");
        }

        beg = ++next - line;

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

          beg = p - line;
          ptristream cstream(p, len - beg);

          if (*p != '(')                // indicates a value expression
            post->cost->parse(cstream, PARSE_NO_MIGRATE);
          else
            parse_amount_expr(cstream, context.scope, *post.get(), *post->cost,
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

          DEBUG("textual.parse", "line " << linenum << ": "
                << "Total cost is " << *post->cost);
          DEBUG("textual.parse", "line " << linenum << ": "
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
    DEBUG("textual.parse", "line " << linenum << ": "
          << "Found a balance assignment indicator");

    beg = ++next - line;

    p = skip_ws(next);
    if (*p) {
      post->assigned_amount = amount_t();

      beg = p - line;
      ptristream stream(p, len - beg);

      if (*p != '(')            // indicates a value expression
        post->assigned_amount->parse(stream, PARSE_NO_MIGRATE);
      else
        parse_amount_expr(stream, context.scope, *post.get(),
                          *post->assigned_amount,
                          PARSE_SINGLE | PARSE_NO_MIGRATE);

      if (post->assigned_amount->is_null()) {
        if (post->amount.is_null())
          throw parse_error(_("Balance assignment must evaluate to a constant"));
        else
          throw parse_error(_("Balance assertion must evaluate to a constant"));
      }

      DEBUG("textual.parse", "line " << linenum << ": "
            << "POST assign: parsed amt = " << *post->assigned_amount);

      amount_t& amt(*post->assigned_amount);
      value_t account_total
        (post->account->amount(false).strip_annotations(keep_details_t()));

      DEBUG("post.assign",
            "line " << linenum << ": " "account balance = " << account_total);
      DEBUG("post.assign",
            "line " << linenum << ": " "post amount = " << amt);

      amount_t diff = amt;

      switch (account_total.type()) {
      case value_t::AMOUNT:
        diff -= account_total.as_amount();
        break;

      case value_t::BALANCE:
        if (optional<amount_t> comm_bal =
            account_total.as_balance().commodity_amount(amt.commodity()))
          diff -= *comm_bal;
        break;

      default:
        break;
      }

      DEBUG("post.assign",
            "line " << linenum << ": " << "diff = " << diff);
      DEBUG("textual.parse",
            "line " << linenum << ": " << "POST assign: diff = " << diff);

      if (! diff.is_zero()) {
        if (! post->amount.is_null()) {
          diff -= post->amount;
          if (! diff.is_zero())
            throw_(parse_error, _("Balance assertion off by %1") << diff);
        } else {
          post->amount = diff;
          DEBUG("textual.parse", "line " << linenum << ": "
                << "Overwrite null posting");
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
    post->append_note(++next, context.scope, true);
    next = line + len;
    DEBUG("textual.parse", "line " << linenum << ": "
          << "Parsed a posting note");
  }

  // There should be nothing more to read

  if (next && *next)
    throw_(parse_error,
           _("Unexpected char '%1' (Note: inline math requires parentheses)")
           << *next);

  post->pos->end_pos  = curr_pos;
  post->pos->end_line = linenum;

  if (! context.state_stack.empty()) {
    foreach (const state_t& state, context.state_stack)
      if (state.type() == typeid(string))
        post->parse_tags(boost::get<string>(state).c_str(), context.scope, true);
  }

  TRACE_STOP(post_details, 1);

  return post.release();

  }
  catch (const std::exception&) {
    add_error_context(_("While parsing posting:"));
    add_error_context(line_context(buf, beg, len));
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
    assert(len > 0);

    if (post_t * post = parse_post(line, len, account, NULL, defer_expr)) {
      xact.add_post(post);
      added = true;
    }
  }

  TRACE_STOP(xact_posts, 1);

  return added;
}

xact_t * instance_t::parse_xact(char *          line,
                                std::streamsize len,
                                account_t *     account)
{
  TRACE_START(xact_text, 1, "Time spent parsing transaction text:");

  std::auto_ptr<xact_t> xact(new xact_t);

  xact->pos           = position_t();
  xact->pos->pathname = pathname;
  xact->pos->beg_pos  = line_beg_pos;
  xact->pos->beg_line = linenum;
  xact->pos->sequence = context.sequence++;

  bool reveal_context = true;

  try {

  // Parse the date

  char * next = next_element(line);

  if (char * p = std::strchr(line, '=')) {
    *p++ = '\0';
    xact->_date_eff = parse_date(p);
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
    char * p = next_element(next, true);
    foreach (payee_mapping_t& value, context.journal.payee_mappings) {
      if (value.first.match(next)) {
        xact->payee = value.second;
        break;
      }
    }
    if (xact->payee.empty())
      xact->payee = next;
    next = p;
  } else {
    xact->payee = _("<Unspecified payee>");
  }

  // Parse the xact note

  if (next && *next == ';')
    xact->append_note(++next, context.scope, false);

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
      item->append_note(p + 1, context.scope, true);
      item->pos->end_pos = curr_pos;
      item->pos->end_line++;
    }
    else if ((remlen > 7 && *p == 'a' &&
              std::strncmp(p, "assert", 6) == 0 && std::isspace(p[6])) ||
             (remlen > 6 && *p == 'c' &&
              std::strncmp(p, "check", 5) == 0 && std::isspace(p[5])) ||
             (remlen > 5 && *p == 'e' &&
              std::strncmp(p, "expr", 4) == 0 && std::isspace(p[4]))) {
      const char c = *p;
      p = skip_ws(&p[*p == 'a' ? 6 : (*p == 'c' ? 5 : 4)]);
      expr_t expr(p);
      bind_scope_t bound_scope(context.scope, *item);
      if (c == 'e') {
        expr.calc(bound_scope);
      }
      else if (! expr.calc(bound_scope).to_boolean()) {
        if (c == 'a') {
          throw_(parse_error, _("Transaction assertion failed: %1") << p);
        } else {
          warning_(_("Transaction check failed: %1") << p);
        }
      }
    }
    else {
      reveal_context = false;

      if (post_t * post =
          parse_post(p, len - (p - line), account, xact.get())) {
        reveal_context = true;
        xact->add_post(post);
        last_post = post;
      }
      reveal_context = true;
    }
  }

  if (xact->_state == item_t::UNCLEARED) {
    item_t::state_t result = item_t::CLEARED;

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

  xact->pos->end_pos  = curr_pos;
  xact->pos->end_line = linenum;

  if (! context.state_stack.empty()) {
    foreach (const state_t& state, context.state_stack)
      if (state.type() == typeid(string))
        xact->parse_tags(boost::get<string>(state).c_str(), context.scope,
                         false);
  }

  TRACE_STOP(xact_details, 1);

  return xact.release();

  }
  catch (const std::exception&) {
    if (reveal_context) {
      add_error_context(_("While parsing transaction:"));
      add_error_context(source_context(xact->pos->pathname,
                                       xact->pos->beg_pos, curr_pos, "> "));
    }
    throw;
  }
}

expr_t::ptr_op_t instance_t::lookup(const symbol_t::kind_t kind,
                                    const string& name)
{
  return context.scope.lookup(kind, name);
}

std::size_t journal_t::parse(std::istream& in,
                             scope_t&      scope,
                             account_t *   master,
                             const path *  original_file,
                             bool          strict)
{
  TRACE_START(parsing_total, 1, "Total time spent parsing text:");

  parse_context_t context(*this, scope);
  context.strict = strict;
  if (master || this->master)
    context.state_stack.push_front(master ? master : this->master);

  instance_t instance(context, in, original_file);
  instance.parse();

  TRACE_STOP(parsing_total, 1);

  // These tracers were started in textual.cc
  TRACE_FINISH(xact_text, 1);
  TRACE_FINISH(xact_details, 1);
  TRACE_FINISH(xact_posts, 1);
  TRACE_FINISH(xacts, 1);
  TRACE_FINISH(instance_parse, 1); // report per-instance timers
  TRACE_FINISH(parsing_total, 1);

  if (context.errors > 0)
    throw static_cast<int>(context.errors);

  return context.count;
}

} // namespace ledger
