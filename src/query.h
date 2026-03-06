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
 * @addtogroup expr
 */

/**
 * @file   query.h
 * @author John Wiegley
 * @brief  Command-line query parsing: lexer, parser, and query_t facade.
 *
 * @ingroup expr
 *
 * This file implements the translation from user-facing command-line
 * arguments into expression-tree predicates that the report engine can
 * evaluate against every posting, transaction, or account.
 *
 * When a user types:
 * @code
 *   ledger bal food and @grocery
 * @endcode
 * the arguments `["food", "and", "@grocery"]` are handed to query_t, which
 * lexes them into tokens (TERM "food", TOK_AND, TOK_PAYEE, TERM "grocery"),
 * then parses those tokens into an expression tree equivalent to:
 * @code
 *   (account =~ /food/) & (payee =~ /grocery/)
 * @endcode
 *
 * The query language supports several categories of output:
 * - **Limit** (the default): filters which postings are included.
 * - **show**: controls display-time filtering (`--display`).
 * - **only**: further narrows results (`--only`).
 * - **bold**: highlights matching entries.
 * - **for** / **since** / **until**: constrain the date range (`--period`).
 *
 * Each category is stored in a query_map_t so the report can retrieve the
 * appropriate predicate string for `--limit`, `--display`, etc.
 *
 * Shorthand operators in the query language:
 * - Bare terms match account names: `food` becomes `account =~ /food/`
 * - `@term` matches payees: `@grocery` becomes `payee =~ /grocery/`
 * - `#term` matches transaction codes: `#1234` becomes `code =~ /1234/`
 * - `=term` matches notes: `=vacation` becomes `note =~ /vacation/`
 * - `%term` matches metadata tags: `%receipt` becomes `has_tag(/receipt/)`
 * - `expr <expression>` passes a raw expression through unchanged
 *
 * Boolean connectives `and`/`&`, `or`/`|`, `not`/`!` and parenthesized
 * grouping are supported with standard precedence (not > and > or).
 */
#pragma once

#include "predicate.h"

namespace ledger {

/**
 * @brief Parses command-line query arguments into expression predicates.
 *
 * query_t is the top-level facade that owns a parser_t (which in turn owns
 * a lexer_t).  Construction or calling parse_args() drives the full
 * lex-parse pipeline.  The resulting predicates are retrievable via
 * get_query() keyed by kind_t (QUERY_LIMIT, QUERY_SHOW, etc.).
 *
 * @see predicate_t for the boolean wrapper applied to each parsed expression.
 */
class query_t {
protected:
  class parser_t;

public:
  /**
   * @brief Lexer that tokenizes command-line query arguments.
   *
   * The lexer iterates over a sequence of string arguments (the argv-like
   * value_t sequence) and produces token_t values.  It handles:
   * - Quoted patterns (`'food'`, `"food"`, `/food/`)
   * - Single-character operators (`@`, `#`, `%`, `=`, `&`, `|`, `!`)
   * - Keyword recognition (`and`, `or`, `not`, `payee`, `code`, `expr`, etc.)
   * - Multi-argument consumption (the `expr` keyword consumes the entire
   *   next argument as a raw expression string)
   *
   * A one-token lookahead cache (token_cache) supports peek_token() and
   * push_token() for the recursive-descent parser.
   */
  class lexer_t {
    friend class query_t;
    friend class parser_t;

    value_t::sequence_t::const_iterator begin; ///< Current position in the argument sequence.
    value_t::sequence_t::const_iterator end;   ///< One past the last argument.

    string::const_iterator prev_arg_i; ///< Start of the previously consumed argument (for rewind).
    string::const_iterator arg_i;      ///< Current character position within the current argument.
    string::const_iterator arg_end;    ///< End of the current argument string.

    bool consume_whitespace; ///< When true, whitespace is included in identifiers (inside expr
                             ///< parens).
    bool consume_next_arg;   ///< When true, the entire next argument is consumed as a single TERM.
    bool multiple_args;      ///< Whether the query spans multiple argv arguments.

  public:
    /**
     * @brief A single token produced by the query lexer.
     *
     * Tokens represent the atomic units of the query grammar: operators,
     * keywords, parentheses, and literal terms (account name patterns,
     * payee patterns, etc.).
     */
    struct token_t {
      /**
       * @brief Enumerates all token types in the query language.
       */
      enum kind_t : uint8_t {
        UNKNOWN, ///< Uninitialized / sentinel value.

        LPAREN, ///< `(` -- opens a grouped sub-expression.
        RPAREN, ///< `)` -- closes a grouped sub-expression.

        TOK_NOT, ///< `not` or `!` -- logical negation.
        TOK_AND, ///< `and` or `&` -- logical conjunction.
        TOK_OR,  ///< `or` or `|` -- logical disjunction.
        TOK_EQ,  ///< `=` when used as a metadata value comparator (e.g., `%tag=value`).

        TOK_CODE,    ///< `code` or `#` -- match transaction code field.
        TOK_PAYEE,   ///< `payee`, `desc`, or `@` -- match payee/description field.
        TOK_NOTE,    ///< `note` or leading `=` -- match note/comment field.
        TOK_ACCOUNT, ///< `account` -- match account name (the default context).
        TOK_META,    ///< `meta`, `tag`, `data`, or `%` -- match metadata tags.
        TOK_EXPR,    ///< `expr` -- pass-through a raw expression string.

        TOK_SHOW,  ///< `show` -- begins the display predicate section.
        TOK_ONLY,  ///< `only` -- begins the secondary filter section.
        TOK_BOLD,  ///< `bold` -- begins the bold/highlight section.
        TOK_FOR,   ///< `for` -- begins a date-range period specification.
        TOK_SINCE, ///< `since` -- specifies the start of a date range.
        TOK_UNTIL, ///< `until` -- specifies the end of a date range.

        TERM, ///< A literal term (pattern string); value holds the text.

        END_REACHED ///< No more tokens available.

      } kind;

      optional<string> value; ///< The literal text for TERM tokens; none for operators/keywords.

      explicit token_t(kind_t _kind = UNKNOWN, const optional<string>& _value = none)
          : kind(_kind), value(_value) {
        TRACE_CTOR(query_t::lexer_t::token_t, "");
      }
      token_t(const token_t& tok) : kind(tok.kind), value(tok.value) {
        TRACE_CTOR(query_t::lexer_t::token_t, "copy");
      }
      ~token_t() noexcept { TRACE_DTOR(query_t::lexer_t::token_t); }

      token_t& operator=(const token_t& tok) {
        if (this != &tok) {
          kind = tok.kind;
          value = tok.value;
        }
        return *this;
      }

      operator bool() const { return kind != END_REACHED; }

      /// @brief Return the debug-friendly name of this token (e.g., "TOK_AND", "TERM(food)").
      string to_string() const {
        switch (kind) {
        case UNKNOWN:
          return "UNKNOWN";
        case LPAREN:
          return "LPAREN";
        case RPAREN:
          return "RPAREN";
        case TOK_NOT:
          return "TOK_NOT";
        case TOK_AND:
          return "TOK_AND";
        case TOK_OR:
          return "TOK_OR";
        case TOK_EQ:
          return "TOK_EQ";
        case TOK_CODE:
          return "TOK_CODE";
        case TOK_PAYEE:
          return "TOK_PAYEE";
        case TOK_NOTE:
          return "TOK_NOTE";
        case TOK_ACCOUNT:
          return "TOK_ACCOUNT";
        case TOK_META:
          return "TOK_META";
        case TOK_EXPR:
          return "TOK_EXPR";
        case TOK_SHOW:
          return "TOK_SHOW";
        case TOK_ONLY:
          return "TOK_ONLY";
        case TOK_BOLD:
          return "TOK_BOLD";
        case TOK_FOR:
          return "TOK_FOR";
        case TOK_SINCE:
          return "TOK_SINCE";
        case TOK_UNTIL:
          return "TOK_UNTIL";
        case TERM:
          return string("TERM(") + *value + ")";
        case END_REACHED:
          return "END_REACHED";
        default:
          assert(false && "Unknown token kind");
          return "<UNKNOWN>";
        }
      }

      /// @brief Return the user-facing symbol for this token (e.g., "and", "@", "(").
      string symbol() const {
        switch (kind) {
        case LPAREN:
          return "(";
        case RPAREN:
          return ")";
        case TOK_NOT:
          return "not";
        case TOK_AND:
          return "and";
        case TOK_OR:
          return "or";
        case TOK_EQ:
          return "=";
        case TOK_CODE:
          return "code";
        case TOK_PAYEE:
          return "payee";
        case TOK_NOTE:
          return "note";
        case TOK_ACCOUNT:
          return "account";
        case TOK_META:
          return "meta";
        case TOK_EXPR:
          return "expr";
        case TOK_SHOW:
          return "show";
        case TOK_ONLY:
          return "only";
        case TOK_BOLD:
          return "bold";
        case TOK_FOR:
          return "for";
        case TOK_SINCE:
          return "since";
        case TOK_UNTIL:
          return "until";

        case END_REACHED:
          return "<EOF>";

        case TERM:
          assert(false);
          return "<TERM>";

        case UNKNOWN:
          assert(false);
          return "<UNKNOWN>";
        }
#if !defined(__clang__)
        return "<ERROR>";
#endif
      }

      /// @brief Throw a parse_error indicating that character @p wanted was expected.
      void expected(char wanted);
    };

    token_t token_cache; ///< One-token lookahead cache for peek_token()/push_token().

    lexer_t(const value_t::sequence_t::const_iterator& _begin,
            const value_t::sequence_t::const_iterator& _end, bool _multiple_args = true)
        : begin(_begin), end(_end), consume_whitespace(false), consume_next_arg(false),
          multiple_args(_multiple_args) {
      assert(begin != end);
      arg_i = (*begin).as_string().begin();
      arg_end = (*begin).as_string().end();

      TRACE_CTOR(query_t::lexer_t, "");
    }
    lexer_t(const lexer_t& lexer)
        : begin(lexer.begin), end(lexer.end), arg_i(lexer.arg_i), arg_end(lexer.arg_end),
          consume_whitespace(lexer.consume_whitespace), consume_next_arg(lexer.consume_next_arg),
          multiple_args(lexer.multiple_args), token_cache(lexer.token_cache) {
      TRACE_CTOR(query_t::lexer_t, "copy");
    }
    lexer_t& operator=(const lexer_t&) = default;
    ~lexer_t() noexcept { TRACE_DTOR(query_t::lexer_t); }

    /**
     * @brief Consume and return the next token from the argument stream.
     * @param tok_context The current parsing context (e.g., TOK_ACCOUNT, TOK_EXPR),
     *        which affects how certain characters are interpreted.
     * @return The next token, or END_REACHED if the argument stream is exhausted.
     */
    token_t next_token(token_t::kind_t tok_context = token_t::UNKNOWN);

    /// @brief Push a token back into the one-element lookahead cache.
    void push_token(const token_t& tok) {
      assert(token_cache.kind == token_t::UNKNOWN);
      token_cache = tok;
    }
    /// @brief Peek at the next token without consuming it (populates the cache).
    token_t peek_token(token_t::kind_t tok_context = token_t::UNKNOWN) {
      if (token_cache.kind == token_t::UNKNOWN)
        token_cache = next_token(tok_context);
      return token_cache;
    }

    /// @brief Check whether @p str has unbalanced parentheses.
    bool unbalanced_braces(const string& str);
  };

  /**
   * @brief Identifies which section of the query a predicate belongs to.
   *
   * A single command line may contain multiple sections separated by
   * keywords like `show`, `only`, `bold`, and `for`.  Each section
   * produces a separate predicate stored in the query_map_t.
   */
  enum kind_t : uint8_t {
    QUERY_LIMIT, ///< The primary filter (bare terms, `--limit`).
    QUERY_SHOW,  ///< Display-time filter (`show`, `--display`).
    QUERY_ONLY,  ///< Secondary narrowing filter (`only`, `--only`).
    QUERY_BOLD,  ///< Highlight matching items (`bold`).
    QUERY_FOR    ///< Date range constraint (`for`/`since`/`until`, `--period`).
  };

  /// @brief Maps each query section (QUERY_LIMIT, etc.) to its predicate string.
  using query_map_t = std::map<kind_t, string>;

protected:
  /**
   * @brief Recursive-descent parser that builds expression trees from query tokens.
   *
   * The parser implements a standard precedence hierarchy:
   *   - parse_query_expr: top-level, handles section keywords (show/only/bold/for)
   *   - parse_or_expr:    `or` / `|`  (lowest precedence among boolean ops)
   *   - parse_and_expr:   `and` / `&`
   *   - parse_unary_expr: `not` / `!`
   *   - parse_query_term: atoms -- literal patterns, field prefixes, parenthesized groups
   *
   * The tok_context parameter threads through the entire parse, tracking
   * which field the current terms should match against.  It starts as
   * TOK_ACCOUNT (bare terms match account names) and changes when the
   * user writes a field prefix like `@` (payee) or `#` (code).
   *
   * Results are stored in query_map keyed by kind_t so the report can
   * retrieve each section's predicate independently.
   */
  class parser_t {
    friend class query_t;

    value_t args;  ///< The original argument sequence (owns the strings the lexer iterates).
    lexer_t lexer; ///< Tokenizer over the argument sequence.
    keep_details_t what_to_keep; ///< Annotation-stripping policy passed to constructed predicates.
    query_map_t query_map;       ///< Accumulated predicates keyed by query section.

    /// @brief Parse a single atomic query term (pattern, field prefix, parens, or expr).
    expr_t::ptr_op_t parse_query_term(lexer_t::token_t::kind_t tok_context);
    /// @brief Parse a unary expression (handles `not`/`!` prefix).
    expr_t::ptr_op_t parse_unary_expr(lexer_t::token_t::kind_t tok_context);
    /// @brief Parse an AND expression (left-associative chain of unary expressions).
    expr_t::ptr_op_t parse_and_expr(lexer_t::token_t::kind_t tok_context);
    /// @brief Parse an OR expression (left-associative chain of AND expressions).
    expr_t::ptr_op_t parse_or_expr(lexer_t::token_t::kind_t tok_context);
    /**
     * @brief Parse a complete query expression, including section keywords.
     * @param tok_context The default field context (typically TOK_ACCOUNT).
     * @param subexpression If true, parsing a parenthesized sub-expression
     *        (section keywords like `show`/`for` are not recognized).
     */
    expr_t::ptr_op_t parse_query_expr(lexer_t::token_t::kind_t tok_context,
                                      bool subexpression = false);

  public:
    parser_t(const value_t& _args, const keep_details_t& _what_to_keep = keep_details_t(),
             bool multiple_args = true)
        : args(_args), lexer(args.begin(), args.end(), multiple_args), what_to_keep(_what_to_keep) {
      TRACE_CTOR(query_t::parser_t, "value_t, keep_details_t, bool");
    }
    parser_t(const parser_t& other) : args(other.args), lexer(other.lexer) {
      TRACE_CTOR(query_t::parser_t, "copy");
    }
    ~parser_t() noexcept { TRACE_DTOR(query_t::parser_t); }

    /// @brief Entry point: parse all arguments, defaulting to account-name context.
    expr_t::ptr_op_t parse(bool subexpression = false) {
      return parse_query_expr(lexer_t::token_t::TOK_ACCOUNT, subexpression);
    }

    /// @brief Check whether any unparsed tokens remain in the lexer stream.
    bool tokens_remaining() {
      lexer_t::token_t tok = lexer.peek_token();
      assert(tok.kind != lexer_t::token_t::UNKNOWN);
      return tok.kind != lexer_t::token_t::END_REACHED;
    }
  };

  optional<parser_t> parser; ///< Lazily constructed parser (created on first parse_args call).
  query_map_t predicates;    ///< Cached predicate strings by section.

public:
  query_t() { TRACE_CTOR(query_t, ""); }
  query_t(const query_t& other) : parser(other.parser), predicates(other.predicates) {
    TRACE_CTOR(query_t, "copy");
  }

  /// @brief Construct from a single query string (e.g., "food and @grocery").
  query_t(const string& arg, const keep_details_t& what_to_keep = keep_details_t(),
          bool multiple_args = true) {
    if (!arg.empty()) {
      value_t temp(string_value(arg));
      parse_args(temp.to_sequence(), what_to_keep, multiple_args);
    }
    TRACE_CTOR(query_t, "string, keep_details_t, bool");
  }
  /// @brief Construct from an argument sequence (e.g., from command-line argv).
  query_t(const value_t& args, const keep_details_t& what_to_keep = keep_details_t(),
          bool multiple_args = true) {
    if (!args.empty())
      parse_args(args, what_to_keep, multiple_args);

    TRACE_CTOR(query_t, "value_t, keep_details_t, bool");
  }
  virtual ~query_t() { TRACE_DTOR(query_t); }

  /**
   * @brief Parse query arguments into expression trees, populating query_map.
   * @param args The sequence of string arguments to parse.
   * @param what_to_keep Annotation-stripping policy for constructed predicates.
   * @param multiple_args Whether args contains multiple separate arguments (true)
   *        or a single string with spaces (false).
   * @param subexpression If true, parse as a parenthesized sub-expression.
   * @return The expression tree for the QUERY_LIMIT section, or nullptr if empty.
   */
  expr_t::ptr_op_t parse_args(const value_t& args,
                              const keep_details_t& what_to_keep = keep_details_t(),
                              bool multiple_args = true, bool subexpression = false) {
    if (!parser)
      parser = parser_t(args, what_to_keep, multiple_args);
    return parser->parse(subexpression);
  }

  /// @brief Check whether a predicate exists for the given query section.
  bool has_query(const kind_t& id) const {
    return parser && parser->query_map.find(id) != parser->query_map.end();
  }
  /// @brief Retrieve the predicate string for a query section, or empty_string if absent.
  string get_query(const kind_t& id) const {
    if (parser) {
      query_map_t::const_iterator i = parser->query_map.find(id);
      if (i != parser->query_map.end())
        return (*i).second;
    }
    return empty_string;
  }

  /// @brief Check whether unparsed tokens remain (useful for incremental parsing).
  bool tokens_remaining() { return parser && parser->tokens_remaining(); }
};

} // namespace ledger
