#define BOOST_TEST_DYN_LINK
//#define BOOST_TEST_MODULE expr
#include <boost/test/unit_test.hpp>

#include <system.hh>

#include "expr.h"
#include "predicate.h"
#include "query.h"
#include "op.h"

using namespace ledger;

struct expr_fixture {
  expr_fixture() {
    times_initialize();
    amount_t::initialize();
  }

  ~expr_fixture() {
    amount_t::shutdown();
    times_shutdown();
  }
};

// 1.  foo and bar
// 2.  'foo and bar'
// 3.  (foo and bar)
// 4.  ( foo and bar )
// 5.  '( foo and' bar)
// 6.  =foo and bar
// 7.  ='foo and bar'
// 8.  'expr foo and bar'
// 9.  expr 'foo and bar'
// 10. expr foo and bar
// 11. foo and bar or baz
// 12. foo and bar | baz
// 13. foo and bar |baz
// 14. foo and bar| baz
// 15. foo and bar|baz
// 16. foo 'and bar|baz'

BOOST_FIXTURE_TEST_SUITE(expr, expr_fixture)

BOOST_AUTO_TEST_CASE(testPredicateTokenizer1)
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer2)
{
  value_t args;
  args.push_back(string_value("foo and bar"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end(), false);

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer3)
{
  value_t args;
  args.push_back(string_value("(foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar)"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::LPAREN, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::RPAREN, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer4)
{
  value_t args;
  args.push_back(string_value("("));
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));
  args.push_back(string_value(")"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::LPAREN, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::RPAREN, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer5)
{
  value_t args;
  args.push_back(string_value("( foo and"));
  args.push_back(string_value("bar)"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end(), false);

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::LPAREN, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::RPAREN, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer6)
{
  value_t args;
  args.push_back(string_value("=foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_NOTE, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer7)
{
  value_t args;
  args.push_back(string_value("=foo and bar"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_NOTE, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer8)
{
  value_t args;
  args.push_back(string_value("expr 'foo and bar'"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end(), false);

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_EXPR, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer9)
{
  value_t args;
  args.push_back(string_value("expr"));
  args.push_back(string_value("'foo and bar'"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_EXPR, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer10)
{
  value_t args;
  args.push_back(string_value("expr"));
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_EXPR, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer11)
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));
  args.push_back(string_value("or"));
  args.push_back(string_value("baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer12)
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));
  args.push_back(string_value("|"));
  args.push_back(string_value("baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer13)
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));
  args.push_back(string_value("|baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer14)
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar|"));
  args.push_back(string_value("baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer15)
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar|baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_CASE(testPredicateTokenizer16)
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and bar|baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end(), false);

  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  BOOST_CHECK_EQUAL(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

BOOST_AUTO_TEST_SUITE_END()
