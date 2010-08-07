#include <system.hh>

#include "t_expr.h"

#include "expr.h"
#include "predicate.h"
#include "query.h"
#include "op.h"

using namespace ledger;

CPPUNIT_TEST_SUITE_NAMED_REGISTRATION(ValueExprTestCase, "expr");

void ValueExprTestCase::setUp()
{
  times_initialize();
  amount_t::initialize();
}

void ValueExprTestCase::tearDown()
{
  amount_t::shutdown();
  times_shutdown();
}

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

void ValueExprTestCase::testPredicateTokenizer1()
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer2()
{
  value_t args;
  args.push_back(string_value("foo and bar"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end(), false);

  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer3()
{
  value_t args;
  args.push_back(string_value("(foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar)"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::LPAREN, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::RPAREN, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer4()
{
  value_t args;
  args.push_back(string_value("("));
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));
  args.push_back(string_value(")"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::LPAREN, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::RPAREN, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer5()
{
  value_t args;
  args.push_back(string_value("( foo and"));
  args.push_back(string_value("bar)"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end(), false);

  assertEqual(query_t::lexer_t::token_t::LPAREN, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::RPAREN, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer6()
{
  value_t args;
  args.push_back(string_value("=foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::TOK_EQ, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer7()
{
  value_t args;
  args.push_back(string_value("=foo and bar"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::TOK_EQ, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer8()
{
  value_t args;
  args.push_back(string_value("expr 'foo and bar'"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end(), false);

  assertEqual(query_t::lexer_t::token_t::TOK_EXPR, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer9()
{
  value_t args;
  args.push_back(string_value("expr"));
  args.push_back(string_value("'foo and bar'"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::TOK_EXPR, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer10()
{
  value_t args;
  args.push_back(string_value("expr"));
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::TOK_EXPR, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer11()
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));
  args.push_back(string_value("or"));
  args.push_back(string_value("baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer12()
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));
  args.push_back(string_value("|"));
  args.push_back(string_value("baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer13()
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar"));
  args.push_back(string_value("|baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer14()
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar|"));
  args.push_back(string_value("baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer15()
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and"));
  args.push_back(string_value("bar|baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end());

  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}

void ValueExprTestCase::testPredicateTokenizer16()
{
  value_t args;
  args.push_back(string_value("foo"));
  args.push_back(string_value("and bar|baz"));

#ifndef NOT_FOR_PYTHON
  query_t::lexer_t tokens(args.begin(), args.end(), false);

  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_AND,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TOK_OR,  tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::TERM, tokens.next_token().kind);
  assertEqual(query_t::lexer_t::token_t::END_REACHED, tokens.next_token().kind);
#endif
}
