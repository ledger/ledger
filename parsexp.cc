/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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

#include "parsexp.h"
#include "parser.h"

namespace ledger {
namespace expr {

void parser_t::token_t::parse_ident(std::istream& in)
{
  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  assert(in.good());

  char c = peek_next_nonws(in);

  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  assert(in.good());

  kind	 = IDENT;
  length = 0;

  char buf[256];
  READ_INTO_(in, buf, 255, c, length,
	     std::isalnum(c) || c == '_' || c == '.' || c == '-');

  switch (buf[0]) {
  case 'a':
    if (std::strcmp(buf, "and") == 0)
      kind = KW_AND;
    break;
  case 'd':
    if (std::strcmp(buf, "div") == 0)
      kind = KW_DIV;
    break;
  case 'e':
    if (std::strcmp(buf, "eq") == 0)
      kind = EQUAL;
    break;
  case 'f':
    if (std::strcmp(buf, "false") == 0) {
      kind = VALUE;
      value = false;
    }
    break;
  case 'g':
    if (std::strcmp(buf, "gt") == 0)
      kind = GREATER;
    else if (std::strcmp(buf, "ge") == 0)
      kind = GREATEREQ;
    break;
  case 'i':
    if (std::strcmp(buf, "is") == 0)
      kind = EQUAL;
    break;
  case 'l':
    if (std::strcmp(buf, "lt") == 0)
      kind = LESS;
    else if (std::strcmp(buf, "le") == 0)
      kind = LESSEQ;
    break;
  case 'm':
    if (std::strcmp(buf, "mod") == 0)
      kind = KW_MOD;
    break;
  case 'n':
    if (std::strcmp(buf, "ne") == 0)
      kind = NEQUAL;
    break;
  case 'o':
    if (std::strcmp(buf, "or") == 0)
      kind = KW_OR;
    break;
  case 't':
    if (std::strcmp(buf, "true") == 0) {
      kind = VALUE;
      value = true;
    }
    break;
  }

  if (kind == IDENT)
    value.set_string(buf);
}

void parser_t::token_t::next(std::istream& in, const flags_t flags)
{
  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  assert(in.good());

  char c = peek_next_nonws(in);

  if (in.eof()) {
    kind = TOK_EOF;
    return;
  }
  assert(in.good());

  symbol[0] = c;
  symbol[1] = '\0';

  length = 1;

  if (! (flags & EXPR_PARSE_RELAXED) &&
      (std::isalpha(c) || c == '_')) {
    parse_ident(in);
    return;
  }

  switch (c) {
  case '@':
    in.get(c);
    kind = AT_SYM;
    break;

  case '&':
    in.get(c);
    kind = KW_AND;
    break;

  case '(':
    in.get(c);
    kind = LPAREN;
    break;
  case ')':
    in.get(c);
    kind = RPAREN;
    break;

  case '[': {
    in.get(c);
    if (! (flags & EXPR_PARSE_NO_DATES)) {
      char buf[256];
      READ_INTO_(in, buf, 255, c, length, c != ']');
      if (c != ']')
	unexpected(c, ']');
      in.get(c);
      length++;
      interval_t timespan(buf);
      kind = VALUE;
      value = timespan.first();
    } else {
      kind = LBRACKET;
    }
    break;
  }

  case ']': {
    in.get(c);
    kind = RBRACKET;
    break;
  }


  case '\'':
  case '"': {
    char delim;
    in.get(delim);
    char buf[4096];
    READ_INTO_(in, buf, 4095, c, length, c != delim);
    if (c != delim)
      unexpected(c, delim);
    in.get(c);
    length++;
    kind = VALUE;
    value.set_string(buf);
    break;
  }

  case '{': {
    in.get(c);
    amount_t temp;
    temp.parse(in, AMOUNT_PARSE_NO_MIGRATE);
    in.get(c);
    if (c != '}')
      unexpected(c, '}');
    length++;
    kind = VALUE;
    value = temp;
    break;
  }

  case '!':
    in.get(c);
    c = in.peek();
    if (c == '=') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = NEQUAL;
      length = 2;
      break;
    }
    kind = EXCLAM;
    break;

  case '-':
    in.get(c);
    kind = MINUS;
    break;
  case '+':
    in.get(c);
    kind = PLUS;
    break;

  case '*':
    in.get(c);
    kind = STAR;
    break;

  case 'c':
  case 'C':
  case 'p':
  case 'w':
  case 'W':
  case 'e':
  case '/': {
    bool code_mask	    = c == 'c';
    bool commodity_mask	    = c == 'C';
    bool payee_mask	    = c == 'p';
    bool note_mask	    = c == 'e';
    bool short_account_mask = c == 'w';

    in.get(c);
    if (c == '/') {
      c = peek_next_nonws(in);
      if (c == '/') {
	in.get(c);
	c = in.peek();
	if (c == '/') {
	  in.get(c);
	  c = in.peek();
	  short_account_mask = true;
	} else {
	  payee_mask = true;
	}
      }
    } else {
      in.get(c);
    }

    // Read in the regexp
    char buf[256];
    READ_INTO_(in, buf, 255, c, length, c != '/');
    if (c != '/')
      unexpected(c, '/');
    in.get(c);
    length++;

    if (short_account_mask)
      kind = SHORT_ACCOUNT_MASK;
    else if (code_mask)
      kind = CODE_MASK;
    else if (commodity_mask)
      kind = COMMODITY_MASK;
    else if (payee_mask)
      kind = PAYEE_MASK;
    else if (note_mask)
      kind = NOTE_MASK;
    else
      kind = ACCOUNT_MASK;

    value.set_string(buf);
    break;
  }

  case '=':
    in.get(c);
    kind = EQUAL;
    break;

  case '<':
    in.get(c);
    if (in.peek() == '=') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = LESSEQ;
      length = 2;
      break;
    }
    kind = LESS;
    break;

  case '>':
    in.get(c);
    if (in.peek() == '=') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = GREATEREQ;
      length = 2;
      break;
    }
    kind = GREATER;
    break;

  case ',':
    in.get(c);
    kind = COMMA;
    break;

  case '.':
    in.get(c);
    c = in.peek();
    if (c == '.') {
      in.get(c);
      length++;
      kind = DOTDOT;
      break;
    }
    else if (! std::isdigit(c)) {
      kind = DOT;
      break;
    }
    in.unget();			// put the first '.' back
    // fall through...

  default:
    if (! (flags & EXPR_PARSE_RELAXED)) {
      kind = UNKNOWN;
    } else {
      amount_t temp;
      unsigned long pos = 0;

      // When in relaxed parsing mode, we want to migrate commodity
      // flags so that any precision specified by the user updates the
      // current maximum displayed precision.
      pos = (long)in.tellg();

      unsigned char parse_flags = 0;
      if (flags & EXPR_PARSE_NO_MIGRATE)
	parse_flags |= AMOUNT_PARSE_NO_MIGRATE;
      if (flags & EXPR_PARSE_NO_REDUCE)
	parse_flags |= AMOUNT_PARSE_NO_REDUCE;

      if (! temp.parse(in, parse_flags | AMOUNT_PARSE_SOFT_FAIL)) {
	// If the amount had no commodity, it must be an unambiguous
	// variable reference

	in.clear();
	in.seekg(pos, std::ios::beg);

	c = in.peek();
	assert(! (std::isdigit(c) || c == '.'));
	parse_ident(in);
      } else {
	kind = VALUE;
	value = temp;
      }
    }
    break;
  }
}

void parser_t::token_t::rewind(std::istream& in)
{
  for (unsigned int i = 0; i < length; i++)
    in.unget();
}


void parser_t::token_t::unexpected()
{
  switch (kind) {
  case TOK_EOF:
    throw_(parse_error, "Unexpected end of expression");
  case IDENT:
    throw_(parse_error, "Unexpected symbol '" << value << "'");
  case VALUE:
    throw_(parse_error, "Unexpected value '" << value << "'");
  default:
    throw_(parse_error, "Unexpected operator '" << symbol << "'");
  }
}

void parser_t::token_t::unexpected(char c, char wanted)
{
  if ((unsigned char) c == 0xff) {
    if (wanted)
      throw_(parse_error, "Missing '" << wanted << "'");
    else
      throw_(parse_error, "Unexpected end");
  } else {
    if (wanted)
      throw_(parse_error, "Invalid char '" << c <<
	     "' (wanted '" << wanted << "')");
    else
      throw_(parse_error, "Invalid char '" << c << "'");
  }
}

ptr_op_t
parser_t::parse_value_term(std::istream& in, scope_t& scope, const flags_t tflags) const
{
  ptr_op_t node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::VALUE:
    node = new op_t(op_t::VALUE);
    node->set_value(tok.value);
    break;

  case token_t::SHORT_ACCOUNT_MASK:
    node = new op_t(op_t::F_SHORT_ACCOUNT_MASK);
    node->set_mask(tok.value.as_string());
    break;
  case token_t::CODE_MASK:
    node = new op_t(op_t::F_CODE_MASK);
    node->set_mask(tok.value.as_string());
    break;
  case token_t::COMMODITY_MASK:
    node = new op_t(op_t::F_COMMODITY_MASK);
    node->set_mask(tok.value.as_string());
    break;
  case token_t::PAYEE_MASK:
    node = new op_t(op_t::F_PAYEE_MASK);
    node->set_mask(tok.value.as_string());
    break;
  case token_t::NOTE_MASK:
    node = new op_t(op_t::F_NOTE_MASK);
    node->set_mask(tok.value.as_string());
    break;
  case token_t::ACCOUNT_MASK:
    node = new op_t(op_t::F_ACCOUNT_MASK);
    node->set_mask(tok.value.as_string());
    break;

  case token_t::IDENT: {
#if 0
#ifdef USE_BOOST_PYTHON
    if (tok.value->as_string() == "lambda") // special
      try {
	char c, buf[4096];

	std::strcpy(buf, "lambda ");
	READ_INTO(in, &buf[7], 4000, c, true);

	ptr_op_t eval = new op_t(op_t::O_EVAL);
	ptr_op_t lambda = new op_t(op_t::FUNCTION);
	lambda->functor = new python_functor_t(python_eval(buf));
	eval->set_left(lambda);
	ptr_op_t sym = new op_t(op_t::SYMBOL);
	sym->name = new string("__ptr");
	eval->set_right(sym);

	node = eval;

	goto done;
      }
      catch(const boost::python::error_already_set&) {
	throw_(parse_error, "Error parsing lambda expression");
      }
#endif /* USE_BOOST_PYTHON */
#endif

    string ident = tok.value.as_string();

    // An identifier followed by ( represents a function call
    tok = next_token(in, tflags);

    if (tok.kind == token_t::LPAREN) {
#if 0
      node = new op_t(op_t::FUNC_NAME);
      node->set_string(ident);

      ptr_op_t call_node(new op_t(op_t::O_CALL));
      call_node->set_left(node);
      call_node->set_right(parse_value_expr(in, scope,
					    tflags | EXPR_PARSE_PARTIAL));

      tok = next_token(in, tflags);
      if (tok.kind != token_t::RPAREN)
	tok.unexpected(0xff, ')');

      node = call_node;
#endif
    } else {
      if (std::isdigit(ident[0])) {
	node = new op_t(op_t::O_ARG);
	node->set_long(lexical_cast<unsigned int>(ident.c_str()));
      } else {
	node = new op_t(op_t::O_LOOKUP);
	node->set_string(ident);
      }
      push_token(tok);
    }
    break;
  }

  case token_t::LPAREN:
    node = new op_t(op_t::O_COMMA);
    node->set_left(parse_value_expr(in, scope, tflags | EXPR_PARSE_PARTIAL));
    if (! node->left())
      throw_(parse_error, tok.symbol << " operator not followed by argument");

    tok = next_token(in, tflags);
    if (tok.kind != token_t::RPAREN)
      tok.unexpected(0xff, ')');
    break;

  default:
    push_token(tok);
    break;
  }

#if 0
#ifdef USE_BOOST_PYTHON
 done:
#endif
#endif
  return node;
}

ptr_op_t
parser_t::parse_unary_expr(std::istream& in, scope_t& scope,
			   const flags_t tflags) const
{
  ptr_op_t node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::EXCLAM: {
    ptr_op_t term(parse_value_term(in, scope, tflags));
    if (! term)
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");

    // A very quick optimization
    if (term->kind == op_t::VALUE) {
      term->as_value_lval().in_place_negate();
      node = term;
    } else {
      node = new op_t(op_t::O_NOT);
      node->set_left(term);
    }
    break;
  }

  case token_t::MINUS: {
    ptr_op_t term(parse_value_term(in, scope, tflags));
    if (! term)
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");

    // A very quick optimization
    if (term->kind == op_t::VALUE) {
      term->as_value_lval().in_place_negate();
      node = term;
    } else {
      node = new op_t(op_t::O_NEG);
      node->set_left(term);
    }
    break;
  }

  default:
    push_token(tok);
    node = parse_value_term(in, scope, tflags);
    break;
  }

  return node;
}

ptr_op_t
parser_t::parse_mul_expr(std::istream& in, scope_t& scope, const flags_t tflags) const
{
  ptr_op_t node(parse_unary_expr(in, scope, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::STAR || tok.kind == token_t::KW_DIV) {
      ptr_op_t prev(node);
      node = new op_t(tok.kind == token_t::STAR ?
		      op_t::O_MUL : op_t::O_DIV);
      node->set_left(prev);
      node->set_right(parse_mul_expr(in, scope, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");

      tok = next_token(in, tflags);
    }
    push_token(tok);
  }

  return node;
}

ptr_op_t
parser_t::parse_add_expr(std::istream& in, scope_t& scope, const flags_t tflags) const
{
  ptr_op_t node(parse_mul_expr(in, scope, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::PLUS ||
	tok.kind == token_t::MINUS) {
      ptr_op_t prev(node);
      node = new op_t(tok.kind == token_t::PLUS ?
		      op_t::O_ADD : op_t::O_SUB);
      node->set_left(prev);
      node->set_right(parse_add_expr(in, scope, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");

      tok = next_token(in, tflags);
    }
    push_token(tok);
  }

  return node;
}

ptr_op_t
parser_t::parse_logic_expr(std::istream& in, scope_t& scope, const flags_t tflags) const
{
  ptr_op_t node(parse_add_expr(in, scope, tflags));

  if (node) {
    op_t::kind_t kind	= op_t::LAST;
    flags_t	 _flags = tflags;
    token_t&	 tok	= next_token(in, tflags);
    switch (tok.kind) {
    case token_t::EQUAL:
      if (tflags & EXPR_PARSE_NO_ASSIGN)
	tok.rewind(in);
      else
	kind = op_t::O_EQ;
      break;
    case token_t::NEQUAL:
      kind = op_t::O_NEQ;
      break;
    case token_t::LESS:
      kind = op_t::O_LT;
      break;
    case token_t::LESSEQ:
      kind = op_t::O_LTE;
      break;
    case token_t::GREATER:
      kind = op_t::O_GT;
      break;
    case token_t::GREATEREQ:
      kind = op_t::O_GTE;
      break;
    default:
      push_token(tok);
      break;
    }

    if (kind != op_t::LAST) {
      ptr_op_t prev(node);
      node = new op_t(kind);
      node->set_left(prev);
      node->set_right(parse_add_expr(in, scope, _flags));

      if (! node->right()) {
	if (tok.kind == token_t::PLUS)
	  throw_(parse_error,
		 tok.symbol << " operator not followed by argument");
	else
	  throw_(parse_error,
		 tok.symbol << " operator not followed by argument");
      }
    }
  }

  return node;
}

ptr_op_t
parser_t::parse_and_expr(std::istream& in, scope_t& scope, const flags_t tflags) const
{
  ptr_op_t node(parse_logic_expr(in, scope, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::KW_AND) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_AND);
      node->set_left(prev);
      node->set_right(parse_and_expr(in, scope, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node;
}

ptr_op_t
parser_t::parse_or_expr(std::istream& in, scope_t& scope, const flags_t tflags) const
{
  ptr_op_t node(parse_and_expr(in, scope, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::KW_OR) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_OR);
      node->set_left(prev);
      node->set_right(parse_or_expr(in, scope, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node;
}

ptr_op_t
parser_t::parse_value_expr(std::istream& in, scope_t& scope, const flags_t tflags) const
{
  ptr_op_t node(parse_or_expr(in, scope, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::COMMA) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_COMMA);
      node->set_left(prev);
      node->set_right(parse_value_expr(in, scope, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
      tok = next_token(in, tflags);
    }

    if (tok.kind != token_t::TOK_EOF) {
      if (tflags & EXPR_PARSE_PARTIAL)
	push_token(tok);
      else
	tok.unexpected();
    }
  }
  else if (! (tflags & EXPR_PARSE_PARTIAL)) {
    throw_(parse_error, "Failed to parse value expression");
  }

  return node;
}

bool op_t::print(std::ostream& out, print_context_t& context) const
{
  bool found = false;

  if (context.start_pos && this == context.op_to_find) {
    *context.start_pos = (long)out.tellp() - 1;
    found = true;
  }

  string symbol;

  switch (kind) {
  case VALUE: {
    as_value().print(out, context.relaxed);
    break;
  }

#if 0
  case FUNC_NAME:
    out << as_string();
    break;

  case ATTR_NAME:
    out << '@' << as_string();
    break;

  case VAR_NAME:
    out << '$' << as_string();
    break;
#endif

  case FUNCTION:
    out << "<FUNCTION>";
    break;

  case ARG_INDEX:
    out << '@' << as_long();
    break;

  case O_NOT:
    out << "!";
    if (left() && left()->print(out, context))
      found = true;
    break;
  case O_NEG:
    out << "-";
    if (left() && left()->print(out, context))
      found = true;
    break;

  case O_ADD:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " + ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_SUB:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " - ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_MUL:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " * ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_DIV:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " / ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_NEQ:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " != ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_EQ:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " == ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_LT:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " < ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_LTE:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " <= ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_GT:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " > ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_GTE:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " >= ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_AND:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " & ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
  case O_OR:
    out << "(";
    if (left() && left()->print(out, context))
      found = true;
    out << " | ";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_COMMA:
    if (left() && left()->print(out, context))
      found = true;
    out << ", ";
    if (right() && right()->print(out, context))
      found = true;
    break;

#if 0
  case O_CALL:
    if (left() && left()->print(out, context))
      found = true;
    out << "(";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;
#endif

  case LAST:
  default:
    assert(false);
    break;
  }

  if (! symbol.empty()) {
    if (amount_t::current_pool->find(symbol))
      out << '@';
    out << symbol;
  }

  if (context.end_pos && this == context.op_to_find)
    *context.end_pos = (long)out.tellp() - 1;

  return found;
}

void op_t::dump(std::ostream& out, const int depth) const
{
  out.setf(std::ios::left);
  out.width(10);
  out << this << " ";

  for (int i = 0; i < depth; i++)
    out << " ";

  switch (kind) {
  case VALUE:
    out << "VALUE - " << as_value();
    break;

#if 0
  case ATTR_NAME:
    out << "ATTR_NAME - " << as_string();
    break;

  case FUNC_NAME:
    out << "FUNC_NAME - " << as_string();
    break;

  case VAR_NAME:
    out << "VAR_NAME - " << as_string();
    break;
#endif

  case ARG_INDEX:
    out << "ARG_INDEX - " << as_long();
    break;

  case FUNCTION:
    out << "FUNCTION";
    break;

#if 0
  case O_CALL:	 out << "O_CALL"; break;
#endif

  case O_NOT:	 out << "O_NOT"; break;
  case O_NEG:	 out << "O_NEG"; break;

  case O_ADD:	 out << "O_ADD"; break;
  case O_SUB:	 out << "O_SUB"; break;
  case O_MUL:	 out << "O_MUL"; break;
  case O_DIV:	 out << "O_DIV"; break;

  case O_NEQ:	 out << "O_NEQ"; break;
  case O_EQ:	 out << "O_EQ"; break;
  case O_LT:	 out << "O_LT"; break;
  case O_LTE:	 out << "O_LTE"; break;
  case O_GT:	 out << "O_GT"; break;
  case O_GTE:	 out << "O_GTE"; break;

  case O_AND:	 out << "O_AND"; break;
  case O_OR:	 out << "O_OR"; break;

  case O_COMMA:	 out << "O_COMMA"; break;

  case LAST:
  default:
    assert(false);
    break;
  }

  out << " (" << refc << ')' << std::endl;

  if (kind > TERMINALS) {
    if (left()) {
      left()->dump(out, depth + 1);
      if (right())
	right()->dump(out, depth + 1);
    } else {
      assert(! right());
    }
  }
}

} // namespace expr
} // namespace ledger
