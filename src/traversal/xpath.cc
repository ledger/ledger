/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#include "xpath.h"
#include "parser.h"

namespace ledger {
namespace xml {

#ifndef THREADSAFE
xpath_t::token_t * xpath_t::lookahead = NULL;
#endif

void xpath_t::initialize()
{
  lookahead = new xpath_t::token_t;
}

void xpath_t::shutdown()
{
  checked_delete(lookahead);
  lookahead = NULL;
}

void xpath_t::token_t::parse_ident(std::istream& in)
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

  kind = IDENT;
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
  case 'u':
    if (std::strcmp(buf, "union") == 0)
      kind = KW_UNION;
    break;
  }

  if (kind == IDENT)
    value.set_string(buf);
}

void xpath_t::token_t::next(std::istream& in, flags_t flags)
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

  if (! (flags & XPATH_PARSE_RELAXED) &&
      (std::isalpha(c) || c == '_')) {
    parse_ident(in);
    return;
  }

  switch (c) {
  case '@':
    in.get(c);
    kind = AT_SYM;
    break;
  case '$':
    in.get(c);
    kind = DOLLAR;
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
    if (flags & XPATH_PARSE_ALLOW_DATE) {
      char buf[256];
      READ_INTO_(in, buf, 255, c, length, c != ']');
      if (c != ']')
	unexpected(c, ']');
      in.get(c);
      length++;
      interval_t timespan(buf);
      kind = VALUE;
      value = timespan.next();
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

  case '/':
    in.get(c);
    kind = SLASH;
    break;

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

  case '|':
    in.get(c);
    kind = PIPE;
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
    if (! (flags & XPATH_PARSE_RELAXED)) {
      kind = UNKNOWN;
    } else {
      amount_t temp;
      unsigned long pos = 0;

      // When in relaxed parsing mode, we want to migrate commodity
      // flags so that any precision specified by the user updates the
      // current maximum displayed precision.
      try {
	pos = (long)in.tellg();

	unsigned char parse_flags = 0;
	if (flags & XPATH_PARSE_NO_MIGRATE)
	  parse_flags |= AMOUNT_PARSE_NO_MIGRATE;
	if (flags & XPATH_PARSE_NO_REDUCE)
	  parse_flags |= AMOUNT_PARSE_NO_REDUCE;

	temp.parse(in, parse_flags);

	kind = VALUE;
	value = temp;
      }
      catch (amount_error& err) {
	// If the amount had no commodity, it must be an unambiguous
	// variable reference

	// jww (2007-04-19): There must be a more efficient way to do this!
	if (std::strcmp(err.what(), "No quantity specified for amount") == 0) {
	  in.clear();
	  in.seekg(pos, std::ios::beg);

	  c = in.peek();
	  assert(! (std::isdigit(c) || c == '.'));
	  parse_ident(in);
	} else {
	  throw;
	}
      }
    }
    break;
  }
}

void xpath_t::token_t::rewind(std::istream& in)
{
  for (unsigned int i = 0; i < length; i++)
    in.unget();
}


void xpath_t::token_t::unexpected()
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

void xpath_t::token_t::unexpected(char c, char wanted)
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


void xpath_t::scope_t::define(const string& name, const value_t& val) {
  define(name, op_t::wrap_value(val));
}

void xpath_t::symbol_scope_t::define(const string& name, ptr_op_t def)
{
  DEBUG("ledger.xpath.syms", "Defining '" << name << "' = " << def);

  std::pair<symbol_map::iterator, bool> result
    = symbols.insert(symbol_map::value_type(name, def));
  if (! result.second) {
    symbol_map::iterator i = symbols.find(name);
    assert(i != symbols.end());
    symbols.erase(i);

    std::pair<symbol_map::iterator, bool> result2
      = symbols.insert(symbol_map::value_type(name, def));
    if (! result2.second)
      throw_(compile_error,
	     "Redefinition of '" << name << "' in same scope");
  }
}

namespace {
  value_t xpath_fn_last(xpath_t::call_scope_t& scope)
  {
    xpath_t::context_scope_t& context(CONTEXT_SCOPE(scope));
    return context.size();
  }

  value_t xpath_fn_position(xpath_t::call_scope_t& scope)
  {
    xpath_t::context_scope_t& context(CONTEXT_SCOPE(scope));
    return context.index() + 1;
  }

  value_t xpath_fn_text(xpath_t::call_scope_t& scope)
  {
    xpath_t::context_scope_t& context(CONTEXT_SCOPE(scope));
    return value_t(context.xml_node().to_value().to_string(), true);
  }

  value_t xpath_fn_type(xpath_t::call_scope_t& scope)
  {
    if (scope.size() == 0) {
      xpath_t::context_scope_t& context(CONTEXT_SCOPE(scope));
      return string_value(context.value().label());
    }
    else if (scope.size() == 1) {
      return string_value(scope[0].label());
    }
    else {
      assert(false);
      return string_value("INVALID");
    }
  }
}

xpath_t::ptr_op_t
xpath_t::symbol_scope_t::lookup(const string& name)
{
  switch (name[0]) {
  case 'l':
    if (name == "last")
      return WRAP_FUNCTOR(bind(xpath_fn_last, _1));
    break;

  case 'p':
    if (name == "position")
      return WRAP_FUNCTOR(bind(xpath_fn_position, _1));
    break;

  case 't':
    if (name == "text")
      return WRAP_FUNCTOR(bind(xpath_fn_text, _1));
    else if (name == "type")
      return WRAP_FUNCTOR(bind(xpath_fn_type, _1));
    break;
  }

  symbol_map::const_iterator i = symbols.find(name);
  if (i != symbols.end())
    return (*i).second;

  return child_scope_t::lookup(name);
}


xpath_t::ptr_op_t
xpath_t::parse_value_term(std::istream& in, flags_t tflags) const
{
  ptr_op_t node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::VALUE:
    node = new op_t(op_t::VALUE);
    node->set_value(tok.value);
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
      node = new op_t(op_t::FUNC_NAME);
      node->set_string(ident);

      ptr_op_t call_node(new op_t(op_t::O_CALL));
      call_node->set_left(node);
      call_node->set_right(parse_value_expr(in, tflags | XPATH_PARSE_PARTIAL));

      tok = next_token(in, tflags);
      if (tok.kind != token_t::RPAREN)
	tok.unexpected(0xff, ')');

      node = call_node;
    } else {
      if (std::isdigit(ident[0])) {
	node = new op_t(op_t::ARG_INDEX);
	node->set_long(lexical_cast<unsigned int>(ident.c_str()));
      }
      else if (optional<node_t::nameid_t> id =
	       document_t::lookup_builtin_id(ident)) {
	node = new op_t(op_t::NODE_ID);
	node->set_name(*id);
      }
      else {
	node = new op_t(op_t::NODE_NAME);
	node->set_string(ident);
      }
      push_token(tok);
    }
    break;
  }

  case token_t::AT_SYM: {
    tok = next_token(in, tflags);
    if (tok.kind != token_t::IDENT)
      throw_(parse_error, "@ symbol must be followed by attribute name");

    string ident = tok.value.as_string();
    if (optional<node_t::nameid_t> id = document_t::lookup_builtin_id(ident)) {
      node = new op_t(op_t::ATTR_ID);
      node->set_name(*id);
    }
    else {
      node = new op_t(op_t::ATTR_NAME);
      node->set_string(ident);
    }
    break;
  }

  case token_t::DOLLAR:
    tok = next_token(in, tflags);
    if (tok.kind != token_t::IDENT)
      throw parse_error("$ symbol must be followed by variable name");

    node = new op_t(op_t::VAR_NAME);
    node->set_string(tok.value.as_string());
    break;

  case token_t::DOT:
    node = new op_t(op_t::NODE_ID);
    node->set_name(document_t::CURRENT);
    break;
  case token_t::DOTDOT:
    node = new op_t(op_t::NODE_ID);
    node->set_name(document_t::PARENT);
    break;
  case token_t::SLASH:
    node = new op_t(op_t::NODE_ID);
    node->set_name(document_t::ROOT);
    push_token();
    break;
  case token_t::STAR:
    node = new op_t(op_t::NODE_ID);
    node->set_name(document_t::ALL);
    break;

  case token_t::LPAREN:
    node = new op_t(op_t::O_COMMA);
    node->set_left(parse_value_expr(in, tflags | XPATH_PARSE_PARTIAL));
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

xpath_t::ptr_op_t
xpath_t::parse_predicate_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_value_term(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    while (tok.kind == token_t::LBRACKET) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_PRED);
      node->set_left(prev);
      node->set_right(parse_value_expr(in, tflags | XPATH_PARSE_PARTIAL));
      if (! node->right())
	throw_(parse_error, "[ operator not followed by valid expression");

      tok = next_token(in, tflags);
      if (tok.kind != token_t::RBRACKET)
	tok.unexpected(0xff, ']');

      tok = next_token(in, tflags);
    }

    push_token(tok);
  }

  return node;
}

xpath_t::ptr_op_t
xpath_t::parse_path_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_predicate_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::SLASH) {
      ptr_op_t prev(node);

      tok  = next_token(in, tflags);
      node = new op_t(tok.kind == token_t::SLASH ?
		      op_t::O_RFIND : op_t::O_FIND);
      if (tok.kind != token_t::SLASH)
	push_token(tok);

      node->set_left(prev);
      node->set_right(parse_path_expr(in, tflags));
      if (! node->right())
	throw_(parse_error, "/ operator not followed by a valid term");
    } else {
      push_token(tok);
    }
  }

  return node;
}

xpath_t::ptr_op_t
xpath_t::parse_unary_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::EXCLAM: {
    ptr_op_t texpr(parse_path_expr(in, tflags));
    if (! texpr)
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");

    // A very quick optimization
    if (texpr->kind == op_t::VALUE) {
      texpr->as_value().in_place_negate();
      node = texpr;
    } else {
      node = new op_t(op_t::O_NOT);
      node->set_left(texpr);
    }
    break;
  }

  case token_t::MINUS: {
    ptr_op_t texpr(parse_path_expr(in, tflags));
    if (! texpr)
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");

    // A very quick optimization
    if (texpr->kind == op_t::VALUE) {
      texpr->as_value().in_place_negate();
      node = texpr;
    } else {
      node = new op_t(op_t::O_NEG);
      node->set_left(texpr);
    }
    break;
  }

  default:
    push_token(tok);
    node = parse_path_expr(in, tflags);
    break;
  }

  return node;
}

xpath_t::ptr_op_t
xpath_t::parse_union_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_unary_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::PIPE || tok.kind == token_t::KW_UNION) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_UNION);
      node->set_left(prev);
      node->set_right(parse_union_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }

  return node;
}

xpath_t::ptr_op_t
xpath_t::parse_mul_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_union_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::STAR || tok.kind == token_t::KW_DIV) {
      ptr_op_t prev(node);
      node = new op_t(tok.kind == token_t::STAR ?
		      op_t::O_MUL : op_t::O_DIV);
      node->set_left(prev);
      node->set_right(parse_mul_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");

      tok = next_token(in, tflags);
    }
    push_token(tok);
  }

  return node;
}

xpath_t::ptr_op_t
xpath_t::parse_add_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_mul_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::PLUS ||
	tok.kind == token_t::MINUS) {
      ptr_op_t prev(node);
      node = new op_t(tok.kind == token_t::PLUS ?
		      op_t::O_ADD : op_t::O_SUB);
      node->set_left(prev);
      node->set_right(parse_add_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");

      tok = next_token(in, tflags);
    }
    push_token(tok);
  }

  return node;
}

xpath_t::ptr_op_t
xpath_t::parse_logic_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_add_expr(in, tflags));

  if (node) {
    op_t::kind_t kind	= op_t::LAST;
    flags_t	 _flags = tflags;
    token_t&	 tok	= next_token(in, tflags);
    switch (tok.kind) {
    case token_t::EQUAL:
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
      node->set_right(parse_add_expr(in, _flags));

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

xpath_t::ptr_op_t
xpath_t::parse_and_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_logic_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::KW_AND) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_AND);
      node->set_left(prev);
      node->set_right(parse_and_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node;
}

xpath_t::ptr_op_t
xpath_t::parse_or_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_and_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::KW_OR) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_OR);
      node->set_left(prev);
      node->set_right(parse_or_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node;
}

xpath_t::ptr_op_t
xpath_t::parse_value_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_or_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::COMMA) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_COMMA);
      node->set_left(prev);
      node->set_right(parse_value_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
      tok = next_token(in, tflags);
    }

    if (tok.kind != token_t::TOK_EOF) {
      if (tflags & XPATH_PARSE_PARTIAL)
	push_token(tok);
      else
	tok.unexpected();
    }
  }
  else if (! (tflags & XPATH_PARSE_PARTIAL)) {
    throw_(parse_error, "Failed to parse value expression");
  }

  return node;
}

xpath_t::ptr_op_t
xpath_t::parse_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_value_expr(in, tflags));

  if (use_lookahead) {
    use_lookahead = false;
#ifdef THREADSAFE
    lookahead.rewind(in);
#else
    lookahead->rewind(in);
#endif
  }
#ifdef THREADSAFE
  lookahead.clear();
#else
  lookahead->clear();
#endif

  return node;
}


xpath_t::ptr_op_t xpath_t::op_t::compile(scope_t& scope)
{
  switch (kind) {
  case VAR_NAME:
  case FUNC_NAME:
    if (ptr_op_t def = scope.lookup(as_string())) {
#if 1
      return def;
#else
      // Aren't definitions compiled when they go in?  Would
      // recompiling here really add any benefit?
      return def->compile(scope);
#endif
    }
    return this;

  default:
    break;
  }

  if (kind < TERMINALS)
    return this;

  ptr_op_t lhs(left()->compile(scope));
  ptr_op_t rhs(right() ? right()->compile(scope) : ptr_op_t());

  if (lhs == left() && (! rhs || rhs == right()))
    return this;

  ptr_op_t intermediate(copy(lhs, rhs));

  if (lhs->is_value() && (! rhs || rhs->is_value()))
    return wrap_value(intermediate->calc(scope));

  return intermediate;
}


value_t xpath_t::op_t::current_value(scope_t& scope)
{
  xpath_t::context_scope_t& context(CONTEXT_SCOPE(scope));
  return context.value();
}

node_t& xpath_t::op_t::current_xml_node(scope_t& scope)
{
  xpath_t::context_scope_t& context(CONTEXT_SCOPE(scope));
  return context.xml_node();
}

namespace {
  value_t select_nodes(xpath_t::scope_t& scope, const value_t& nodes,
		       xpath_t::ptr_op_t selection_path, bool recurse);

  value_t select_recursively(xpath_t::scope_t& scope, node_t& xml_node,
			     xpath_t::ptr_op_t selection_path)
  {
    value_t result;

    if (xml_node.is_parent_node()) {
      parent_node_t& parent_node(xml_node.as_parent_node());
      foreach (node_t * child, parent_node)
	result.push_back(select_nodes(scope, child, selection_path, true));
    }
    return result;
  }

  value_t select_nodes(xpath_t::scope_t& scope, const value_t& nodes,
		       xpath_t::ptr_op_t selection_path, bool recurse)
  {
    if (nodes.is_null())
      return NULL_VALUE;

    value_t result;

    if (! nodes.is_sequence()) {
      xpath_t::context_scope_t node_scope(scope, nodes, 0, 1);
      result.push_back(selection_path->calc(node_scope));

      if (recurse && nodes.is_xml_node())
	result.push_back(select_recursively(scope, *nodes.as_xml_node(),
					    selection_path));
    } else {
      std::size_t index = 0;
      std::size_t size  = nodes.as_sequence().size();

      foreach (const value_t& node, nodes.as_sequence()) {
	xpath_t::context_scope_t node_scope(scope, node, index, size);
	result.push_back(selection_path->calc(node_scope));

	if (recurse && nodes.is_xml_node())
	  result.push_back(select_recursively(scope, *node.as_xml_node(),
					      selection_path));

	index++;
      }
    }
    return result;
  }
}

value_t xpath_t::op_t::calc(scope_t& scope)
{
  bool find_all_nodes = false;

  switch (kind) {
  case VALUE:
    return as_value();

  case VAR_NAME:
  case FUNC_NAME:
    if (ptr_op_t reference = compile(scope)) {
      return reference->calc(scope);
    } else {
      throw_(calc_error, "No " << (kind == VAR_NAME ? "variable" : "function")
	     << " named '" << as_string() << "'");
    }
    break;

  case FUNCTION:
    // This should never be evaluated directly; it only appears as the
    // left node of an O_CALL operator.
    assert(false);
    break;

  case O_CALL: {
    call_scope_t call_args(scope);

    if (right())
      call_args.set_args(right()->calc(scope));

    ptr_op_t func = left();
    string   name;

    if (func->kind == FUNC_NAME) {
      name = func->as_string();
      func = func->compile(scope);
    }

    if (func->kind != FUNCTION)
      throw_(calc_error,
	     name.empty() ? string("Attempt to call non-function") :
	     (string("Attempt to call unknown function '") + name + "'"));

    return func->as_function()(call_args);
  }

  case ARG_INDEX: {
    call_scope_t& args(CALL_SCOPE(scope));

    if (as_long() >= 0 && as_long() < args.size())
      return args[as_long()];
    else
      throw_(calc_error, "Reference to non-existing argument");
    break;
  }

  case O_FIND:
  case O_RFIND:
    return select_nodes(scope, left()->calc(scope), right(), kind == O_RFIND);

  case O_PRED: {
    value_t values = left()->calc(scope);

    if (! values.is_null()) {
      op_predicate pred(right());

      if (! values.is_sequence()) {
	context_scope_t value_scope(scope, values, 0, 1);
	if (pred(value_scope))
	  return values;
	return NULL_VALUE;
      } else {
	std::size_t index = 0;
	std::size_t size  = values.as_sequence().size();

	value_t result;

	foreach (const value_t& value, values.as_sequence()) {
	  context_scope_t value_scope(scope, value, index, size);
	  if (pred(value_scope))
	    result.push_back(value);
	  index++;
	}
	return result;
      }
    }
    break;
  }

  case NODE_ID:
    switch (as_name()) {
    case document_t::CURRENT:
      return current_value(scope);

    case document_t::PARENT:
      if (optional<parent_node_t&> parent = current_xml_node(scope).parent())
	return &*parent;
      else
	throw_(std::logic_error, "Attempt to access parent of root node");
      break;

    case document_t::ROOT:
      return &current_xml_node(scope).document();

    case document_t::ALL:
      find_all_nodes = true;
      break;

    default:
      break;			// pass down to the NODE_NAME case
    }
    // fall through...

  case NODE_NAME: {
    node_t& current_node(current_xml_node(scope));

    if (current_node.is_parent_node()) {
      const bool have_name_id = kind == NODE_ID;

      parent_node_t& parent(current_node.as_parent_node());

      value_t result;
      foreach (node_t * child, parent) {
	if (find_all_nodes ||
	    (  have_name_id && as_name()   == child->name_id()) ||
	    (! have_name_id && as_string() == child->name()))
	  result.push_back(child);
      }
      return result;
    }
    break;
  }

  case ATTR_ID:
  case ATTR_NAME:
    if (optional<value_t&> value =
	kind == ATTR_ID ? current_xml_node(scope).get_attr(as_name()) :
	                  current_xml_node(scope).get_attr(as_string()))
      return *value;

    break;

  case O_NEQ:
    return left()->calc(scope) != right()->calc(scope);
  case O_EQ:
    return left()->calc(scope) == right()->calc(scope);
  case O_LT:
    return left()->calc(scope) <  right()->calc(scope);
  case O_LTE:
    return left()->calc(scope) <= right()->calc(scope);
  case O_GT:
    return left()->calc(scope) >  right()->calc(scope);
  case O_GTE:
    return left()->calc(scope) >= right()->calc(scope);

  case O_ADD:
    return left()->calc(scope) + right()->calc(scope);
  case O_SUB:
    return left()->calc(scope) - right()->calc(scope);
  case O_MUL:
    return left()->calc(scope) * right()->calc(scope);
  case O_DIV:
    return left()->calc(scope) / right()->calc(scope);

  case O_NEG:
    assert(! right());
    return left()->calc(scope).negate();

  case O_NOT:
    assert(! right());
    return ! left()->calc(scope);

  case O_AND:
    return left()->calc(scope) && right()->calc(scope);
  case O_OR:
    return left()->calc(scope) || right()->calc(scope);

  case O_COMMA:
  case O_UNION: {
    value_t result(left()->calc(scope));

    ptr_op_t next = right();
    while (next) {
      ptr_op_t value_op;
      if (next->kind == O_COMMA || next->kind == O_UNION) {
	value_op = next->left();
	next     = next->right();
      } else {
	value_op = next;
	next     = NULL;
      }

      result.push_back(value_op->calc(scope));
    }
    return result;
  }

  case LAST:
  default:
    assert(false);
    break;
  }

  return NULL_VALUE;
}


bool xpath_t::op_t::print(std::ostream& out, print_context_t& context) const
{
  bool found = false;

  if (context.start_pos && this == context.op_to_find) {
    *context.start_pos = (long)out.tellp() - 1;
    found = true;
  }

  string symbol;

  switch (kind) {
  case VALUE: {
    const value_t& value(as_value());
    switch (value.type()) {
    case value_t::VOID:
      out << "<VOID>";
      break;
    case value_t::BOOLEAN:
      if (value)
	out << "1";
      else
	out << "0";
      break;
    case value_t::INTEGER:
      out << value;
      break;
    case value_t::AMOUNT:
      if (! context.relaxed)
	out << '{';
      out << value;
      if (! context.relaxed)
	out << '}';
      break;
    case value_t::BALANCE:
    case value_t::BALANCE_PAIR:
      assert(false);
      break;
    case value_t::DATETIME:
      out << '[' << value << ']';
      break;
    case value_t::STRING:
      out << '"' << value << '"';
      break;

    case value_t::XML_NODE:
      out << '<' << value << '>';
      break;
    case value_t::POINTER:
      out << '&' << value;
      break;
    case value_t::SEQUENCE:
      out << '~' << value << '~';
      break;
    }
    break;
  }

  case ATTR_ID:
    out << '@';
    // fall through...
  case NODE_ID: {
    context_scope_t& node_scope(CONTEXT_SCOPE(context.scope));
    if (optional<const char *> name =
	node_scope.xml_node().document().lookup_name(as_name()))
      out << *name;
    else
      out << '#' << as_name();
    break;
  }

  case NODE_NAME:
  case FUNC_NAME:
    out << as_string();
    break;

  case ATTR_NAME:
    out << '@' << as_string();
    break;

  case VAR_NAME:
    out << '$' << as_string();
    break;

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

  case O_UNION:
    if (left() && left()->print(out, context))
      found = true;
    out << " | ";
    if (right() && right()->print(out, context))
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

  case O_CALL:
    if (left() && left()->print(out, context))
      found = true;
    out << "(";
    if (right() && right()->print(out, context))
      found = true;
    out << ")";
    break;

  case O_FIND:
    if (left() && left()->print(out, context))
      found = true;
    out << "/";
    if (right() && right()->print(out, context))
      found = true;
    break;
  case O_RFIND:
    if (left() && left()->print(out, context))
      found = true;
    out << "//";
    if (right() && right()->print(out, context))
      found = true;
    break;
  case O_PRED:
    if (left() && left()->print(out, context))
      found = true;
    out << "[";
    if (right() && right()->print(out, context))
      found = true;
    out << "]";
    break;

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

void xpath_t::op_t::dump(std::ostream& out, const int depth) const
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

  case NODE_NAME:
    out << "NODE_NAME - " << as_string();
    break;
  case NODE_ID:
    out << "NODE_ID - " << as_name();
    break;

  case ATTR_NAME:
    out << "ATTR_NAME - " << as_string();
    break;
  case ATTR_ID:
    out << "ATTR_ID - " << as_name();
    break;

  case FUNC_NAME:
    out << "FUNC_NAME - " << as_string();
    break;

  case VAR_NAME:
    out << "VAR_NAME - " << as_string();
    break;

  case ARG_INDEX:
    out << "ARG_INDEX - " << as_long();
    break;

  case FUNCTION:
    out << "FUNCTION";
    break;

  case O_CALL:	 out << "O_CALL"; break;

  case O_NOT:	 out << "O_NOT"; break;
  case O_NEG:	 out << "O_NEG"; break;

  case O_UNION:	 out << "O_UNION"; break;

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

  case O_FIND:	 out << "O_FIND"; break;
  case O_RFIND:	 out << "O_RFIND"; break;
  case O_PRED:	 out << "O_PRED"; break;

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

} // namespace xml


value_t xml_command(xml::xpath_t::call_scope_t& args)
{
  assert(args.size() == 0);

  value_t	ostream = args.resolve("ostream");
  std::ostream& outs(ostream.as_ref_lval<std::ostream>());

  xml::xpath_t::context_scope_t& node_context(CONTEXT_SCOPE(args));
  node_context.xml_node().print(outs);

  return true;
}

} // namespace ledger
