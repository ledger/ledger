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
#if 0
  case '$':
    in.get(c);
    kind = DOLLAR;
    break;
#endif

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

  case '"': {
    in.get(c);
    char buf[4096];
    READ_INTO_(in, buf, 4095, c, length, c != '"');
    if (c != '"')
      unexpected(c, '"');
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
    if (in.peek() == '*') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = POWER;
      length = 2;
      break;
    }
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

  case '&':
    in.get(c);
    kind = AMPER;
    break;
  case '|':
    in.get(c);
    kind = PIPE;
    break;
  case '?':
    in.get(c);
    kind = QUESTION;
    break;
  case ':':
    in.get(c);
    if (in.peek() == '=') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = ASSIGN;
      length = 2;
      break;
    }
    kind = COLON;
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

xpath_t::ptr_op_t xpath_t::wrap_value(const value_t& val)
{
  xpath_t::ptr_op_t temp(new xpath_t::op_t(xpath_t::op_t::VALUE));
  temp->set_value(val);
  return temp;
}

xpath_t::ptr_op_t xpath_t::wrap_functor(const function_t& fobj)
{
  xpath_t::ptr_op_t temp(new xpath_t::op_t(xpath_t::op_t::FUNCTION));
  temp->set_function(fobj);
  return temp;
}

void xpath_t::scope_t::define(const string& name, ptr_op_t def)
{
  DEBUG("ledger.xpath.syms", "Defining '" << name << "' = " << def);

  std::pair<symbol_map::iterator, bool> result
    = symbols.insert(symbol_map::value_type(name, def));
  if (! result.second) {
    symbol_map::iterator i = symbols.find(name);
    assert(i != symbols.end());
    (*i).second->release();
    symbols.erase(i);

    std::pair<symbol_map::iterator, bool> result2
      = symbols.insert(symbol_map::value_type(name, def));
    if (! result2.second)
      throw_(compile_error,
	     "Redefinition of '" << name << "' in same scope");
  }
  def->acquire();
}

xpath_t::ptr_op_t
xpath_t::scope_t::lookup(const string& name)
{
  symbol_map::const_iterator i = symbols.find(name);
  if (i != symbols.end())
    return (*i).second;
  else if (parent)
    return parent->lookup(name);
  return NULL;
}

void xpath_t::scope_t::define(const string& name, const function_t& def) {
  define(name, wrap_functor(def));
}

optional<value_t>
xpath_t::function_scope_t::resolve(const string& name, scope_t * locals)
{
  switch (name[0]) {
  case 'l':
    if (name == "last") {
      return value_t((long)size);
    }
    break;

  case 'p':
    if (name == "position") {
      return value_t((long)index + 1);
    }
    break;

  case 't':
    if (name == "text") {
      return node.to_value();
    }
    break;
  }
  return scope_t::resolve(name, locals);
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

      ptr_op_t call_node(new op_t(op_t::O_EVAL));
      call_node->set_left(node);
      call_node->set_right(parse_value_expr(in, tflags | XPATH_PARSE_PARTIAL));

      tok = next_token(in, tflags);
      if (tok.kind != token_t::RPAREN)
	tok.unexpected();		// jww (2006-09-09): wanted )

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

#if 0
  case token_t::DOLLAR:
    tok = next_token(in, tflags);
    if (tok.kind != token_t::IDENT)
      throw parse_error("$ symbol must be followed by variable name");

    node = new op_t(op_t::VAR_NAME);
    node->name = new string(tok.value.as_string());
    break;
#endif

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
    node = parse_value_expr(in, tflags | XPATH_PARSE_PARTIAL);
    if (! node)
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");
    tok = next_token(in, tflags);
    if (tok.kind != token_t::RPAREN)
      tok.unexpected();		// jww (2006-09-09): wanted )
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
	tok.unexpected();		// jww (2006-09-09): wanted ]

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
    while (tok.kind == token_t::SLASH) {
      ptr_op_t prev(node);

      tok  = next_token(in, tflags);
      node = new op_t(tok.kind == token_t::SLASH ?
		      op_t::O_RFIND : op_t::O_FIND);
      if (tok.kind != token_t::SLASH)
	push_token(tok);

      node->set_left(prev);
      node->set_right(parse_predicate_expr(in, tflags));
      if (! node->right())
	throw_(parse_error, "/ operator not followed by a valid term");

      tok = next_token(in, tflags);
    }

    push_token(tok);
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
    case token_t::ASSIGN:
      kind = op_t::O_DEFINE;
      break;
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
      if (kind == op_t::O_DEFINE)
	node->set_right(parse_querycolon_expr(in, tflags));
      else
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
xpath_t::parse_querycolon_expr(std::istream& in, flags_t tflags) const
{
  ptr_op_t node(parse_or_expr(in, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::QUESTION) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_QUES);
      node->set_left(prev);
      node->set_right(new op_t(op_t::O_COLON));
      node->right()->set_left(parse_querycolon_expr(in, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
      tok = next_token(in, tflags);
      if (tok.kind != token_t::COLON)
	tok.unexpected();	// jww (2006-09-09): wanted :
      node->right()->set_right(parse_querycolon_expr(in, tflags));
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
  ptr_op_t node(parse_querycolon_expr(in, tflags));

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

xpath_t::ptr_op_t
xpath_t::op_t::new_node(kind_t kind, ptr_op_t left, ptr_op_t right)
{
  ptr_op_t node(new op_t(kind));
  if (left)
    node->set_left(left);
  if (right)
    node->set_right(right);
  return node;
}

xpath_t::ptr_op_t
xpath_t::op_t::copy(ptr_op_t tleft, ptr_op_t tright) const
{
  ptr_op_t node(new op_t(kind));
  if (tleft)
    node->set_left(tleft);
  if (tright)
    node->set_right(tright);
  return node;
}

void xpath_t::op_t::find_values(const node_t& context, scope_t * scope,
				value_t::sequence_t& result_seq,
				bool recursive)
{
  xpath_t expr(compile(context, scope, true));

  if (expr.ptr->is_value() &&
      (expr.ptr->as_value().is_xml_node() ||
       expr.ptr->as_value().is_sequence()))
    append_value(result_seq, expr.ptr->as_value());

  if (recursive && context.is_parent_node())
    foreach (node_t * node, context.as_parent_node())
      find_values(*node, scope, result_seq, recursive);
}

bool xpath_t::op_t::test_value(const node_t& context, scope_t * scope, int index)
{
  xpath_t expr(compile(context, scope, true));

  if (expr.ptr->kind != VALUE)
    throw_(calc_error, "Predicate expression does not yield a constant value");

  switch (expr.ptr->as_value().type()) {
  case value_t::INTEGER:
  case value_t::AMOUNT:
    return expr.ptr->as_value() == value_t((long)index + 1);

  default:
    return expr.ptr->as_value().as_boolean();
  }
}

xpath_t::ptr_op_t xpath_t::op_t::defer_sequence(value_t::sequence_t& result_seq)
{
  // If not all of the elements were constants, transform the result
  // into an expression sequence using O_COMMA.

  assert(! result_seq.empty());

  if (result_seq.size() == 1)
    return wrap_value(result_seq.front());

  value_t::sequence_t::iterator i = result_seq.begin();

  ptr_op_t lit_seq(new op_t(O_COMMA));

  lit_seq->set_left(wrap_value(*i++));
  ptr_op_t* opp = &lit_seq->right();

  for (; i != result_seq.end(); i++) {
    if (*opp) {
      ptr_op_t val = *opp;
      *opp = new op_t(O_COMMA);
      (*opp)->set_left(val);
      opp = &(*opp)->right();
    }

    if (! (*i).is_type(value_t::POINTER))
      *opp = wrap_value(*i);
    else
#if 1
      assert(false);
#else
      *opp = static_cast<ptr_op_t>((*i).as_pointer());
#endif
  }

  return lit_seq;
}

void xpath_t::op_t::append_value(value_t::sequence_t& result_seq, value_t& val)
{
  if (val.is_type(value_t::SEQUENCE))
    std::for_each(val.as_sequence().begin(), val.as_sequence().end(),
		  bind(&value_t::sequence_t::push_back, ref(result_seq), _1));
  else
    result_seq.push_back(val);
}

xpath_t::ptr_op_t
xpath_t::op_t::compile(const node_t& context, scope_t * scope, bool resolve)
{
#if 0
  try {
#endif
  switch (kind) {
  case VALUE:
    return this;

  case NODE_ID:
    switch (as_name()) {
    case document_t::CURRENT:
      return wrap_value(&context);

    case document_t::PARENT:
      if (context.parent())
	return wrap_value(&*context.parent());
      else
	throw_(compile_error, "Referencing parent node from the root node");

    case document_t::ROOT:
      return wrap_value(&context.document());

    case document_t::ALL: {
      value_t::sequence_t nodes;
      foreach (node_t * node, context.as_parent_node())
	nodes.push_back(node);
      return wrap_value(nodes);
    }

    default:
      break;			// pass down to the NODE_NAME case
    }
    // fall through...

  case NODE_NAME:
    if (resolve) {
      // First, look up the symbol as a node name within the current
      // context.  If any exist, then return the set of names.

      if (context.is_parent_node()) {
	value_t::sequence_t nodes;

	foreach (node_t * node, context.as_parent_node()) {
	  if ((kind == NODE_NAME &&
	       std::strcmp(as_string().c_str(), node->name()) == 0) ||
	      (kind == NODE_ID && as_name() == node->name_id()))
	    nodes.push_back(node);
	}
	return wrap_value(nodes);
      }
    }
    else if (optional<node_t::nameid_t> id =
	     context.document().lookup_name_id(as_string())) {
      ptr_op_t node = new_node(NODE_ID);
      node->set_name(*id);
      return node;
    }
    return this;

  case ATTR_ID:
    if (optional<const string&> value = context.get_attr(as_long()))
      return wrap_value(*value);
    return this;

  case ATTR_NAME:
    if (optional<node_t::nameid_t> id =
	context.document().lookup_name_id(as_string())) {
      if (optional<const string&> value = context.get_attr(*id))
	return wrap_value(*value);
    }
    return this;

  case VAR_NAME:
  case FUNC_NAME:
    if (scope) {
      if (resolve) {
	if (optional<value_t> temp = scope->resolve(as_string()))
	  return wrap_value(*temp);
      }
      if (ptr_op_t def = scope->lookup(as_string()))
	return def->compile(context, scope, resolve);
    }
    return this;

  case ARG_INDEX:
    if (scope && scope->kind == scope_t::ARGUMENT) {
      if (as_long() < scope->args.size())
	return wrap_value(scope->args[as_long()]);
      else
	throw_(compile_error, "Reference to non-existing argument");
    } else {
      return this;
    }

  case FUNCTION:
    if (resolve)
      return wrap_value(as_function()(scope));
    else
      return this;
    break;

  case O_NOT: {
    xpath_t expr(left()->compile(context, scope, resolve));
    if (! expr.ptr->is_value()) {
      if (left() == expr.ptr)
	return this;
      else
	return copy(expr.ptr);
    }

    if (left() == expr.ptr) {
      if (expr.ptr->as_value().strip_annotations())
	return wrap_value(false);
      else
	return wrap_value(true);
    } else {
      if (expr.ptr->as_value().strip_annotations())
	expr.ptr->set_value(false);
      else
	expr.ptr->set_value(true);

      return expr.ptr;
    }
  }

  case O_NEG: {
    xpath_t expr(left()->compile(context, scope, resolve));
    if (! expr.ptr->is_value()) {
      if (left() == expr.ptr)
	return this;
      else
	return copy(expr.ptr);
    }

    if (left() == expr.ptr) {
      return wrap_value(expr.ptr->as_value().negate());
    } else {
      expr.ptr->as_value().in_place_negate();
      return expr.ptr;
    }
  }

  case O_UNION: {
    xpath_t lexpr(left()->compile(context, scope, resolve));
    xpath_t rexpr(right()->compile(context, scope, resolve));
    if (! lexpr.ptr->is_value() || ! rexpr.ptr->is_value()) {
      if (left() == lexpr.ptr && right() == rexpr.ptr)
	return this;
      else
	return copy(lexpr.ptr, rexpr.ptr);
    }

    value_t::sequence_t result_seq;

    append_value(result_seq, lexpr.ptr->as_value());
    append_value(result_seq, rexpr.ptr->as_value());

    return wrap_value(result_seq);
  }

  case O_ADD:
  case O_SUB:
  case O_MUL:
  case O_DIV: {
    xpath_t lexpr(left()->compile(context, scope, resolve));
    xpath_t rexpr(right()->compile(context, scope, resolve));
    if (! lexpr.ptr->is_value() || ! rexpr.ptr->is_value()) {
      if (left() == lexpr.ptr && right() == rexpr.ptr)
	return this;
      else
	return copy(lexpr.ptr, rexpr.ptr);
    }

    if (left() == lexpr.ptr) {
      value_t temp(lexpr.ptr->as_value());
      switch (kind) {
      case O_ADD: temp += rexpr.ptr->as_value(); break;
      case O_SUB: temp -= rexpr.ptr->as_value(); break;
      case O_MUL: temp *= rexpr.ptr->as_value(); break;
      case O_DIV: temp /= rexpr.ptr->as_value(); break;
      default: assert(false); break;
      }
      return wrap_value(temp);
    } else {
      switch (kind) {
      case O_ADD: lexpr.ptr->as_value() += rexpr.ptr->as_value(); break;
      case O_SUB: lexpr.ptr->as_value() -= rexpr.ptr->as_value(); break;
      case O_MUL: lexpr.ptr->as_value() *= rexpr.ptr->as_value(); break;
      case O_DIV: lexpr.ptr->as_value() /= rexpr.ptr->as_value(); break;
      default: assert(false); break;
      }
      return lexpr.ptr;
    }
  }

  case O_NEQ:
  case O_EQ:
  case O_LT:
  case O_LTE:
  case O_GT:
  case O_GTE: {
    xpath_t lexpr(left()->compile(context, scope, resolve));
    xpath_t rexpr(right()->compile(context, scope, resolve));
    if (! lexpr.ptr->is_value() || ! rexpr.ptr->is_value()) {
      if (left() == lexpr.ptr && right() == rexpr.ptr)
	return this;
      else
	return copy(lexpr.ptr, rexpr.ptr);
    }

    if (left() == lexpr.ptr) {
      switch (kind) {
      case O_NEQ:
	return wrap_value(lexpr.ptr->as_value() != rexpr.ptr->as_value());
	break;
      case O_EQ:
	return wrap_value(lexpr.ptr->as_value() == rexpr.ptr->as_value());
	break;
      case O_LT:
	return wrap_value(lexpr.ptr->as_value() <  rexpr.ptr->as_value());
	break;
      case O_LTE:
	return wrap_value(lexpr.ptr->as_value() <= rexpr.ptr->as_value());
	break;
      case O_GT:
	return wrap_value(lexpr.ptr->as_value() >  rexpr.ptr->as_value());
	break;
      case O_GTE:
	return wrap_value(lexpr.ptr->as_value() >= rexpr.ptr->as_value());
	break;
      default: assert(false); break;
      }
    } else {
      switch (kind) {
      case O_NEQ:
	lexpr.ptr->set_value(lexpr.ptr->as_value() != rexpr.ptr->as_value());
	break;
      case O_EQ:
	lexpr.ptr->set_value(lexpr.ptr->as_value() == rexpr.ptr->as_value());
	break;
      case O_LT:
	lexpr.ptr->set_value(lexpr.ptr->as_value() <  rexpr.ptr->as_value());
	break;
      case O_LTE:
	lexpr.ptr->set_value(lexpr.ptr->as_value() <= rexpr.ptr->as_value());
	break;
      case O_GT:
	lexpr.ptr->set_value(lexpr.ptr->as_value() >  rexpr.ptr->as_value());
	break;
      case O_GTE:
	lexpr.ptr->set_value(lexpr.ptr->as_value() >= rexpr.ptr->as_value());
	break;
      default:
	assert(false);
	break;
      }
      return lexpr.ptr;
    }
  }

  case O_AND: {
    xpath_t lexpr(left()->compile(context, scope, resolve));
    if (lexpr.ptr->is_value() && ! lexpr.ptr->as_value().strip_annotations()) {
      lexpr.ptr->set_value(false);
      return lexpr.ptr;
    }

    xpath_t rexpr(right()->compile(context, scope, resolve));
    if (! lexpr.ptr->is_value() || ! rexpr.ptr->is_value()) {
      if (left() == lexpr.ptr && right() == rexpr.ptr)
	return this;
      else
	return copy(lexpr.ptr, rexpr.ptr);
    }

    if (! rexpr.ptr->as_value().strip_annotations()) {
      if (left() == lexpr.ptr) {
	return wrap_value(false);
      } else {
	lexpr.ptr->set_value(false);
	return lexpr.ptr;
      }
    } else {
      return rexpr.ptr;
    }
  }

  case O_OR: {
    xpath_t lexpr(left()->compile(context, scope, resolve));
    if (lexpr.ptr->is_value() && lexpr.ptr->as_value().strip_annotations())
      return lexpr.ptr;

    xpath_t rexpr(right()->compile(context, scope, resolve));
    if (! lexpr.ptr->is_value() || ! rexpr.ptr->is_value()) {
      if (left() == lexpr.ptr && right() == rexpr.ptr)
	return this;
      else
	return copy(lexpr.ptr, rexpr.ptr);
    }

    if (rexpr.ptr->as_value().strip_annotations()) {
      return rexpr.ptr;
    } else {
      if (left() == lexpr.ptr) {
	return wrap_value(false);
      } else {
	lexpr.ptr->set_value(false);
	return lexpr.ptr;
      }
    }
  }

  case O_QUES: {
    assert(right()->kind == O_COLON);
    xpath_t lexpr(left()->compile(context, scope, resolve));
    if (! lexpr.ptr->is_value()) {
      xpath_t rexpr(right()->compile(context, scope, resolve));
      if (left() == lexpr.ptr && right() == rexpr.ptr)
	return this;
      else
	return copy(lexpr.ptr, rexpr.ptr);
    }

    if (lexpr.ptr->as_value().strip_annotations())
      return right()->left()->compile(context, scope, resolve);
    else
      return right()->right()->compile(context, scope, resolve);
  }

  case O_COLON: {
    xpath_t lexpr(left()->compile(context, scope, resolve));
    xpath_t rexpr(right()->compile(context, scope, resolve));
    if (left() == lexpr.ptr && right() == rexpr.ptr)
      return this;
    else
      return copy(lexpr.ptr, rexpr.ptr);
  }

  case O_COMMA: {
    // jww (2006-09-29): This should act just like union
    xpath_t lexpr(left()->compile(context, scope, resolve)); // for side-effects
    return right()->compile(context, scope, resolve);
  }

  case O_DEFINE:
    if (left()->kind == VAR_NAME || left()->kind == FUNC_NAME) {
      xpath_t rexpr(right()->compile(context, scope, resolve));
      if (scope)
	scope->define(left()->as_string(), rexpr.ptr);
      return rexpr.ptr;
    } else {
      assert(left()->kind == O_EVAL);
      assert(left()->left()->kind == FUNC_NAME);

      std::auto_ptr<scope_t> arg_scope(new scope_t(scope));

      unsigned int index = 0;
      ptr_op_t args = left()->right();
      while (args) {
	ptr_op_t arg = args;
	if (args->kind == O_COMMA) {
	  arg = args->left();
	  args = args->right();
	} else {
	  args = NULL;
	}

	// Define the parameter so that on lookup the parser will find
	// an ARG_INDEX value.
	ptr_op_t ref(new op_t(ARG_INDEX));
	ref->set_long(index++);

	assert(arg->kind == NODE_NAME);
	arg_scope->define(arg->as_string(), ref);
      }

      // jww (2006-09-16): If I compile the definition of a function,
      // I eliminate the possibility of future lookups
      //xpath_t rexpr(right->compile(arg_scope.get(), resolve));

      if (scope)
	scope->define(left()->left()->as_string(), right());

      return right();
    }

  case O_EVAL: {
    std::auto_ptr<scope_t> call_args(new scope_t(scope));
    call_args->kind = scope_t::ARGUMENT;

    value_t::sequence_t call_seq;

    ptr_op_t args = right();
    while (args) {
      ptr_op_t arg = args;
      if (args->kind == O_COMMA) {
	arg  = args->left();
	args = args->right();
      } else {
	args = NULL;
      }

      // jww (2006-09-15): Need to return a reference to these, if
      // there are undetermined arguments!
      call_seq.push_back(arg->compile(context, scope, resolve)->as_value());
    }

    call_args->args = call_seq;

    if (left()->kind == FUNC_NAME) {
      if (resolve && scope)
	if (optional<value_t> temp =
	    scope->resolve(left()->as_string(), call_args.get()))
	  return wrap_value(*temp);

      // Don't compile to the left, otherwise the function name may
      // get resolved before we have a chance to call it
      xpath_t func(left()->compile(context, scope, false));
      if (func.ptr->kind == FUNCTION) {
	return wrap_value(func.ptr->as_function()(call_args.get()));
      }
      else if (! resolve) {
	return func.ptr->compile(context, call_args.get(), resolve);
      }
      else {
	throw_(calc_error,
	       "Unknown function name '" << left()->as_string() << "'");
      }
    }
    else if (left()->kind == FUNCTION) {
      return wrap_value(left()->as_function()(call_args.get()));
    }
    else {
      assert(false);
    }
    break;
  }

  case O_FIND:
  case O_RFIND:
  case O_PRED: {
    xpath_t lexpr(left()->compile(context, scope, resolve));
    xpath_t rexpr(resolve ? right() : right()->compile(context, scope, false));

    if (! lexpr.ptr->is_value() || ! resolve) {
      if (left() == lexpr.ptr)
	return this;
      else
	return copy(lexpr.ptr, rexpr.ptr);
    }

    value_t::sequence_t result_seq;

    // jww (2006-09-24): What about when nothing is found?
    switch (lexpr.ptr->as_value().type()) {
    case value_t::XML_NODE: {
      value_t& value(lexpr.ptr->as_value());
      function_scope_t xpath_fscope(*value.as_xml_node(), 0, 1, scope);
      if (kind == O_PRED) {
	if (rexpr.ptr->test_value(*value.as_xml_node(), &xpath_fscope))
	  result_seq.push_back(value);
      } else {
	rexpr.ptr->find_values(*value.as_xml_node(), &xpath_fscope,
			       result_seq, kind == O_RFIND);
      }
      break;
    }

    case value_t::SEQUENCE: {
      const value_t::sequence_t& seq(lexpr.ptr->as_value().as_sequence());

      int index = 0;
      for (value_t::sequence_t::const_iterator i = seq.begin();
	   i != seq.end();
	   i++, index++) {
	assert(! (*i).is_type(value_t::SEQUENCE));
	if (! (*i).is_type(value_t::XML_NODE))
	  throw_(compile_error, "Attempting to apply path selection "
		 "to non-node(s)");

	function_scope_t xpath_fscope(seq, *(*i).as_xml_node(), index, scope);
	if (kind == O_PRED) {
	  if (rexpr.ptr->test_value(*(*i).as_xml_node(), &xpath_fscope, index))
	    result_seq.push_back(*i);
	} else {
	  rexpr.ptr->find_values(*(*i).as_xml_node(), &xpath_fscope, result_seq,
				 kind == O_RFIND);
	}
      }
      break;
    }

    default:
      throw_(compile_error, "Attempting to apply path selection "
	     "to non-node(s)");
    }
    return wrap_value(result_seq);
  }

  case LAST:
  default:
    assert(false);
    break;
  }
#if 0
  }
  catch (error * err) {
#if 0
    // jww (2006-09-09): I need a reference to the parent xpath_t
    if (err->context.empty() ||
	! dynamic_cast<context *>(err->context.back()))
      err->context.push_back(new context(this));
#endif
    throw err;
  }
#endif

  assert(false);
  return NULL;
}

value_t xpath_t::calc(const node_t& context, scope_t * scope) const
{
#if 0
  try {
#endif
    xpath_t final(ptr->compile(context, scope, true));
    // jww (2006-09-09): Give a better error here if this is not
    // actually a value
    return final.ptr->as_value();
#if 0
  }
  catch (error * err) {
    if (err->context.empty() ||
	! dynamic_cast<context *>(err->context.back()))
      err->context.push_back
	(new context(*this, ptr, "While calculating value expression:"));
#if 0
    error_context * last = err->context.back();
    if (context * ctxt = dynamic_cast<context *>(last)) {
      ctxt->xpath = *this;
      ctxt->desc = "While calculating value expression:";
    }
#endif
    throw err;
  }
#endif
}

#if 0
xpath_t::context::context(const xpath_t&  _xpath,
			  const ptr_op_t& _err_node,
			  const string&   desc) throw()
  : error_context(desc), xpath(_xpath), err_node(_err_node)
{
}

void xpath_t::context::describe(std::ostream& out) const throw()
{
  if (! xpath) {
    out << "xpath_t::context expr not set!" << std::endl;
    return;
  }

  if (! desc.empty())
    out << desc << std::endl;

  out << "  ";
  unsigned long start = (long)out.tellp() - 1;
  unsigned long begin;
  unsigned long end;
  bool found = false;
  if (xpath)
    xpath.print(out, true, err_node, &begin, &end);
  out << std::endl;
  if (found) {
    out << "  ";
    for (unsigned int i = 0; i < end - start; i++) {
      if (i >= begin - start)
	out << "^";
      else
	out << " ";
    }
    out << std::endl;
  }
}
#endif

bool xpath_t::op_t::print(std::ostream&	  out,
			  document_t&     document,
			  const bool      relaxed,
			  const ptr_op_t& op_to_find,
			  unsigned long * start_pos,
			  unsigned long * end_pos) const
{
  int arg_index = 0;
  bool found = false;

  if (start_pos && this == op_to_find) {
    *start_pos = (long)out.tellp() - 1;
    found = true;
  }

  string symbol;

  switch (kind) {
  case VALUE: {
    const value_t& value(as_value());
    switch (value.type()) {
    case value_t::BOOLEAN:
      if (value)
	out << "1";
      else
	out << "0";
      break;
    case value_t::INTEGER:
    case value_t::AMOUNT:
      if (! relaxed)
	out << '{';
      out << value;
      if (! relaxed)
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
    optional<const char *> name = document.lookup_name(as_name());
    if (name)
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
    out << as_function();
    break;

  case ARG_INDEX:
    out << '@' << as_long();
    break;

  case O_NOT:
    out << "!";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_NEG:
    out << "-";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_UNION:
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " | ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_ADD:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " + ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_SUB:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " - ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_MUL:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " * ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_DIV:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " / ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_NEQ:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " != ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_EQ:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " == ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_LT:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " < ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_LTE:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " <= ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_GT:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " > ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_GTE:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " >= ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_AND:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " & ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_OR:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " | ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_QUES:
    out << "(";
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " ? ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_COLON:
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " : ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_COMMA:
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ", ";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_DEFINE:
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << '=';
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_EVAL:
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "(";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_FIND:
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "/";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_RFIND:
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "//";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_PRED:
    if (left() && left()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "[";
    if (right() && right()->print(out, document, relaxed, op_to_find, start_pos, end_pos))
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

  if (end_pos && this == op_to_find)
    *end_pos = (long)out.tellp() - 1;

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
    out << "FUNCTION - " << as_function();
    break;

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

  case O_QUES:	 out << "O_QUES"; break;
  case O_COLON:	 out << "O_COLON"; break;

  case O_COMMA:	 out << "O_COMMA"; break;

  case O_DEFINE: out << "O_DEFINE"; break;
  case O_EVAL:	 out << "O_EVAL"; break;

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

xpath_t::path_t::path_t(const xpath_t& path_expr)
{
  ptr_op_t op = path_expr.ptr;

  while (true) {
    element_t element;

    switch (op->kind) {
    case op_t::O_RFIND:
      element.recurse = true;
      // fall through...
    case op_t::O_FIND: {
      ptr_op_t name;
      if (op->right()->kind == op_t::O_PRED) {
	element.predicate = op_predicate(op->right()->right());
	name = op->right()->left();
      } else {
	name = op->right();
      }

      switch (name->kind) {
      case op_t::NODE_ID: {
      //case op_t::ATTR_ID:
	node_t::nameid_t name_id = name->as_name();
	if (name_id < document_t::LAST_BUILTIN)
	  element.ident = document_t::special_names_t(name_id);
	else
	  element.ident = name_id;
	break;
      }
      case op_t::NODE_NAME:
      //case op_t::ATTR_NAME:
	element.ident = name->as_string();
	break;
      default:
	break;
      }
      break;
    }

    case op_t::NODE_ID: {
    //case op_t::ATTR_ID:
      node_t::nameid_t name_id = op->as_name();
      if (name_id < document_t::LAST_BUILTIN)
	element.ident = document_t::special_names_t(name_id);
      else
	element.ident = name_id;
      break;
    }
    case op_t::NODE_NAME:
    //case op_t::ATTR_NAME:
      element.ident = op->as_string();
      break;

    default:
      throw_(std::logic_error, "XPath expression is not strictly a path selection");
      break;
    }

    elements.push_front(element);

    if (op->kind < op_t::TERMINALS)
      break;
    else
      op = op->left();
  }
}

void xpath_t::path_t::check_element(node_t&		    start,
				    const element_iterator& element,
				    scope_t *		    scope,
				    std::size_t             index,
				    std::size_t             size,
				    const visitor_t&	    func)
{
  if (element->predicate) {
    function_scope_t xpath_fscope(start, index, size, scope);
    if (! element->predicate(start, &xpath_fscope))
      return;
  }

  element_iterator next_element = next(element);
  if (next_element == elements.end())
    func(start);
  else
    walk_elements(start, next_element, scope, func);
}

void xpath_t::path_t::walk_elements(node_t&			  start,
				    const element_iterator&	  element,
				    scope_t *		  	  scope,
				    const visitor_t&		  func)
{
  if (element->ident.type() == typeid(document_t::special_names_t)) {
    switch (boost::get<document_t::special_names_t>(element->ident)) {
    case document_t::CURRENT:
      check_element(start, element, scope, 0, 1, func);
      break;

    case document_t::PARENT:
      if (optional<parent_node_t&> parent = start.parent())
	check_element(*parent, element, scope, 0, 1, func);
      else
	throw_(std::logic_error, "Attempt to access parent of root node");
      break;

    case document_t::ROOT:
      check_element(start.document(), element, scope, 0, 1, func);
      break;

    case document_t::ALL: {
      if (! start.is_parent_node())
	throw_(compile_error, "Referencing child nodes from a non-parent value");

      std::size_t index = 0;
      std::size_t size  = start.as_parent_node().size();
      foreach (node_t * node, start.as_parent_node())
	check_element(*node, element, scope, index++, size, func);
      break;
    }
    }
  }
  else if (start.is_parent_node()) {
    bool have_name_id = element->ident.type() == typeid(node_t::nameid_t);

    std::size_t index = 0;
    std::size_t size  = start.as_parent_node().size();
    foreach (node_t * child, start.as_parent_node()) {
      if ((have_name_id &&
	   boost::get<node_t::nameid_t>(element->ident) == child->name_id()) ||
	  (! have_name_id &&
	   boost::get<string>(element->ident) == child->name()))
	check_element(*child, element, scope, index++, size, func);
      else if (element->recurse)
	walk_elements(*child, element, scope, func);
    }
  }
}

} // namespace xml
} // namespace ledger
