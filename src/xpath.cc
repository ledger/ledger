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
	     std::isalnum(c) || c == '_' || c == '.');

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

void xpath_t::token_t::next(std::istream& in, unsigned short flags)
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
#if 0
    else if (c == '~') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = NMATCH;
      length = 2;
      break;
    }
#endif
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
#if 0
    if (flags & XPATH_PARSE_REGEXP) {
      char buf[1024];
      READ_INTO_(in, buf, 1023, c, length, c != '/');
      in.get(c);
      if (c != '/')
	unexpected(c, '/');
      kind = REGEXP;
      value.set_string(buf);
      break;
    }
#endif
    kind = SLASH;
    break;

  case '=':
    in.get(c);
#if 0
    if (in.peek() == '~') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = MATCH;
      length = 2;
      break;
    }
#endif
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
#if 0
  case '%':
    in.get(c);
    kind = PERCENT;
    break;
#endif

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

xpath_t::op_t * xpath_t::wrap_value(const value_t& val)
{
  xpath_t::op_t * temp = new xpath_t::op_t(xpath_t::op_t::VALUE);
  temp->valuep = new value_t(val);
  return temp;
}

xpath_t::op_t * xpath_t::wrap_sequence(const value_t::sequence_t& val)
{
  if (val.size() == 0)
    return wrap_value(false);
  else if (val.size() == 1)
    return wrap_value(val.front());
  else
    return wrap_value(val);
}

xpath_t::op_t * xpath_t::wrap_functor(functor_t * fobj)
{
  xpath_t::op_t * temp = new xpath_t::op_t(xpath_t::op_t::FUNCTOR);
  temp->functor = fobj;
  return temp;
}

#if 0
xpath_t::op_t * xpath_t::wrap_mask(const string& pattern)
{
  xpath_t::op_t * temp = new xpath_t::op_t(xpath_t::op_t::MASK);
  temp->mask = new mask_t(pattern);
  return temp;
}
#endif

void xpath_t::scope_t::define(const string& name, op_t * def)
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

xpath_t::op_t *
xpath_t::scope_t::lookup(const string& name)
{
  symbol_map::const_iterator i = symbols.find(name);
  if (i != symbols.end())
    return (*i).second;
  else if (parent)
    return parent->lookup(name);
  return NULL;
}

void xpath_t::scope_t::define(const string& name, functor_t * def) {
  define(name, wrap_functor(def));
}

bool xpath_t::function_scope_t::resolve(const string& name,
					value_t&      result,
					scope_t *     locals)
{
  switch (name[0]) {
  case 'l':
    if (name == "last") {
      result = (long)sequence.size();
      return true;
    }
    break;

  case 'p':
    if (name == "position") {
      result = (long)index + 1;
      return true;
    }
    break;

  case 't':
    if (name == "text") {
      if (value->type == value_t::XML_NODE)
	result = value->as_xml_node()->to_value();
      else
	throw_(calc_error, "Attempt to call text() on a non-node value");
      return true;
    }
    break;
  }
  return scope_t::resolve(name, result, locals);
}

xpath_t::op_t::~op_t()
{
  TRACE_DTOR(xpath_t::op_t);

  DEBUG("ledger.xpath.memory", "Destroying " << this);
  assert(refc == 0);

  switch (kind) {
  case VALUE:
    assert(! left);
    assert(valuep);
    checked_delete(valuep);
    break;

  case NODE_NAME:
  case FUNC_NAME:
  case ATTR_NAME:
  case VAR_NAME:
    assert(! left);
    assert(name);
    checked_delete(name);
    break;

  case ARG_INDEX:
    break;

  case FUNCTOR:
    assert(! left);
    assert(functor);
    checked_delete(functor);
    break;

#if 0
  case MASK:
    assert(! left);
    assert(mask);
    checked_delete(mask);
    break;
#endif

  default:
    assert(kind < LAST);
    if (left)
      left->release();
    if (kind > TERMINALS && right)
      right->release();
    break;
  }
}

void xpath_t::op_t::get_value(value_t& result) const
{
  switch (kind) {
  case VALUE:
    result = *valuep;
    break;
  case ARG_INDEX:
    result = (long)arg_index;
    break;
  default:
    throw_(calc_error,
	   "Cannot determine value of expression symbol '" << *this << "'");
  }
}

xpath_t::op_t *
xpath_t::parse_value_term(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::VALUE:
    node.reset(new op_t(op_t::VALUE));
    node->valuep = new value_t(tok.value);
    break;

  case token_t::IDENT: {
#if 0
#ifdef USE_BOOST_PYTHON
    if (tok.value->as_string() == "lambda") // special
      try {
	char c, buf[4096];

	std::strcpy(buf, "lambda ");
	READ_INTO(in, &buf[7], 4000, c, true);

	op_t * eval = new op_t(op_t::O_EVAL);
	op_t * lambda = new op_t(op_t::FUNCTOR);
	lambda->functor = new python_functor_t(python_eval(buf));
	eval->set_left(lambda);
	op_t * sym = new op_t(op_t::SYMBOL);
	sym->name = new string("__ptr");
	eval->set_right(sym);

	node.reset(eval);

	goto done;
      }
      catch(const boost::python::error_already_set&) {
	throw_(parse_error, "Error parsing lambda expression");
      }
#endif /* USE_BOOST_PYTHON */
#endif

    string ident = tok.value.as_string();
    if (std::isdigit(ident[0])) {
      node.reset(new op_t(op_t::ARG_INDEX));
      node->arg_index = lexical_cast<unsigned int>(ident.c_str());
    }
    else if (optional<node_t::nameid_t> id =
	     document_t::lookup_builtin_id(ident)) {
      node.reset(new op_t(op_t::NODE_ID));
      node->name_id = *id;
    }
    else {
      node.reset(new op_t(op_t::NODE_NAME));
      node->name = new string(ident);
    }

    // An identifier followed by ( represents a function call
    tok = next_token(in, tflags);
    if (tok.kind == token_t::LPAREN) {
      node->kind = op_t::FUNC_NAME;

      std::auto_ptr<op_t> call_node;
      call_node.reset(new op_t(op_t::O_EVAL));
      call_node->set_left(node.release());
      call_node->set_right(parse_value_expr(in, tflags | XPATH_PARSE_PARTIAL));

      tok = next_token(in, tflags);
      if (tok.kind != token_t::RPAREN)
	tok.unexpected();		// jww (2006-09-09): wanted )

      node.reset(call_node.release());
    } else {
      push_token(tok);
    }
    break;
  }

  case token_t::AT_SYM:
    tok = next_token(in, tflags);
    if (tok.kind != token_t::IDENT)
      throw_(parse_error, "@ symbol must be followed by attribute name");

    node.reset(new op_t(op_t::ATTR_NAME));
    node->name = new string(tok.value.as_string());
    break;

#if 0
  case token_t::DOLLAR:
    tok = next_token(in, tflags);
    if (tok.kind != token_t::IDENT)
      throw parse_error("$ symbol must be followed by variable name");

    node.reset(new op_t(op_t::VAR_NAME));
    node->name = new string(tok.value.as_string());
    break;
#endif

  case token_t::DOT:
    node.reset(new op_t(op_t::NODE_ID));
    node->name_id = document_t::CURRENT;
    break;
  case token_t::DOTDOT:
    node.reset(new op_t(op_t::NODE_ID));
    node->name_id = document_t::PARENT;
    break;
  case token_t::SLASH:
    node.reset(new op_t(op_t::NODE_ID));
    node->name_id = document_t::ROOT;
    push_token();
    break;
  case token_t::STAR:
    node.reset(new op_t(op_t::NODE_ID));
    node->name_id = document_t::ALL;
    break;

  case token_t::LPAREN:
    node.reset(parse_value_expr(in, tflags | XPATH_PARSE_PARTIAL));
    if (! node.get())
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");
    tok = next_token(in, tflags);
    if (tok.kind != token_t::RPAREN)
      tok.unexpected();		// jww (2006-09-09): wanted )
    break;

#if 0
  case token_t::REGEXP:
    node.reset(wrap_mask(tok.value.as_string()));
    break;
#endif

  default:
    push_token(tok);
    break;
  }

#if 0
#ifdef USE_BOOST_PYTHON
 done:
#endif
#endif
  return node.release();
}

xpath_t::op_t *
xpath_t::parse_predicate_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_value_term(in, tflags));

  if (node.get()) {
    token_t& tok = next_token(in, tflags);
    while (tok.kind == token_t::LBRACKET) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(op_t::O_PRED));
      node->set_left(prev.release());
      node->set_right(parse_value_expr(in, tflags | XPATH_PARSE_PARTIAL));
      if (! node->right)
	throw_(parse_error, "[ operator not followed by valid expression");

      tok = next_token(in, tflags);
      if (tok.kind != token_t::RBRACKET)
	tok.unexpected();		// jww (2006-09-09): wanted ]

      tok = next_token(in, tflags);
    }

    push_token(tok);
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_path_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_predicate_expr(in, tflags));

  if (node.get()) {
    token_t& tok = next_token(in, tflags);
    while (tok.kind == token_t::SLASH) {
      std::auto_ptr<op_t> prev(node.release());

      tok = next_token(in, tflags);
      node.reset(new op_t(tok.kind == token_t::SLASH ?
			  op_t::O_RFIND : op_t::O_FIND));
      if (tok.kind != token_t::SLASH)
	push_token(tok);

      node->set_left(prev.release());
      node->set_right(parse_predicate_expr(in, tflags));
      if (! node->right)
	throw_(parse_error, "/ operator not followed by a valid term");

      tok = next_token(in, tflags);
    }

    push_token(tok);
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_unary_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node;

  token_t& tok = next_token(in, tflags);

  switch (tok.kind) {
  case token_t::EXCLAM: {
    std::auto_ptr<op_t> texpr(parse_path_expr(in, tflags));
    if (! texpr.get())
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");
    // A very quick optimization
    if (texpr->kind == op_t::VALUE) {
      *texpr->valuep = ! *texpr->valuep;
      node.reset(texpr.release());
    } else {
      node.reset(new op_t(op_t::O_NOT));
      node->set_left(texpr.release());
    }
    break;
  }

  case token_t::MINUS: {
    std::auto_ptr<op_t> texpr(parse_path_expr(in, tflags));
    if (! texpr.get())
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");
    // A very quick optimization
    if (texpr->kind == op_t::VALUE) {
      texpr->valuep->in_place_negate();
      node.reset(texpr.release());
    } else {
      node.reset(new op_t(op_t::O_NEG));
      node->set_left(texpr.release());
    }
    break;
  }

#if 0
  case token_t::PERCENT: {
    std::auto_ptr<op_t> texpr(parse_path_expr(in, tflags));
    if (! texpr.get())
      throw_(parse_error,
	     tok.symbol << " operator not followed by argument");
    // A very quick optimization
    if (texpr->kind == op_t::VALUE) {
      static value_t perc("100.0%");
      *texpr->valuep = perc * *texpr->valuep;
      node.reset(texpr.release());
    } else {
      node.reset(new op_t(op_t::O_PERC));
      node->set_left(texpr.release());
    }
    break;
  }
#endif

  default:
    push_token(tok);
    node.reset(parse_path_expr(in, tflags));
    break;
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_union_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_unary_expr(in, tflags));

  if (node.get()) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::PIPE || tok.kind == token_t::KW_UNION) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(op_t::O_UNION));
      node->set_left(prev.release());
      node->set_right(parse_union_expr(in, tflags));
      if (! node->right)
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node.release();
}

xpath_t::op_t *
xpath_t::parse_mul_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_union_expr(in, tflags));

  if (node.get()) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::STAR || tok.kind == token_t::KW_DIV) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(tok.kind == token_t::STAR ?
			  op_t::O_MUL : op_t::O_DIV));
      node->set_left(prev.release());
      node->set_right(parse_mul_expr(in, tflags));
      if (! node->right)
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");

      tok = next_token(in, tflags);
    }
    push_token(tok);
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_add_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_mul_expr(in, tflags));

  if (node.get()) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::PLUS ||
	tok.kind == token_t::MINUS) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(tok.kind == token_t::PLUS ?
			  op_t::O_ADD : op_t::O_SUB));
      node->set_left(prev.release());
      node->set_right(parse_add_expr(in, tflags));
      if (! node->right)
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");

      tok = next_token(in, tflags);
    }
    push_token(tok);
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_logic_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_add_expr(in, tflags));

  if (node.get()) {
    op_t::kind_t kind = op_t::LAST;

    unsigned short _flags = tflags;

    token_t& tok = next_token(in, tflags);
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
#if 0
    case token_t::MATCH:
      kind = op_t::O_MATCH;
      _flags |= XPATH_PARSE_REGEXP;
      break;
    case token_t::NMATCH:
      kind = op_t::O_NMATCH;
      _flags |= XPATH_PARSE_REGEXP;
      break;
#endif
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
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(kind));
      node->set_left(prev.release());
      if (kind == op_t::O_DEFINE)
	node->set_right(parse_querycolon_expr(in, tflags));
      else
	node->set_right(parse_add_expr(in, _flags));

      if (! node->right) {
	if (tok.kind == token_t::PLUS)
	  throw_(parse_error,
		 tok.symbol << " operator not followed by argument");
	else
	  throw_(parse_error,
		 tok.symbol << " operator not followed by argument");
      }
    }
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_and_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_logic_expr(in, tflags));

  if (node.get()) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::KW_AND) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(op_t::O_AND));
      node->set_left(prev.release());
      node->set_right(parse_and_expr(in, tflags));
      if (! node->right)
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node.release();
}

xpath_t::op_t *
xpath_t::parse_or_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_and_expr(in, tflags));

  if (node.get()) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::KW_OR) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(op_t::O_OR));
      node->set_left(prev.release());
      node->set_right(parse_or_expr(in, tflags));
      if (! node->right)
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node.release();
}

xpath_t::op_t *
xpath_t::parse_querycolon_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_or_expr(in, tflags));

  if (node.get()) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::QUESTION) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(op_t::O_QUES));
      node->set_left(prev.release());
      node->set_right(new op_t(op_t::O_COLON));
      node->right->set_left(parse_querycolon_expr(in, tflags));
      if (! node->right)
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
      tok = next_token(in, tflags);
      if (tok.kind != token_t::COLON)
	tok.unexpected();	// jww (2006-09-09): wanted :
      node->right->set_right(parse_querycolon_expr(in, tflags));
      if (! node->right)
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }
  return node.release();
}

xpath_t::op_t *
xpath_t::parse_value_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_querycolon_expr(in, tflags));

  if (node.get()) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::COMMA) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(op_t::O_COMMA));
      node->set_left(prev.release());
      node->set_right(parse_value_expr(in, tflags));
      if (! node->right)
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

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_expr(std::istream& in, unsigned short tflags) const
{
  std::auto_ptr<op_t> node(parse_value_expr(in, tflags));

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

  return node.release();
}

xpath_t::op_t *
xpath_t::op_t::new_node(kind_t kind, op_t * left, op_t * right)
{
  std::auto_ptr<op_t> node(new op_t(kind));
  if (left)
    node->set_left(left);
  if (right)
    node->set_right(right);
  return node.release();
}

xpath_t::op_t *
xpath_t::op_t::copy(op_t * tleft, op_t * tright) const
{
  std::auto_ptr<op_t> node(new op_t(kind));
  if (tleft)
    node->set_left(tleft);
  if (tright)
    node->set_right(tright);
  return node.release();
}

void xpath_t::op_t::find_values(value_t * context, scope_t * scope,
				value_t::sequence_t& result_seq,
				bool recursive)
{
  xpath_t expr(compile(context, scope, true));

  if (expr->kind == VALUE)
    append_value(*expr->valuep, result_seq);

  if (recursive) {
    if (context->type == value_t::XML_NODE) {
      node_t * ptr = context->as_xml_node();
      if (ptr->is_parent_node()) {
	foreach (node_t * node, ptr->as_parent_node()) {
	  value_t temp(node);
	  find_values(&temp, scope, result_seq, recursive);
	}
      }
    } else {
      throw_(calc_error, "Recursive path selection on a non-node value");
    }
  }
}

bool xpath_t::op_t::test_value(value_t * context, scope_t * scope,
			       int index)
{
  xpath_t expr(compile(context, scope, true));

  if (expr->kind != VALUE)
    throw_(calc_error, "Predicate expression does not yield a constant value");

  switch (expr->valuep->type) {
  case value_t::INTEGER:
  case value_t::AMOUNT:
    return *expr->valuep == value_t((long)index + 1);

  default:
    return expr->valuep->as_boolean();
  }
}

xpath_t::op_t * xpath_t::op_t::defer_sequence(value_t::sequence_t& result_seq)
{
  // If not all of the elements were constants, transform the result
  // into an expression sequence using O_COMMA.

  assert(! result_seq.empty());

  if (result_seq.size() == 1)
    return wrap_value(result_seq.front())->acquire();

  value_t::sequence_t::iterator i = result_seq.begin();

  std::auto_ptr<op_t> lit_seq(new op_t(O_COMMA));

  lit_seq->set_left(wrap_value(*i++));
  op_t ** opp = &lit_seq->right;

  for (; i != result_seq.end(); i++) {
    if (*opp) {
      op_t * val = *opp;
      *opp = new op_t(O_COMMA);
      (*opp)->set_left(val);
      opp = &(*opp)->right;
    }

    if ((*i).type != value_t::POINTER)
      *opp = wrap_value(*i)->acquire();
    else
      *opp = static_cast<op_t *>((*i).as_pointer());
  }

  return lit_seq.release();
}

void xpath_t::op_t::append_value(value_t& val,
				 value_t::sequence_t& result_seq)
{
  if (val.type == value_t::SEQUENCE)
    std::for_each(val.as_sequence().begin(), val.as_sequence().end(),
		  bind(&value_t::sequence_t::push_back, ref(result_seq), _1));
  else
    result_seq.push_back(val);
}

xpath_t::op_t * xpath_t::op_t::compile(value_t * context, scope_t * scope,
				       bool resolve)
{
#if 0
  try {
#endif
  switch (kind) {
  case VALUE:
    return acquire();

  case NODE_ID:
    switch (name_id) {
    case document_t::CURRENT:
      return wrap_value(context)->acquire();

    case document_t::PARENT:
      if (context->type != value_t::XML_NODE)
	throw_(compile_error, "Referencing parent node from a non-node value");
      else if (context->as_xml_node()->parent())
	return wrap_value(&*context->as_xml_node()->parent())->acquire();
      else
	throw_(compile_error, "Referencing parent node from the root node");

    case document_t::ROOT:
      if (context->type != value_t::XML_NODE)
	throw_(compile_error, "Referencing root node from a non-node value");
      else
	return wrap_value(&context->as_xml_node()->document())->acquire();

    case document_t::ALL: {
      if (context->type != value_t::XML_NODE)
	throw_(compile_error, "Referencing child nodes from a non-node value");

      value_t::sequence_t nodes;
      foreach (node_t * node, context->as_xml_node()->as_parent_node())
	nodes.push_back(node);

      return wrap_value(nodes)->acquire();
    }

    default:
      break;			// pass down to the NODE_NAME case
    }
    // fall through...

  case NODE_NAME:
    if (context->type == value_t::XML_NODE) {
      node_t * ptr = context->as_xml_node();
      if (resolve) {
	// First, look up the symbol as a node name within the current
	// context.  If any exist, then return the set of names.

	if (ptr->is_parent_node()) {
	  value_t::sequence_t nodes;

	  foreach (node_t * node, ptr->as_parent_node()) {
	    if ((kind == NODE_NAME &&
		 std::strcmp(name->c_str(), node->name()) == 0) ||
		(kind == NODE_ID && name_id == node->name_id()))
	      nodes.push_back(node);
	  }
	  return wrap_value(nodes)->acquire();
	}
      } else {
	assert(ptr);
	if (optional<node_t::nameid_t> id =
	    ptr->document().lookup_name_id(*name)) {
	  op_t * node = new_node(NODE_ID);
	  node->name_id = *id;
	  return node->acquire();
	}
      }
    }
    return acquire();

  case ATTR_NAME:
    if (optional<node_t::nameid_t> id =
	context->as_xml_node()->document().lookup_name_id(*name)) {
      optional<const string&> value = context->as_xml_node()->get_attr(*id);
      if (value)
	return wrap_value(*value)->acquire();
    }
    return acquire();

  case VAR_NAME:
  case FUNC_NAME:
    if (scope) {
      if (resolve) {
	value_t temp;
	if (scope->resolve(*name, temp))
	  return wrap_value(temp)->acquire();
      }
      if (op_t * def = scope->lookup(*name))
	return def->compile(context, scope, resolve);
    }
    return acquire();

  case ARG_INDEX:
    if (scope && scope->kind == scope_t::ARGUMENT) {
      assert(scope->args.type == value_t::SEQUENCE);
      if (arg_index < scope->args.as_sequence().size())
	return wrap_value(scope->args.as_sequence()[arg_index])->acquire();
      else
	throw_(compile_error, "Reference to non-existing argument");
    } else {
      return acquire();
    }

  case FUNCTOR:
    if (resolve) {
      value_t temp;
      (*functor)(temp, scope);
      return wrap_value(temp)->acquire();
    } else {
      return acquire();
    }
    break;

#if 0
  case MASK:
    return acquire();
#endif

  case O_NOT: {
    assert(left);
    xpath_t expr(left->compile(context, scope, resolve));
    if (! expr->constant()) {
      if (left == expr)
	return acquire();
      else
	return copy(expr)->acquire();
    }

    if (left == expr) {
      if (expr->valuep->strip_annotations())
	return wrap_value(false)->acquire();
      else
	return wrap_value(true)->acquire();
    } else {
      if (expr->valuep->strip_annotations())
	*expr->valuep = false;
      else
	*expr->valuep = true;

      return expr->acquire();
    }
  }

  case O_NEG: {
    assert(left);
    xpath_t expr(left->compile(context, scope, resolve));
    if (! expr->constant()) {
      if (left == expr)
	return acquire();
      else
	return copy(expr)->acquire();
    }

    if (left == expr) {
      return wrap_value(expr->valuep->negate())->acquire();
    } else {
      expr->valuep->in_place_negate();
      return expr->acquire();
    }
  }

  case O_UNION: {
    assert(left);
    assert(right);
    xpath_t lexpr(left->compile(context, scope, resolve));
    xpath_t rexpr(right->compile(context, scope, resolve));
    if (! lexpr->constant() || ! rexpr->constant()) {
      if (left == lexpr && right == rexpr)
	return acquire();
      else
	return copy(lexpr, rexpr)->acquire();
    }

    value_t::sequence_t result_seq;

    append_value(*lexpr->valuep, result_seq);
    append_value(*rexpr->valuep, result_seq);

    if (result_seq.size() == 1)
      return wrap_value(result_seq.front())->acquire();
    else
      return wrap_sequence(result_seq)->acquire();
    break;
  }

  case O_ADD:
  case O_SUB:
  case O_MUL:
  case O_DIV: {
    assert(left);
    assert(right);
    xpath_t lexpr(left->compile(context, scope, resolve));
    xpath_t rexpr(right->compile(context, scope, resolve));
    if (! lexpr->constant() || ! rexpr->constant()) {
      if (left == lexpr && right == rexpr)
	return acquire();
      else
	return copy(lexpr, rexpr)->acquire();
    }

    if (left == lexpr) {
      value_t temp(*lexpr->valuep);
      switch (kind) {
      case O_ADD: temp += *rexpr->valuep; break;
      case O_SUB: temp -= *rexpr->valuep; break;
      case O_MUL: temp *= *rexpr->valuep; break;
      case O_DIV: temp /= *rexpr->valuep; break;
      default: assert(false); break;
      }
      return wrap_value(temp)->acquire();
    } else {
      switch (kind) {
      case O_ADD: *lexpr->valuep += *rexpr->valuep; break;
      case O_SUB: *lexpr->valuep -= *rexpr->valuep; break;
      case O_MUL: *lexpr->valuep *= *rexpr->valuep; break;
      case O_DIV: *lexpr->valuep /= *rexpr->valuep; break;
      default: assert(false); break;
      }
      return lexpr->acquire();
    }
  }

  case O_NEQ:
  case O_EQ:
  case O_LT:
  case O_LTE:
  case O_GT:
  case O_GTE: {
    assert(left);
    assert(right);
    xpath_t lexpr(left->compile(context, scope, resolve));
    xpath_t rexpr(right->compile(context, scope, resolve));
    if (! lexpr->constant() || ! rexpr->constant()) {
      if (left == lexpr && right == rexpr)
	return acquire();
      else
	return copy(lexpr, rexpr)->acquire();
    }

    if (left == lexpr) {
      switch (kind) {
      case O_NEQ:
	return wrap_value(*lexpr->valuep != *rexpr->valuep)->acquire();
	break;
      case O_EQ:
	return wrap_value(*lexpr->valuep == *rexpr->valuep)->acquire();
	break;
      case O_LT:
	return wrap_value(*lexpr->valuep <  *rexpr->valuep)->acquire();
	break;
      case O_LTE:
	return wrap_value(*lexpr->valuep <= *rexpr->valuep)->acquire();
	break;
      case O_GT:
	return wrap_value(*lexpr->valuep >  *rexpr->valuep)->acquire();
	break;
      case O_GTE:
	return wrap_value(*lexpr->valuep >= *rexpr->valuep)->acquire();
	break;
      default: assert(false); break;
      }
    } else {
      switch (kind) {
      case O_NEQ: *lexpr->valuep = *lexpr->valuep != *rexpr->valuep; break;
      case O_EQ:  *lexpr->valuep = *lexpr->valuep == *rexpr->valuep; break;
      case O_LT:  *lexpr->valuep = *lexpr->valuep <  *rexpr->valuep; break;
      case O_LTE: *lexpr->valuep = *lexpr->valuep <= *rexpr->valuep; break;
      case O_GT:  *lexpr->valuep = *lexpr->valuep >  *rexpr->valuep; break;
      case O_GTE: *lexpr->valuep = *lexpr->valuep >= *rexpr->valuep; break;
      default: assert(false); break;
      }
      return lexpr->acquire();
    }
  }

  case O_AND: {
    assert(left);
    assert(right);
    xpath_t lexpr(left->compile(context, scope, resolve));
    if (lexpr->constant() && ! lexpr->valuep->strip_annotations()) {
      *lexpr->valuep = false;
      return lexpr->acquire();
    }

    xpath_t rexpr(right->compile(context, scope, resolve));
    if (! lexpr->constant() || ! rexpr->constant()) {
      if (left == lexpr && right == rexpr)
	return acquire();
      else
	return copy(lexpr, rexpr)->acquire();
    }

    if (! rexpr->valuep->strip_annotations()) {
      if (left == lexpr) {
	return wrap_value(false)->acquire();
      } else {
	*lexpr->valuep = false;
	return lexpr->acquire();
      }
    } else {
      return rexpr->acquire();
    }
  }

  case O_OR: {
    assert(left);
    assert(right);
    xpath_t lexpr(left->compile(context, scope, resolve));
    if (lexpr->constant() && lexpr->valuep->strip_annotations())
      return lexpr->acquire();

    xpath_t rexpr(right->compile(context, scope, resolve));
    if (! lexpr->constant() || ! rexpr->constant()) {
      if (left == lexpr && right == rexpr)
	return acquire();
      else
	return copy(lexpr, rexpr)->acquire();
    }

    if (rexpr->valuep->strip_annotations()) {
      return rexpr->acquire();
    } else {
      if (left == lexpr) {
	return wrap_value(false)->acquire();
      } else {
	*lexpr->valuep = false;
	return lexpr->acquire();
      }
    }
  }

  case O_QUES: {
    assert(left);
    assert(right);
    assert(right->kind == O_COLON);
    xpath_t lexpr(left->compile(context, scope, resolve));
    if (! lexpr->constant()) {
      xpath_t rexpr(right->compile(context, scope, resolve));
      if (left == lexpr && right == rexpr)
	return acquire();
      else
	return copy(lexpr, rexpr)->acquire();
    }

    if (lexpr->valuep->strip_annotations())
      return right->left->compile(context, scope, resolve);
    else
      return right->right->compile(context, scope, resolve);
  }

  case O_COLON: {
    xpath_t lexpr(left->compile(context, scope, resolve));
    xpath_t rexpr(right->compile(context, scope, resolve));
    if (left == lexpr && right == rexpr)
      return acquire();
    else
      return copy(lexpr, rexpr)->acquire();
  }

  case O_COMMA: {
    assert(left);
    assert(right);
    // jww (2006-09-29): This should act just like union
    xpath_t lexpr(left->compile(context, scope, resolve)); // for side-effects
    return right->compile(context, scope, resolve);
  }

#if 0
  case O_MATCH:
  case O_NMATCH: {
    assert(left);
    assert(right);
    xpath_t rexpr(right->compile(context, scope, resolve));
    xpath_t lexpr(left->compile(context, scope, resolve));
    if (! lexpr->constant() || rexpr->kind != MASK) {
      if (left == lexpr)
	return acquire();
      else
	return copy(lexpr, rexpr)->acquire();
    }

    if (lexpr->valuep->type != value_t::STRING)
      throw_(compile_error, "Left operand of mask operator is not a string");

    assert(rexpr->mask);

    bool result = rexpr->mask->match(lexpr->valuep->as_string());
    if (kind == O_NMATCH)
      result = ! result;

    if (left == lexpr) {
      return wrap_value(result)->acquire();
    } else {
      *lexpr->valuep = result;
      return lexpr->acquire();
    }
  }
#endif

  case O_DEFINE:
    assert(left);
    assert(right);
    if (left->kind == VAR_NAME || left->kind == FUNC_NAME) {
      xpath_t rexpr(right->compile(context, scope, resolve));
      if (scope)
	scope->define(*left->name, rexpr);
      return rexpr->acquire();
    } else {
      assert(left->kind == O_EVAL);
      assert(left->left->kind == FUNC_NAME);

      std::auto_ptr<scope_t> arg_scope(new scope_t(scope));

      int index = 0;
      op_t * args = left->right;
      while (args) {
	op_t * arg = args;
	if (args->kind == O_COMMA) {
	  arg = args->left;
	  args = args->right;
	} else {
	  args = NULL;
	}

	// Define the parameter so that on lookup the parser will find
	// an ARG_INDEX value.
	std::auto_ptr<op_t> ref(new op_t(ARG_INDEX));
	ref->arg_index = index++;

	assert(arg->kind == NODE_NAME);
	arg_scope->define(*arg->name, ref.release());
      }

      // jww (2006-09-16): If I compile the definition of a function,
      // I eliminate the possibility of future lookups
      //xpath_t rexpr(right->compile(arg_scope.get(), resolve));

      if (scope)
	scope->define(*left->left->name, right);

      return right->acquire();
    }

  case O_EVAL: {
    assert(left);

    std::auto_ptr<scope_t> call_args(new scope_t(scope));
    call_args->kind = scope_t::ARGUMENT;

    value_t::sequence_t call_seq;

    op_t * args = right;
    while (args) {
      op_t * arg = args;
      if (args->kind == O_COMMA) {
	arg = args->left;
	args = args->right;
      } else {
	args = NULL;
      }

      // jww (2006-09-15): Need to return a reference to these, if
      // there are undetermined arguments!
      call_seq.push_back(arg->compile(context, scope, resolve)->value());
    }

    call_args->args = call_seq;

    if (left->kind == FUNC_NAME) {
      if (resolve) {
	value_t temp;
	if (scope && scope->resolve(*left->name, temp, call_args.get()))
	  return wrap_value(temp)->acquire();
      }

      // Don't compile to the left, otherwise the function name may
      // get resolved before we have a chance to call it
      xpath_t func(left->compile(context, scope, false));
      if (func->kind == FUNCTOR) {
	value_t temp;
	(*func->functor)(temp, call_args.get());
	return wrap_value(temp)->acquire();
      }
      else if (! resolve) {
	return func->compile(context, call_args.get(), resolve);
      }
      else {
	throw_(calc_error, "Unknown function name '" << *left->name << "'");
      }
    }
    else if (left->kind == FUNCTOR) {
      value_t temp;
      (*left->functor)(temp, call_args.get());
      return wrap_value(temp)->acquire();
    }
    else {
      assert(false);
    }
    break;
  }

  case O_FIND:
  case O_RFIND:
  case O_PRED: {
    assert(left);
    assert(right);
    xpath_t lexpr(left->compile(context, scope, resolve));
    xpath_t rexpr(resolve ? right->acquire() :
		  right->compile(context, scope, false));

    if (! lexpr->constant() || ! resolve) {
      if (left == lexpr)
	return acquire();
      else
	return copy(lexpr, rexpr)->acquire();
    }

    value_t::sequence_t result_seq;

    // jww (2006-09-24): What about when nothing is found?
    switch (lexpr->valuep->type) {
    case value_t::XML_NODE: {
      function_scope_t xpath_fscope(lexpr->valuep, 0, scope);
      if (kind == O_PRED) {
	if (rexpr->test_value(lexpr->valuep, &xpath_fscope))
	  result_seq.push_back(*lexpr->valuep);
      } else {
	rexpr->find_values(lexpr->valuep, &xpath_fscope, result_seq,
			   kind == O_RFIND);
      }
      break;
    }

    case value_t::SEQUENCE: {
      value_t::sequence_t& seq(lexpr->valuep->as_sequence());

      int index = 0;
      for (value_t::sequence_t::iterator i = seq.begin();
	   i != seq.end();
	   i++, index++) {
	assert((*i).type != value_t::SEQUENCE);
	if ((*i).type != value_t::XML_NODE)
	  throw_(compile_error, "Attempting to apply path selection "
				  "to non-node(s)");

	function_scope_t xpath_fscope(seq, &(*i), index, scope);
	if (kind == O_PRED) {
	  if (rexpr->test_value(&(*i), &xpath_fscope, index))
	    result_seq.push_back(*i);
	} else {
	  rexpr->find_values(&(*i), &xpath_fscope, result_seq,
			     kind == O_RFIND);
	}
      }
      break;
    }

    default:
      throw_(compile_error, "Attempting to apply path selection "
	     "to non-node(s)");
    }

    if (result_seq.size() == 1)
      return wrap_value(result_seq.front())->acquire();
    else
      return wrap_sequence(result_seq)->acquire();
  }

#if 0
  case O_PERC: {
    assert(left);
    xpath_t expr(left->compile(context, scope, resolve));
    if (! expr->constant()) {
      if (left == expr)
	return acquire();
      else
	return copy(expr)->acquire();
    }

    static value_t perc("100.0%");
    *expr->valuep = perc * *expr->valuep;
    return expr->acquire();
  }
#endif

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

void xpath_t::calc(value_t& result, node_t& node, scope_t * scope) const
{
#if 0
  try {
#endif
    value_t context_node(&node);
    xpath_t final(ptr->compile(&context_node, scope, true));
    // jww (2006-09-09): Give a better error here if this is not
    // actually a value
    final->get_value(result);
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
xpath_t::context::context(const xpath_t& _xpath,
			  const op_t *   _err_node,
			  const string&  desc) throw()
  : error_context(desc), xpath(_xpath), err_node(_err_node)
{
  _err_node->acquire();
}

xpath_t::context::~context() throw()
{
  if (err_node) err_node->release();
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
			  const bool      relaxed,
			  const op_t *    op_to_find,
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
  case VALUE:
    switch (valuep->type) {
    case value_t::BOOLEAN:
      if (*(valuep))
	out << "1";
      else
	out << "0";
      break;
    case value_t::INTEGER:
    case value_t::AMOUNT:
      if (! relaxed)
	out << '{';
      out << *(valuep);
      if (! relaxed)
	out << '}';
      break;
    case value_t::BALANCE:
    case value_t::BALANCE_PAIR:
      assert(false);
      break;
    case value_t::DATETIME:
      out << '[' << *valuep << ']';
      break;
    case value_t::STRING:
      out << '"' << *valuep << '"';
      break;

    case value_t::XML_NODE:
      out << '<' << valuep << '>';
      break;
    case value_t::POINTER:
      out << '&' << valuep;
      break;
    case value_t::SEQUENCE:
      out << '~' << valuep << '~';
      break;
    }
    break;

  case NODE_ID:
    out << '%' << name_id;
    break;

  case NODE_NAME:
  case FUNC_NAME:
    out << *name;
    break;

  case ATTR_NAME:
    out << '@' << *name;
    break;

  case VAR_NAME:
    out << '$' << *name;
    break;

  case FUNCTOR:
    out << functor->name();
    break;

#if 0
  case MASK:
    out << '/' << mask->pattern << '/';
    break;
#endif

  case ARG_INDEX:
    out << '@' << arg_index;
    break;

  case O_NOT:
    out << "!";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_NEG:
    out << "-";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_UNION:
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " | ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_ADD:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " + ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_SUB:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " - ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_MUL:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " * ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_DIV:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " / ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_NEQ:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " != ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_EQ:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " == ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_LT:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " < ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_LTE:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " <= ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_GT:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " > ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_GTE:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " >= ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_AND:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " & ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_OR:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " | ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_QUES:
    out << "(";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " ? ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_COLON:
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " : ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_COMMA:
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ", ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

#if 0
  case O_MATCH:
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " =~ ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_NMATCH:
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " !~ ";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
#endif

  case O_DEFINE:
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << '=';
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_EVAL:
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "(";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_FIND:
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "/";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_RFIND:
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "//";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_PRED:
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "[";
    if (right && right->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "]";
    break;

#if 0
  case O_PERC:
    out << "%";
    if (left && left->print(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
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
    out << "VALUE - " << *valuep;
    break;

  case NODE_NAME:
    out << "NODE_NAME - " << *name;
    break;

  case NODE_ID:
    out << "NODE_ID - " << name_id;
    break;

  case ATTR_NAME:
    out << "ATTR_NAME - " << *name;
    break;

  case FUNC_NAME:
    out << "FUNC_NAME - " << *name;
    break;

  case VAR_NAME:
    out << "VAR_NAME - " << *name;
    break;

  case ARG_INDEX:
    out << "ARG_INDEX - " << arg_index;
    break;

  case FUNCTOR:
    out << "FUNCTOR - " << functor->name();
    break;
#if 0
  case MASK:
    out << "MASK - " << mask->pattern;
    break;
#endif

  case O_NOT: out << "O_NOT"; break;
  case O_NEG: out << "O_NEG"; break;

  case O_UNION: out << "O_UNION"; break;

  case O_ADD: out << "O_ADD"; break;
  case O_SUB: out << "O_SUB"; break;
  case O_MUL: out << "O_MUL"; break;
  case O_DIV: out << "O_DIV"; break;

  case O_NEQ: out << "O_NEQ"; break;
  case O_EQ: out << "O_EQ"; break;
  case O_LT: out << "O_LT"; break;
  case O_LTE: out << "O_LTE"; break;
  case O_GT: out << "O_GT"; break;
  case O_GTE: out << "O_GTE"; break;

  case O_AND: out << "O_AND"; break;
  case O_OR: out << "O_OR"; break;

  case O_QUES: out << "O_QUES"; break;
  case O_COLON: out << "O_COLON"; break;

  case O_COMMA: out << "O_COMMA"; break;

#if 0
  case O_MATCH: out << "O_MATCH"; break;
  case O_NMATCH: out << "O_NMATCH"; break;
#endif

  case O_DEFINE: out << "O_DEFINE"; break;
  case O_EVAL: out << "O_EVAL"; break;

  case O_FIND: out << "O_FIND"; break;
  case O_RFIND: out << "O_RFIND"; break;
  case O_PRED: out << "O_PRED"; break;

#if 0
  case O_PERC: out << "O_PERC"; break;
#endif

  case LAST:
  default:
    assert(false);
    break;
  }

  out << " (" << refc << ')' << std::endl;

  if (kind > TERMINALS) {
    if (left) {
      left->dump(out, depth + 1);
      if (right)
	right->dump(out, depth + 1);
    } else {
      assert(! right);
    }
  } else {
    assert(! left);
  }
}

} // namespace xml
} // namespace ledger
