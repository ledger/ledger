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
#if 0
  case 'u':
    if (std::strcmp(buf, "union") == 0)
      kind = KW_UNION;
    break;
#endif
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
    if (flags & EXPR_PARSE_ALLOW_DATE) {
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

#if 0
  case '|':
    in.get(c);
    kind = PIPE;
    break;
#endif
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
      try {
	pos = (long)in.tellg();

	unsigned char parse_flags = 0;
	if (flags & EXPR_PARSE_NO_MIGRATE)
	  parse_flags |= AMOUNT_PARSE_NO_MIGRATE;
	if (flags & EXPR_PARSE_NO_REDUCE)
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
#if 0
    if (tok.kind == token_t::LPAREN) {
      node = new op_t(op_t::FUNC_NAME);
      node->set_string(ident);

      ptr_op_t call_node(new op_t(op_t::O_CALL));
      call_node->set_left(node);
      call_node->set_right(parse_value_expr(in, scope, tflags | EXPR_PARSE_PARTIAL));

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
#endif
    break;
  }

  case token_t::AT_SYM: {
    tok = next_token(in, tflags);
    if (tok.kind != token_t::IDENT)
      throw_(parse_error, "@ symbol must be followed by attribute name");

#if 0
    string ident = tok.value.as_string();
    if (optional<node_t::nameid_t> id = document_t::lookup_builtin_id(ident)) {
      node = new op_t(op_t::ATTR_ID);
      node->set_name(*id);
    }
    else {
      node = new op_t(op_t::ATTR_NAME);
      node->set_string(ident);
    }
#endif
    break;
  }

#if 0
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
#endif

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
parser_t::parse_unary_expr(std::istream& in, scope_t& scope, const flags_t tflags) const
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
      term->as_value().in_place_negate();
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
      term->as_value().in_place_negate();
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

#if 0
ptr_op_t
parser_t::parse_union_expr(std::istream& in, scope_t& scope, const flags_t tflags) const
{
  ptr_op_t node(parse_unary_expr(in, scope, tflags));

  if (node) {
    token_t& tok = next_token(in, tflags);
    if (tok.kind == token_t::PIPE || tok.kind == token_t::KW_UNION) {
      ptr_op_t prev(node);
      node = new op_t(op_t::O_UNION);
      node->set_left(prev);
      node->set_right(parse_union_expr(in, scope, tflags));
      if (! node->right())
	throw_(parse_error,
	       tok.symbol << " operator not followed by argument");
    } else {
      push_token(tok);
    }
  }

  return node;
}
#endif

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

#if 0
bool print(std::ostream& out, op_t::print_context_t& context) const
{
  if (ptr)
    ptr->print(out, context);
  return true;
}
#endif

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

#if 0
    case value_t::XML_NODE:
      out << '<' << value << '>';
      break;
#endif
    case value_t::POINTER:
      out << '&' << value;
      break;
    case value_t::SEQUENCE:
      out << '~' << value << '~';
      break;
    }
    break;
  }

#if 0
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
#endif

#if 0
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

#if 0
  case O_UNION:
    if (left() && left()->print(out, context))
      found = true;
    out << " | ";
    if (right() && right()->print(out, context))
      found = true;
    break;
#endif

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

#if 0
  case O_UNION:	 out << "O_UNION"; break;
#endif

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

#if 0
  case O_FIND:	 out << "O_FIND"; break;
  case O_RFIND:	 out << "O_RFIND"; break;
  case O_PRED:	 out << "O_PRED"; break;
#endif

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

#if 0
ptr_op_t parse_value_term(std::istream& in, scope_t * scope,
				const short flags)
{
  value_expr node;

  char buf[256];
  char c = peek_next_nonws(in);

  if (flags & PARSE_VALEXPR_RELAXED) {
    if (c == '@') {
      in.get(c);
      c = peek_next_nonws(in);
    }
    else if (! (c == '(' || c == '[' || c == '{' || c == '/')) {
      amount_t temp;
      char prev_c = c;
      unsigned long pos = 0;
      // When in relaxed parsing mode, we do want to migrate commodity
      // flags, so that any precision specified by the user updates
      // the current maximum precision displayed.
      try {
	pos = (long)in.tellg();

	unsigned char parse_flags = 0;
	if (flags & PARSE_VALEXPR_NO_MIGRATE)
	  parse_flags |= AMOUNT_PARSE_NO_MIGRATE;
	if (flags & PARSE_VALEXPR_NO_REDUCE)
	  parse_flags |= AMOUNT_PARSE_NO_REDUCE;

	temp.parse(in, parse_flags);
      }
      catch (amount_error * err) {
	// If the amount had no commodity, it must be an unambiguous
	// variable reference
	if (std::strcmp(err->what(), "No quantity specified for amount") == 0) {
	  in.clear();
	  in.seekg(pos, std::ios::beg);
	  c = prev_c;
	  goto parse_ident;
	} else {
	  throw err;
	}
      }
      node.reset(new op_t(op_t::VALUE));
      node->set_value(temp);
      goto parsed;
    }
  }

 parse_ident:
  if (std::isdigit(c) || c == '.') {
    READ_INTO(in, buf, 255, c, std::isdigit(c) || c == '.');
    amount_t temp;
    temp.parse(buf, AMOUNT_PARSE_NO_MIGRATE);
    node.reset(new op_t(op_t::VALUE));
    node->set_value(temp);
    goto parsed;
  }
  else if (std::isalnum(c) || c == '_') {
    bool have_args = false;
    istream_pos_type beg;

    READ_INTO(in, buf, 255, c, std::isalnum(c) || c == '_');
    c = peek_next_nonws(in);
    if (c == '(') {
      in.get(c);
      beg = in.tellg();

      int paren_depth = 0;
      while (! in.eof()) {
	in.get(c);
	if (c == '(' || c == '{' || c == '[')
	  paren_depth++;
	else if (c == ')' || c == '}' || c == ']') {
	  if (paren_depth == 0)
	    break;
	  paren_depth--;
	}
      }
      if (c != ')')
	unexpected(c, ')');

      have_args = true;
      c = peek_next_nonws(in);
    }

    bool definition = false;
    if (c == '=') {
      in.get(c);
      if (peek_next_nonws(in) == '=') {
	in.unget();
	c = '\0';
      } else {
	definition = true;
      }
    }

    if (definition) {
      std::auto_ptr<call_scope_t> params(new call_scope_t(*scope));

      long arg_index = 0;
      if (have_args) {
	bool done = false;

	in.clear();
	in.seekg(beg, std::ios::beg);
	while (! done && ! in.eof()) {
	  char ident[32];
	  READ_INTO(in, ident, 31, c, std::isalnum(c) || c == '_');

	  c = peek_next_nonws(in);
	  in.get(c);
	  if (c != ',' && c != ')')
	    unexpected(c, ')');
	  else if (c == ')')
	    done = true;

	  // Define the parameter so that on lookup the parser will find
	  // an O_ARG value.
	  node.reset(new op_t(op_t::O_ARG));
	  node->set_left(new op_t(op_t::ARG_INDEX));
	  node->left()->set_long(arg_index++);
	  params->define(ident, node.release());
	}

	if (peek_next_nonws(in) != '=') {
	  in.get(c);
	  unexpected(c, '=');
	}
	in.get(c);
      }

#if 0
      // Define the value associated with the defined identifier
      value_expr def(parse_boolean_expr(in, scope, params.get(), flags));
      if (! def.get())
	throw new value_expr_error(string("Definition failed for '") + buf + "'");

      node.reset(new op_t(op_t::O_DEF));
      node->set_left(new op_t(op_t::ARG_INDEX));
      node->left()->set_long(arg_index);
      node->set_right(def.release());
#endif

      scope->define(buf, node.get());
    } else {
      assert(scope);
      ptr_op_t def = scope->lookup(buf);
      if (! def) {
	if (buf[1] == '\0' &&
	    (buf[0] == 'c' || buf[0] == 'C' || buf[0] == 'p' ||
	     buf[0] == 'w' || buf[0] == 'W' || buf[0] == 'e')) {
	  in.unget();
	  goto find_term;
	}
	throw new value_expr_error(string("Unknown identifier '") +
				   buf + "'");
      }
      else if (def->kind == op_t::O_DEF) {
	node.reset(new op_t(op_t::O_REF));
	node->set_left(def->right());

	unsigned int count = 0;
	if (have_args) {
	  in.clear();
	  in.seekg(beg, std::ios::beg);
	  value_expr args
	    (parse_value_expr(in, scope, flags | PARSE_VALEXPR_PARTIAL));

	  if (peek_next_nonws(in) != ')') {
	    in.get(c);
	    unexpected(c, ')');
	  }
	  in.get(c);

	  if (args.get()) {
	    count = count_leaves(args.get());
	    node->set_right(args.release());
	  }
	}

	if (count != def->left()->as_long()) {
	  std::ostringstream errmsg;
	  errmsg << "Wrong number of arguments to '" << buf
		 << "': saw " << count
		 << ", wanted " << def->left()->as_long();
	  throw new value_expr_error(errmsg.str());
	}
      }
      else {
	node.reset(def);
      }
    }
    goto parsed;
  }

 find_term:
  in.get(c);
  switch (c) {
  // Functions
  case '^':
    node.reset(new op_t(op_t::F_PARENT));
    node->set_left(parse_value_term(in, scope, flags));
    break;

  // Other
#if 0
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
    READ_INTO(in, buf, 255, c, c != '/');
    if (c != '/')
      unexpected(c, '/');

    op_t::kind_t kind;

    if (short_account_mask)
      kind = op_t::F_SHORT_ACCOUNT_MASK;
    else if (code_mask)
      kind = op_t::F_CODE_MASK;
    else if (commodity_mask)
      kind = op_t::F_COMMODITY_MASK;
    else if (payee_mask)
      kind = op_t::F_PAYEE_MASK;
    else if (note_mask)
      kind = op_t::F_NOTE_MASK;
    else
      kind = op_t::F_ACCOUNT_MASK;

    in.get(c);
    node.reset(new op_t(kind));
    node->mask = new mask_t(buf);
    break;
  }
#endif

  case '{': {
    amount_t temp;
    temp.parse(in, AMOUNT_PARSE_NO_MIGRATE);
    in.get(c);
    if (c != '}')
      unexpected(c, '}');

    node.reset(new op_t(op_t::VALUE));
    node->set_value(temp);
    break;
  }

  case '(': {
    std::auto_ptr<symbol_scope_t> locals(new symbol_scope_t(*scope));
    node.reset(parse_value_expr(in, locals.get(),
				flags | PARSE_VALEXPR_PARTIAL));
    in.get(c);
    if (c != ')')
      unexpected(c, ')');
    break;
  }

  case '[': {
    READ_INTO(in, buf, 255, c, c != ']');
    if (c != ']')
      unexpected(c, ']');
    in.get(c);

    interval_t timespan(buf);
    node.reset(new op_t(op_t::VALUE));
    node->set_value(timespan.first());
    break;
  }

  default:
    in.unget();
    break;
  }

 parsed:
  return node.release();
}

void init_value_expr()
{
  global_scope.reset(new symbol_scope_t());
  symbol_scope_t * globals = global_scope.get();

  ptr_op_t node;

  // Basic terms
  node = new op_t(op_t::F_NOW);
  globals->define("m", node);
  globals->define("now", node);
  globals->define("today", node);

  node = new op_t(op_t::AMOUNT);
  globals->define("a", node);
  globals->define("amount", node);

  node = new op_t(op_t::PRICE);
  globals->define("i", node);
  globals->define("price", node);

  node = new op_t(op_t::COST);
  globals->define("b", node);
  globals->define("cost", node);

  node = new op_t(op_t::DATE);
  globals->define("d", node);
  globals->define("date", node);

  node = new op_t(op_t::ACT_DATE);
  globals->define("act_date", node);
  globals->define("actual_date", node);

  node = new op_t(op_t::EFF_DATE);
  globals->define("eff_date", node);
  globals->define("effective_date", node);

  node = new op_t(op_t::CLEARED);
  globals->define("X", node);
  globals->define("cleared", node);

  node = new op_t(op_t::PENDING);
  globals->define("Y", node);
  globals->define("pending", node);

  node = new op_t(op_t::REAL);
  globals->define("R", node);
  globals->define("real", node);

  node = new op_t(op_t::ACTUAL);
  globals->define("L", node);
  globals->define("actual", node);

  node = new op_t(op_t::INDEX);
  globals->define("n", node);
  globals->define("index", node);

  node = new op_t(op_t::COUNT);
  globals->define("N", node);
  globals->define("count", node);

  node = new op_t(op_t::DEPTH);
  globals->define("l", node);
  globals->define("depth", node);

  node = new op_t(op_t::TOTAL);
  globals->define("O", node);
  globals->define("total", node);

  node = new op_t(op_t::PRICE_TOTAL);
  globals->define("I", node);
  globals->define("total_price", node);

  node = new op_t(op_t::COST_TOTAL);
  globals->define("B", node);
  globals->define("total_cost", node);

  // Relating to format_t
  globals->define("t", ptr_op_t(new op_t(op_t::VALUE_EXPR)));
  globals->define("T", ptr_op_t(new op_t(op_t::TOTAL_EXPR)));

  // Functions
  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(1);
  node->set_right(new op_t(op_t::F_ABS));
  globals->define("U", node);
  globals->define("abs", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(1);
  node->set_right(new op_t(op_t::F_ROUND));
  globals->define("round", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(1);
  node->set_right(new op_t(op_t::F_QUANTITY));
  globals->define("S", node);
  globals->define("quant", node);
  globals->define("quantity", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(1);
  node->set_right(new op_t(op_t::F_COMMODITY));
  globals->define("comm", node);
  globals->define("commodity", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(2);
  node->set_right(new op_t(op_t::F_SET_COMMODITY));
  globals->define("setcomm", node);
  globals->define("set_commodity", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(1);
  node->set_right(new op_t(op_t::F_ARITH_MEAN));
  globals->define("A", node);
  globals->define("avg", node);
  globals->define("mean", node);
  globals->define("average", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(2);
  node->set_right(new op_t(op_t::F_VALUE));
  globals->define("P", node);

  parse_value_definition("@value=@P(@t,@m)", globals);
  parse_value_definition("@total_value=@P(@T,@m)", globals);
  parse_value_definition("@valueof(x)=@P(@x,@m)", globals);
  parse_value_definition("@datedvalueof(x,y)=@P(@x,@y)", globals);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(1);
  node->set_right(new op_t(op_t::F_PRICE));
  globals->define("priceof", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(1);
  node->set_right(new op_t(op_t::F_DATE));
  globals->define("dateof", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(2);
  node->set_right(new op_t(op_t::F_DATECMP));
  globals->define("datecmp", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(1);
  node->set_right(new op_t(op_t::F_YEAR));
  globals->define("yearof", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(1);
  node->set_right(new op_t(op_t::F_MONTH));
  globals->define("monthof", node);

  node = new op_t(op_t::O_DEF);
  node->set_left(new op_t(op_t::ARG_INDEX));
  node->left()->set_long(1);
  node->set_right(new op_t(op_t::F_DAY));
  globals->define("dayof", node);

  parse_value_definition("@year=@yearof(@d)", globals);
  parse_value_definition("@month=@monthof(@d)", globals);
  parse_value_definition("@day=@dayof(@d)", globals);

  // Macros
  node = parse_value_expr("@P(@a,@d)");
  globals->define("v", node);
  globals->define("market", node);

  node = parse_value_expr("@P(@O,@d)");
  globals->define("V", node);
  globals->define("total_market", node);

  node = parse_value_expr("@v-@b");
  globals->define("g", node);
  globals->define("gain", node);

  node = parse_value_expr("@V-@B");
  globals->define("G", node);
  globals->define("total_gain", node);

  parse_value_definition("@min(x,y)=@x<@y?@x:@y", globals);
  parse_value_definition("@max(x,y)=@x>@y?@x:@y", globals);
}

bool print_value_expr(std::ostream&   out,
		      const ptr_op_t  node,
		      const bool      relaxed,
		      const ptr_op_t  op_to_find,
		      unsigned long * start_pos,
		      unsigned long * end_pos)
{
  bool found = false;

  if (start_pos && node == op_to_find) {
    *start_pos = (long)out.tellp() - 1;
    found = true;
  }

  string symbol;

  switch (node->kind) {
  case op_t::ARG_INDEX:
    out << node->as_long();
    break;

  case op_t::VALUE:
    switch (node->as_value().type()) {
    case value_t::BOOLEAN:
      assert(false);
      break;
    case value_t::DATETIME:
      out << '[' << node->as_value().as_datetime() << ']';
      break;
    case value_t::INTEGER:
    case value_t::AMOUNT:
      if (! relaxed)
	out << '{';
      out << node->as_value();
      if (! relaxed)
	out << '}';
      break;
    //case value_t::BALANCE:
    //case value_t::BALANCE_PAIR:
    default:
      assert(false);
      break;
    }
    break;

  case op_t::AMOUNT:
    symbol = "amount"; break;
  case op_t::PRICE:
    symbol = "price"; break;
  case op_t::COST:
    symbol = "cost"; break;
  case op_t::DATE:
    symbol = "date"; break;
  case op_t::ACT_DATE:
    symbol = "actual_date"; break;
  case op_t::EFF_DATE:
    symbol = "effective_date"; break;
  case op_t::CLEARED:
    symbol = "cleared"; break;
  case op_t::PENDING:
    symbol = "pending"; break;
  case op_t::REAL:
    symbol = "real"; break;
  case op_t::ACTUAL:
    symbol = "actual"; break;
  case op_t::INDEX:
    symbol = "index"; break;
  case op_t::COUNT:
    symbol = "count"; break;
  case op_t::DEPTH:
    symbol = "depth"; break;
  case op_t::TOTAL:
    symbol = "total"; break;
  case op_t::PRICE_TOTAL:
    symbol = "total_price"; break;
  case op_t::COST_TOTAL:
    symbol = "total_cost"; break;
  case op_t::F_NOW:
    symbol = "now"; break;

  case op_t::VALUE_EXPR:
    if (print_value_expr(out, amount_expr.get(), relaxed,
			 op_to_find, start_pos, end_pos))
      found = true;
    break;
  case op_t::TOTAL_EXPR:
    if (print_value_expr(out, total_expr.get(), relaxed,
			 op_to_find, start_pos, end_pos))
      found = true;
    break;

  case op_t::F_ARITH_MEAN:
    symbol = "average"; break;
  case op_t::F_ABS:
    symbol = "abs"; break;
  case op_t::F_QUANTITY:
    symbol = "quantity"; break;
  case op_t::F_COMMODITY:
    symbol = "commodity"; break;
  case op_t::F_SET_COMMODITY:
    symbol = "set_commodity"; break;
  case op_t::F_VALUE:
    symbol = "valueof"; break;
  case op_t::F_PRICE:
    symbol = "priceof"; break;
  case op_t::F_DATE:
    symbol = "dateof"; break;
  case op_t::F_DATECMP:
    symbol = "datecmp"; break;
  case op_t::F_YEAR:
    symbol = "yearof"; break;
  case op_t::F_MONTH:
    symbol = "monthof"; break;
  case op_t::F_DAY:
    symbol = "dayof"; break;

#if 0
  case op_t::F_CODE_MASK:
    out << "c/" << node->mask->expr.str() << "/";
    break;
  case op_t::F_PAYEE_MASK:
    out << "p/" << node->mask->expr.str() << "/";
    break;
  case op_t::F_NOTE_MASK:
    out << "e/" << node->mask->expr.str() << "/";
    break;
  case op_t::F_ACCOUNT_MASK:
    out << "W/" << node->mask->expr.str() << "/";
    break;
  case op_t::F_SHORT_ACCOUNT_MASK:
    out << "w/" << node->mask->expr.str() << "/";
    break;
  case op_t::F_COMMODITY_MASK:
    out << "C/" << node->mask->expr.str() << "/";
    break;
#endif

  case op_t::O_NOT:
    out << "!";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case op_t::O_NEG:
    out << "-";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case op_t::O_PERC:
    out << "%";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case op_t::O_ARG:
    out << "@arg" << node->as_long();
    break;
  case op_t::O_DEF:
    out << "<def args=\"";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "\" value=\"";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "\">";
    break;

  case op_t::O_REF:
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    if (node->right()) {
      out << "(";
      if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
	found = true;
      out << ")";
    }
    break;

  case op_t::O_COM:
    if (node->left() &&
	print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ", ";
    if (node->right() &&
	print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case op_t::O_QUES:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " ? ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case op_t::O_COL:
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " : ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case op_t::O_AND:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " & ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case op_t::O_OR:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " | ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case op_t::O_NEQ:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " != ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case op_t::O_EQ:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " == ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case op_t::O_LT:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " < ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case op_t::O_LTE:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " <= ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case op_t::O_GT:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " > ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case op_t::O_GTE:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " >= ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case op_t::O_ADD:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " + ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case op_t::O_SUB:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " - ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case op_t::O_MUL:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " * ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case op_t::O_DIV:
    out << "(";
    if (print_value_expr(out, node->left(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " / ";
    if (print_value_expr(out, node->right(), relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case op_t::LAST:
  default:
    assert(false);
    break;
  }

  if (! symbol.empty()) {
    if (amount_t::current_pool->find(symbol))
      out << '@';
    out << symbol;
  }

  if (end_pos && node == op_to_find)
    *end_pos = (long)out.tellp() - 1;

  return found;
}

void dump_value_expr(std::ostream& out, const ptr_op_t node,
		     const int depth)
{
  out.setf(std::ios::left);
  out.width(10);
  out << node << " ";

  for (int i = 0; i < depth; i++)
    out << " ";

  switch (node->kind) {
  case op_t::ARG_INDEX:
    out << "ARG_INDEX - " << node->as_long();
    break;
  case op_t::VALUE:
    out << "VALUE - " << node->as_value();
    break;

  case op_t::AMOUNT: out << "AMOUNT"; break;
  case op_t::PRICE: out << "PRICE"; break;
  case op_t::COST: out << "COST"; break;
  case op_t::DATE: out << "DATE"; break;
  case op_t::ACT_DATE: out << "ACT_DATE"; break;
  case op_t::EFF_DATE: out << "EFF_DATE"; break;
  case op_t::CLEARED: out << "CLEARED"; break;
  case op_t::PENDING: out << "PENDING"; break;
  case op_t::REAL: out << "REAL"; break;
  case op_t::ACTUAL: out << "ACTUAL"; break;
  case op_t::INDEX: out << "INDEX"; break;
  case op_t::COUNT: out << "COUNT"; break;
  case op_t::DEPTH: out << "DEPTH"; break;
  case op_t::TOTAL: out << "TOTAL"; break;
  case op_t::PRICE_TOTAL: out << "PRICE_TOTAL"; break;
  case op_t::COST_TOTAL: out << "COST_TOTAL"; break;

  case op_t::VALUE_EXPR: out << "VALUE_EXPR"; break;
  case op_t::TOTAL_EXPR: out << "TOTAL_EXPR"; break;

  case op_t::F_NOW: out << "F_NOW"; break;
  case op_t::F_ARITH_MEAN: out << "F_ARITH_MEAN"; break;
  case op_t::F_ABS: out << "F_ABS"; break;
  case op_t::F_QUANTITY: out << "F_QUANTITY"; break;
  case op_t::F_COMMODITY: out << "F_COMMODITY"; break;
  case op_t::F_SET_COMMODITY: out << "F_SET_COMMODITY"; break;
  case op_t::F_CODE_MASK: out << "F_CODE_MASK"; break;
  case op_t::F_PAYEE_MASK: out << "F_PAYEE_MASK"; break;
  case op_t::F_NOTE_MASK: out << "F_NOTE_MASK"; break;
  case op_t::F_ACCOUNT_MASK:
    out << "F_ACCOUNT_MASK"; break;
  case op_t::F_SHORT_ACCOUNT_MASK:
    out << "F_SHORT_ACCOUNT_MASK"; break;
  case op_t::F_COMMODITY_MASK:
    out << "F_COMMODITY_MASK"; break;
  case op_t::F_VALUE: out << "F_VALUE"; break;
  case op_t::F_PRICE: out << "F_PRICE"; break;
  case op_t::F_DATE: out << "F_DATE"; break;
  case op_t::F_DATECMP: out << "F_DATECMP"; break;
  case op_t::F_YEAR: out << "F_YEAR"; break;
  case op_t::F_MONTH: out << "F_MONTH"; break;
  case op_t::F_DAY: out << "F_DAY"; break;

  case op_t::O_NOT: out << "O_NOT"; break;
  case op_t::O_ARG: out << "O_ARG"; break;
  case op_t::O_DEF: out << "O_DEF"; break;
  case op_t::O_REF: out << "O_REF"; break;
  case op_t::O_COM: out << "O_COM"; break;
  case op_t::O_QUES: out << "O_QUES"; break;
  case op_t::O_COL: out << "O_COL"; break;
  case op_t::O_AND: out << "O_AND"; break;
  case op_t::O_OR: out << "O_OR"; break;
  case op_t::O_NEQ: out << "O_NEQ"; break;
  case op_t::O_EQ: out << "O_EQ"; break;
  case op_t::O_LT: out << "O_LT"; break;
  case op_t::O_LTE: out << "O_LTE"; break;
  case op_t::O_GT: out << "O_GT"; break;
  case op_t::O_GTE: out << "O_GTE"; break;
  case op_t::O_NEG: out << "O_NEG"; break;
  case op_t::O_ADD: out << "O_ADD"; break;
  case op_t::O_SUB: out << "O_SUB"; break;
  case op_t::O_MUL: out << "O_MUL"; break;
  case op_t::O_DIV: out << "O_DIV"; break;
  case op_t::O_PERC: out << "O_PERC"; break;

  case op_t::LAST:
  default:
    assert(false);
    break;
  }

  out << " (" << node->refc << ')' << std::endl;

  if (node->kind > op_t::TERMINALS) {
    dump_value_expr(out, node->left(), depth + 1);
    if (node->right())
      dump_value_expr(out, node->right(), depth + 1);
  }
}

#endif

} // namespace expr
} // namespace ledger
