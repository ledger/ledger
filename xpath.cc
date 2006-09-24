#ifdef USE_PCH
#include "pch.h"
#else
#include "xpath.h"
#include "debug.h"
#include "util.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif
#endif

namespace xml {

#ifndef THREADED
xpath_t::token_t xpath_t::lookahead;
#endif

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

  if (c == '@') {
    in.get(c);
    length = 1;
  } else {
    length = 0;
  }

  char buf[256];
  READ_INTO_(in, buf, 255, c, length,
	     std::isalnum(c) || c == '_' || c == '.');

  kind = IDENT;
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

  if (c == '@' || (! (flags & XPATH_PARSE_RELAXED) &&
		   (std::isalpha(c) || c == '_'))) {
    parse_ident(in);
    return;
  }

  switch (c) {
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
  case '%':
    in.get(c);
    kind = PERCENT;
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
      catch (amount_error * err) {
	// If the amount had no commodity, it must be an unambiguous
	// variable reference
	if (std::strcmp(err->what(), "No quantity specified for amount") == 0) {
	  in.clear();
	  in.seekg(pos, std::ios::beg);

	  c = in.peek();
	  assert(! (std::isdigit(c) || c == '.'));
	  parse_ident(in);

	  switch ((**(std::string **) value.data)[0]) {
	  case 'a':
	    if (**(std::string **) value.data == "and")
	      kind = AMPER;
	    break;
	  case 'd':
	    if (**(std::string **) value.data == "div")
	      kind = SLASH;
	    break;
	  case 'f':
	    if (**(std::string **) value.data == "false") {
	      kind = VALUE;
	      value = false;
	    }
	    break;
	  case 'o':
	    if (**(std::string **) value.data == "or")
	      kind = PIPE;
	    break;
	  case 't':
	    if (**(std::string **) value.data == "true") {
	      kind = VALUE;
	      value = true;
	    }
	    break;
	  }
	} else {
	  throw err;
	}
      }
    }
    break;
  }
}

void xpath_t::token_t::rewind(std::istream& in)
{
  for (int i = 0; i < length; i++)
    in.unget();
}


void xpath_t::token_t::unexpected()
{
  switch (kind) {
  case TOK_EOF:
    throw new parse_error("Unexpected end of expression");
  case IDENT:
    throw new parse_error(std::string("Unexpected symbol '") +
			  value.to_string() + "'");
  case VALUE:
    throw new parse_error(std::string("Unexpected value '") +
			  value.to_string() + "'");
  default:
    throw new parse_error(std::string("Unexpected operator '") + symbol + "'");
  }
}

void xpath_t::token_t::unexpected(char c, char wanted)
{
  if ((unsigned char) c == 0xff) {
    if (wanted)
      throw new parse_error(std::string("Missing '") + wanted + "'");
    else
      throw new parse_error("Unexpected end");
  } else {
    if (wanted)
      throw new parse_error(std::string("Invalid char '") + c +
			    "' (wanted '" + wanted + "')");
    else
      throw new parse_error(std::string("Invalid char '") + c + "'");
  }
}

xpath_t::op_t * xpath_t::wrap_value(const value_t& val)
{
  xpath_t::op_t * temp = new xpath_t::op_t(xpath_t::op_t::VALUE);
  temp->valuep = new value_t(val);
  return temp;
}

xpath_t::op_t * xpath_t::wrap_sequence(value_t::sequence_t * val)
{
  if (val->size() == 0) {
    // jww (2006-09-24): What is the "void" value?
    assert(0);
  }
  else if (val->size() == 1) {
    return wrap_value(val->front());
  }
  else {
    xpath_t::op_t * temp = new xpath_t::op_t(xpath_t::op_t::VALUE);
    temp->valuep = new value_t(val);
    return temp;
  }
}

xpath_t::op_t * xpath_t::wrap_functor(functor_t * fobj)
{
  xpath_t::op_t * temp = new xpath_t::op_t(xpath_t::op_t::FUNCTOR);
  temp->functor = fobj;
  return temp;
}

#if 0
xpath_t::op_t * xpath_t::wrap_mask(const std::string& pattern)
{
  xpath_t::op_t * temp = new xpath_t::op_t(xpath_t::op_t::MASK);
  temp->mask = new mask_t(pattern);
  return temp;
}
#endif

void xpath_t::scope_t::define(const std::string& name, op_t * def)
{
  DEBUG_PRINT("ledger.xpath.syms", "Defining '" << name << "' = " << def);

  std::pair<symbol_map::iterator, bool> result
    = symbols.insert(symbol_pair(name, def));
  if (! result.second) {
    symbol_map::iterator i = symbols.find(name);
    assert(i != symbols.end());
    (*i).second->release();
    symbols.erase(i);

    std::pair<symbol_map::iterator, bool> result
      = symbols.insert(symbol_pair(name, def));
    if (! result.second)
      throw new compile_error(std::string("Redefinition of '") +
			      name + "' in same scope");
  }
  def->acquire();
}

xpath_t::op_t *
xpath_t::scope_t::lookup(const std::string& name)
{
  symbol_map::const_iterator i = symbols.find(name);
  if (i != symbols.end())
    return (*i).second;
  else if (parent)
    return parent->lookup(name);
  return NULL;
}

void xpath_t::scope_t::define(const std::string& name, functor_t * def) {
  define(name, wrap_functor(def));
}

xpath_t::op_t::~op_t()
{
  TRACE_DTOR("xpath_t::op_t");

  DEBUG_PRINT("ledger.xpath.memory", "Destroying " << this);
  assert(refc == 0);

  switch (kind) {
  case VALUE:
    assert(! left);
    assert(valuep);
    delete valuep;
    break;

  case SYMBOL:
    assert(! left);
    assert(name);
    delete name;
    break;

  case ARG_INDEX:
    break;

  case FUNCTOR:
    assert(! left);
    assert(functor);
    delete functor;
    break;

#if 0
  case MASK:
    assert(! left);
    assert(mask);
    delete mask;
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
  default: {
    std::ostringstream buf;
    write(buf);
    throw new calc_error
      (std::string("Cannot determine value of expression symbol '") +
       buf.str() + "'");
  }
  }
}

xpath_t::op_t *
xpath_t::parse_value_term(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<op_t> node;

  token_t& tok = next_token(in, flags);

  switch (tok.kind) {
  case token_t::VALUE:
    node.reset(new op_t(op_t::VALUE));
    node->valuep = new value_t(tok.value);
    break;

  case token_t::IDENT: {
#ifdef USE_BOOST_PYTHON
    if (tok.value->to_string() == "lambda") // special
      try {
	char c, buf[4096];

	std::strcpy(buf, "lambda ");
	READ_INTO(in, &buf[7], 4000, c, true);

	op_t * eval = new op_t(op_t::O_EVAL);
	op_t * lambda = new op_t(op_t::FUNCTOR);
	lambda->functor = new python_functor_t(python_eval(buf));
	eval->set_left(lambda);
	op_t * sym = new op_t(op_t::SYMBOL);
	sym->name = new std::string("__ptr");
	eval->set_right(sym);

	node.reset(eval);

	goto done;
      }
      catch(const boost::python::error_already_set&) {
	throw new parse_error("Error parsing lambda expression");
      }
#endif

    std::string ident = tok.value.to_string();
    if (std::isdigit(ident[0])) {
      node.reset(new op_t(op_t::ARG_INDEX));
      node->arg_index = std::atol(ident.c_str());
    } else {
      //node.reset(new op_t(op_t::NODE_NAME));
      node.reset(new op_t(op_t::SYMBOL));
      node->name = new std::string(ident);
    }

    // An identifier followed by ( represents a function call
    tok = next_token(in, flags);
    if (tok.kind == token_t::LPAREN) {
      std::auto_ptr<op_t> call_node;
      call_node.reset(new op_t(op_t::O_EVAL));
      call_node->set_left(node.release());
      call_node->set_right(parse_value_expr(in, flags | XPATH_PARSE_PARTIAL));

      tok = next_token(in, flags);
      if (tok.kind != token_t::RPAREN)
	tok.unexpected();		// jww (2006-09-09): wanted )

      node.reset(call_node.release());
    } else {
      push_token(tok);
    }
    break;
  }

  case token_t::AT_SYM:
  case token_t::DOLLAR:
    break;

  case token_t::DOT:
    node.reset(new op_t(op_t::NODE_NAME));
    node->name_id = tree_t::CURRENT;
    break;
  case token_t::DOTDOT:
    node.reset(new op_t(op_t::NODE_NAME));
    node->name_id = tree_t::PARENT;
    break;
  case token_t::SLASH:
    node.reset(new op_t(op_t::NODE_NAME));
    node->name_id = tree_t::ROOT;
    break;
  case token_t::STAR:
    node.reset(new op_t(op_t::NODE_NAME));
    node->name_id = tree_t::ALL;
    break;

  case token_t::LPAREN:
    node.reset(parse_value_expr(in, flags | XPATH_PARSE_PARTIAL));
    if (! node.get())
	throw new parse_error(std::string(tok.symbol) +
			      " operator not followed by argument");
    tok = next_token(in, flags);
    if (tok.kind != token_t::RPAREN)
      tok.unexpected();		// jww (2006-09-09): wanted )
    break;

#if 0
  case token_t::REGEXP:
    node.reset(wrap_mask(tok.value.to_string()));
    break;
#endif

  default:
    push_token(tok);
    break;
  }

 done:
  return node.release();
}

xpath_t::op_t *
xpath_t::parse_path_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<op_t> node(parse_value_term(in, flags));

  if (node.get()) {
    // If the beginning of the path was /, just put it back; this
    // makes parsing much simpler.
    if (node->kind == op_t::NODE_NAME && node->name_id == tree_t::ROOT)
      push_token();

    token_t& tok = next_token(in, flags);
    while (tok.kind == token_t::SLASH) {
      std::auto_ptr<op_t> prev(node.release());

      tok = next_token(in, flags);
      node.reset(new op_t(tok.kind == token_t::SLASH ?
			  op_t::O_RFIND : op_t::O_FIND));
      if (tok.kind != token_t::SLASH)
	push_token(tok);

      node->set_left(prev.release());
      node->set_right(parse_value_term(in, flags));
      if (! node->right)
	throw new parse_error("/ operator not followed by a valid term");

      tok = next_token(in, flags);
      while (tok.kind == token_t::LBRACKET) {
	prev.reset(node.release());
	node.reset(new op_t(op_t::O_PRED));
	node->set_left(prev.release());
	node->set_right(parse_value_expr(in, flags | XPATH_PARSE_PARTIAL));
	if (! node->right)
	  throw new parse_error("[ operator not followed by valid expression");

	tok = next_token(in, flags);
	if (tok.kind != token_t::RBRACKET)
	  tok.unexpected();		// jww (2006-09-09): wanted ]

	tok = next_token(in, flags);
      }
    }

    push_token(tok);
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_unary_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<op_t> node;

  token_t& tok = next_token(in, flags);

  switch (tok.kind) {
  case token_t::EXCLAM: {
    std::auto_ptr<op_t> expr(parse_path_expr(in, flags));
    if (! expr.get())
      throw new parse_error(std::string(tok.symbol) +
			    " operator not followed by argument");
    // A very quick optimization
    if (expr->kind == op_t::VALUE) {
      *expr->valuep = ! *expr->valuep;
      node.reset(expr.release());
    } else {
      node.reset(new op_t(op_t::O_NOT));
      node->set_left(expr.release());
    }
    break;
  }

  case token_t::MINUS: {
    std::auto_ptr<op_t> expr(parse_path_expr(in, flags));
    if (! expr.get())
      throw new parse_error(std::string(tok.symbol) +
			    " operator not followed by argument");
    // A very quick optimization
    if (expr->kind == op_t::VALUE) {
      expr->valuep->negate();
      node.reset(expr.release());
    } else {
      node.reset(new op_t(op_t::O_NEG));
      node->set_left(expr.release());
    }
    break;
  }

  case token_t::PERCENT: {
    std::auto_ptr<op_t> expr(parse_path_expr(in, flags));
    if (! expr.get())
      throw new parse_error(std::string(tok.symbol) +
			    " operator not followed by argument");
    // A very quick optimization
    if (expr->kind == op_t::VALUE) {
      static value_t perc("100.0%");
      *expr->valuep = perc * *expr->valuep;
      node.reset(expr.release());
    } else {
      node.reset(new op_t(op_t::O_PERC));
      node->set_left(expr.release());
    }
    break;
  }

  default:
    push_token(tok);
    node.reset(parse_path_expr(in, flags));
    break;
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_mul_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<op_t> node(parse_unary_expr(in, flags));

  if (node.get()) {
    token_t& tok = next_token(in, flags);
    // jww (2006-09-24): Look for "div" operator here
    if (tok.kind == token_t::STAR /*|| tok.kind == token_t::SLASH*/) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(tok.kind == token_t::STAR ?
				       op_t::O_MUL :
				       op_t::O_DIV));
      node->set_left(prev.release());
      node->set_right(parse_mul_expr(in, flags));
      if (! node->right)
	throw new parse_error(std::string(tok.symbol) +
			      " operator not followed by argument");

      tok = next_token(in, flags);
    }
    push_token(tok);
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_add_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<op_t> node(parse_mul_expr(in, flags));

  if (node.get()) {
    token_t& tok = next_token(in, flags);
    if (tok.kind == token_t::PLUS ||
	tok.kind == token_t::MINUS) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(tok.kind == token_t::PLUS ?
				       op_t::O_ADD :
				       op_t::O_SUB));
      node->set_left(prev.release());
      node->set_right(parse_add_expr(in, flags));
      if (! node->right)
	throw new parse_error(std::string(tok.symbol) +
			      " operator not followed by argument");

      tok = next_token(in, flags);
    }
    push_token(tok);
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_logic_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<op_t> node(parse_add_expr(in, flags));

  if (node.get()) {
    op_t::kind_t kind = op_t::LAST;

    unsigned short _flags = flags;

    token_t& tok = next_token(in, flags);
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
	node->set_right(parse_boolean_expr(in, flags));
      else
	node->set_right(parse_add_expr(in, _flags));

      if (! node->right) {
	if (tok.kind == token_t::PLUS)
	  throw new parse_error(std::string(tok.symbol) +
				" operator not followed by argument");
	else
	  throw new parse_error(std::string(tok.symbol) +
				" operator not followed by argument");
      }
    }
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_boolean_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<op_t> node(parse_logic_expr(in, flags));

  if (node.get()) {
    token_t& tok = next_token(in, flags);
    if (tok.kind == token_t::AMPER ||
	tok.kind == token_t::PIPE ||
	tok.kind == token_t::QUESTION) {
      switch (tok.kind) {
      case token_t::AMPER: {
	std::auto_ptr<op_t> prev(node.release());
	node.reset(new op_t(op_t::O_AND));
	node->set_left(prev.release());
	node->set_right(parse_boolean_expr(in, flags));
	if (! node->right)
	  throw new parse_error(std::string(tok.symbol) +
				" operator not followed by argument");
	break;
      }
      case token_t::PIPE: {
	std::auto_ptr<op_t> prev(node.release());
	node.reset(new op_t(op_t::O_OR));
	node->set_left(prev.release());
	node->set_right(parse_boolean_expr(in, flags));
	if (! node->right)
	  throw new parse_error(std::string(tok.symbol) +
				" operator not followed by argument");
	break;
      }

      case token_t::QUESTION: {
	std::auto_ptr<op_t> prev(node.release());
	node.reset(new op_t(op_t::O_QUES));
	node->set_left(prev.release());
	node->set_right(new op_t(op_t::O_COLON));
	node->right->set_left(parse_logic_expr(in, flags));
	if (! node->right)
	  throw new parse_error(std::string(tok.symbol) +
				" operator not followed by argument");
	tok = next_token(in, flags);
	if (tok.kind != token_t::COLON)
	  tok.unexpected();	// jww (2006-09-09): wanted :
	node->right->set_right(parse_logic_expr(in, flags));
	if (! node->right)
	  throw new parse_error(std::string(tok.symbol) +
				" operator not followed by argument");
	break;
      }
      }
      tok = next_token(in, flags);
    }
    push_token(tok);
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_value_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<op_t> node(parse_boolean_expr(in, flags));

  if (node.get()) {
    token_t& tok = next_token(in, flags);
    if (tok.kind == token_t::COMMA) {
      std::auto_ptr<op_t> prev(node.release());
      node.reset(new op_t(op_t::O_COMMA));
      node->set_left(prev.release());
      node->set_right(parse_value_expr(in, flags));
      if (! node->right)
	throw new parse_error(std::string(tok.symbol) +
			      " operator not followed by argument");

      tok = next_token(in, flags);
    }

    if (tok.kind != token_t::TOK_EOF) {
      if (flags & XPATH_PARSE_PARTIAL)
	push_token(tok);
      else
	tok.unexpected();
    }
  }
  else if (! (flags & XPATH_PARSE_PARTIAL)) {
    throw new parse_error(std::string("Failed to parse value expression"));
  }

  return node.release();
}

xpath_t::op_t *
xpath_t::parse_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<op_t> node(parse_value_expr(in, flags));

  if (use_lookahead) {
    use_lookahead = false;
    lookahead.rewind(in);
  }
  lookahead.clear();

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
xpath_t::op_t::copy(op_t * left, op_t * right) const
{
  std::auto_ptr<op_t> node(new op_t(kind));
  if (left)
    node->set_left(left);
  if (right)
    node->set_right(right);
  return node.release();
}

bool xpath_t::op_t::find_xml_nodes(xml::node_t * context, scope_t * scope,
				   bool resolve, bool defer,
				   value_t::sequence_t& result_seq,
				   bool recursive)
{
  xpath_t expr(compile(context, scope, resolve));

  bool all_constant = defer;

  if (defer || ! expr->constant()) {
    result_seq.push_back((void *)expr->acquire());
    all_constant = false;
  } else {
    // Flatten out nested sequences, according to XPath 2.0
    if (expr->valuep->type == value_t::SEQUENCE) {
      value_t::sequence_t * subseq = expr->valuep->to_sequence();
      for (value_t::sequence_t::iterator j = subseq->begin();
	   j != subseq->end();
	   j++) {
	result_seq.push_back(*j);
      }
    }
    else if (expr->valuep->type == value_t::XML_NODE) {
      result_seq.push_back(*expr->valuep);
    }
    all_constant = true;
  }

  if (recursive) {
    for (xml::node_t * node = context->children; node; node = node->next)
      if (! find_xml_nodes(node, scope, resolve, defer, result_seq, recursive))
	all_constant = false;
  }

  return all_constant;
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
      *opp = static_cast<op_t *>((*i).to_pointer());
  }

  return lit_seq.release();
}

xpath_t::op_t * xpath_t::op_t::compile(node_t * context, scope_t * scope,
				       bool resolve)
{
  try {
  switch (kind) {
  case VALUE:
    return acquire();

  case SYMBOL: {
    // First, look up the symbol as a node name within the current
    // context.  If any exist, then return the set of names.

    xml::node_t *      first = NULL;
    value_t::sequence_t * nodes = NULL;
    for (xml::node_t * node = context->children; node; node = node->next) {
      if (*name == node->name) {
	if (! first) {
	  first = node;
	}
	else if (! nodes) {
	  nodes = new value_t::sequence_t;
	  nodes->push_back(first);
	}

	if (nodes)
	  nodes->push_back(node);
      }
    }

    if (nodes)
      return wrap_value(nodes)->acquire();
    else if (first)
      return wrap_value(first)->acquire();

    // jww (2006-09-24): What happens if none are found?

    // jww (2006-09-24): Symbol lookup will have to be for $symbol
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
  }

  case NODE_NAME:
    switch (name_id) {
    case tree_t::CURRENT:
      return wrap_value(context)->acquire();

    case tree_t::PARENT:
      if (context->parent)
	return wrap_value(context->parent)->acquire();
      else
	throw new compile_error("Reference to parent node from root node");

    case tree_t::ROOT:
      return wrap_value(context->tree->top)->acquire();

    case tree_t::ALL: {
      value_t::sequence_t * nodes = new value_t::sequence_t;
      for (xml::node_t * node = context->children; node; node = node->next)
	nodes->push_back(node);
      return wrap_value(nodes)->acquire();
    }
    }
    break;

  case ARG_INDEX:
    if (scope && scope->arg_scope) {
      if (arg_index < scope->args.size())
	return wrap_value(scope->args[arg_index])->acquire();
      else
	throw new compile_error("Reference to non-existing argument");
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
      return wrap_value(expr->valuep->negated())->acquire();
    } else {
      expr->valuep->negate();
      return expr->acquire();
    }
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
      default: assert(0); break;
      }
      return wrap_value(temp)->acquire();
    } else {
      switch (kind) {
      case O_ADD: *lexpr->valuep += *rexpr->valuep; break;
      case O_SUB: *lexpr->valuep -= *rexpr->valuep; break;
      case O_MUL: *lexpr->valuep *= *rexpr->valuep; break;
      case O_DIV: *lexpr->valuep /= *rexpr->valuep; break;
      default: assert(0); break;
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
      default: assert(0); break;
      }
    } else {
      switch (kind) {
      case O_NEQ: *lexpr->valuep = *lexpr->valuep != *rexpr->valuep; break;
      case O_EQ:  *lexpr->valuep = *lexpr->valuep == *rexpr->valuep; break;
      case O_LT:  *lexpr->valuep = *lexpr->valuep <  *rexpr->valuep; break;
      case O_LTE: *lexpr->valuep = *lexpr->valuep <= *rexpr->valuep; break;
      case O_GT:  *lexpr->valuep = *lexpr->valuep >  *rexpr->valuep; break;
      case O_GTE: *lexpr->valuep = *lexpr->valuep >= *rexpr->valuep; break;
      default: assert(0); break;
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
      throw new compile_error("Left operand of mask operator is not a string");

    assert(rexpr->mask);

    bool result = rexpr->mask->match(lexpr->valuep->to_string());
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
    if (left->kind == SYMBOL) {
      xpath_t rexpr(right->compile(context, scope, resolve));
      if (scope)
	scope->define(*left->name, rexpr);
      return rexpr->acquire();
    } else {
      assert(left->kind == O_EVAL);
      assert(left->left->kind == SYMBOL);

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

	assert(arg->kind == SYMBOL);
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
    call_args->arg_scope = true;

    int index = 0;
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
      call_args->args.push_back(arg->compile(context, scope, resolve)->value());
    }

    if (left->kind == SYMBOL) {
      if (resolve) {
	value_t temp;
	if (scope->resolve(*left->name, temp, call_args.get()))
	  return wrap_value(temp)->acquire();
      }

      // Don't compile to the left, otherwise the function name may
      // get resolved before we have a chance to call it
      xpath_t func(left->compile(context, scope, false));
      if (func->kind == FUNCTOR) {
	value_t temp;
	(*func->functor)(temp, call_args.get());
	return wrap_value(temp)->acquire();
      } else {
	return func->compile(context, call_args.get(), resolve);
      }
    }
    else if (left->kind == FUNCTOR) {
      value_t temp;
      (*left->functor)(temp, call_args.get());
      return wrap_value(temp)->acquire();
    }
    else {
      assert(0);
    }
    break;
  }

  case O_FIND:
  case O_RFIND: {
    assert(left);
    assert(right);
    xpath_t lexpr(left->compile(context, scope, resolve));
    if (! lexpr->constant()) {
      if (left == lexpr)
	return acquire();
      else
	return copy(lexpr, right)->acquire();
    }

    std::auto_ptr<value_t::sequence_t> result_seq(new value_t::sequence_t);

    // jww (2006-09-24): What about when nothing is found?
    switch (lexpr->valuep->type) {
    case value_t::XML_NODE:
      if (! right->find_xml_nodes(lexpr->valuep->to_xml_node(), scope, resolve,
			      false, *result_seq.get(), kind == O_RFIND))
	return defer_sequence(*result_seq.get());
      else
	return wrap_sequence(result_seq.release())->acquire();

    case value_t::SEQUENCE: {
      value_t::sequence_t * seq = lexpr->valuep->to_sequence();

      bool all_constant = true;
      for (value_t::sequence_t::iterator i = seq->begin();
	   i != seq->end();
	   i++) {
	assert((*i).type != value_t::SEQUENCE);
	if ((*i).type != value_t::XML_NODE)
	  throw new compile_error("Attempting to apply path selection "
				  "to non-node(s)");

	if (! right->find_xml_nodes((*i).to_xml_node(), scope, resolve,
				    all_constant, *result_seq.get(),
				    kind == O_RFIND))
	  all_constant = false;
      }

      if (! all_constant)
	return defer_sequence(*result_seq.get());
      else
	return wrap_sequence(result_seq.release())->acquire();
    }

    default:
      throw new compile_error("Attempting to apply path selection "
			      "to non-node(s)");
    }
    break;
  }

  case O_PRED:
    // jww (2006-09-24): Implement predicates/indexes!
    assert(0);
    break;

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

  case LAST:
  default:
    assert(0);
    break;
  }
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

  assert(0);
  return NULL;
}

void xpath_t::calc(value_t& result, scope_t * scope) const
{
  try {
    // jww (2006-09-24): Fix
    xpath_t final(ptr->compile(NULL, scope, true));
    // jww (2006-09-09): Give a better error here if this is not
    // actually a value
    final->get_value(result);
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
}

xpath_t::context::context(const xpath_t&     _xpath,
			  const op_t *       _err_node,
			  const std::string& desc) throw()
  : xpath(_xpath), err_node(_err_node), error_context(desc)
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
    xpath.write(out, true, err_node, &begin, &end);
  out << std::endl;
  if (found) {
    out << "  ";
    for (int i = 0; i < end - start; i++) {
      if (i >= begin - start)
	out << "^";
      else
	out << " ";
    }
    out << std::endl;
  }
}

bool xpath_t::op_t::write(std::ostream&   out,
			      const bool      relaxed,
			      const op_t *  op_to_find,
			      unsigned long * start_pos,
			      unsigned long * end_pos) const
{
  int arg_index = 0;
  bool found = false;
  op_t * expr;

  if (start_pos && this == op_to_find) {
    *start_pos = (long)out.tellp() - 1;
    found = true;
  }

  std::string symbol;

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
      assert(0);
      break;
    case value_t::DATETIME:
      out << '[' << *(valuep) << ']';
      break;
    case value_t::STRING:
      out << '"' << *(valuep) << '"';
      break;
    }
    break;

  case SYMBOL:
    out << *name;
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
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_NEG:
    out << "-";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_ADD:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " + ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_SUB:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " - ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_MUL:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " * ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_DIV:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " / ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_NEQ:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " != ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_EQ:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " == ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_LT:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " < ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_LTE:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " <= ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_GT:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " > ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_GTE:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " >= ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_AND:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " & ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_OR:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " | ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_QUES:
    out << "(";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " ? ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_COLON:
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " : ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_COMMA:
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ", ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

#if 0
  case O_MATCH:
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " =~ ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_NMATCH:
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << " !~ ";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
#endif

  case O_DEFINE:
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << '=';
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_EVAL:
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "(";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_FIND:
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "/";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_RFIND:
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "//";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_PRED:
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "[";
    if (right && right->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    out << "]";
    break;

  case O_PERC:
    out << "%";
    if (left && left->write(out, relaxed, op_to_find, start_pos, end_pos))
      found = true;
    break;

  case LAST:
  default:
    assert(0);
    break;
  }

  if (! symbol.empty()) {
    if (commodity_t::find(symbol))
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
    out << "VALUE - " << *(valuep);
    break;

  case SYMBOL:
    out << "SYMBOL - " << *name;
    break;

  case NODE_NAME:
    out << "NODE_NAME - " << name_id;
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

  case O_PERC: out << "O_PERC"; break;

  case LAST:
  default:
    assert(0);
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

#ifdef USE_BOOST_PYTHON

#ifndef USE_PCH
#include <boost/python.hpp>
#endif

using namespace boost::python;
using namespace ledger;

value_t py_calc_1(xpath_t::op_t& xpath_t, const details_t& item)
{
  value_t result;
  xpath_t.calc(result, item);
  return result;
}

template <typename T>
value_t py_calc(xpath_t::op_t& xpath_t, const T& item)
{
  value_t result;
  xpath_t.calc(result, details_t(item));
  return result;
}

xpath_t::op_t * py_parse_xpath_t_1(const std::string& str)
{
  return parse_xpath_t(str);
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_RuntimeError, err.what());	\
  }

EXC_TRANSLATOR(xpath_t_error)
EXC_TRANSLATOR(calc_error)
#if 0
EXC_TRANSLATOR(mask_error)
#endif

void export_xpath()
{
  class_< details_t > ("Details", init<const entry_t&>())
    .def(init<const transaction_t&>())
    .def(init<const account_t&>())
    .add_property("entry",
		  make_getter(&details_t::entry,
			      return_value_policy<reference_existing_object>()))
    .add_property("xact",
		  make_getter(&details_t::xact,
			      return_value_policy<reference_existing_object>()))
    .add_property("account",
		  make_getter(&details_t::account,
			      return_value_policy<reference_existing_object>()))
    ;

  class_< xpath_t::op_t > ("ValueExpr", init<xpath_t::op_t::kind_t>())
    .def("calc", py_calc_1)
    .def("calc", py_calc<account_t>)
    .def("calc", py_calc<entry_t>)
    .def("calc", py_calc<transaction_t>)
    ;

  def("parse_xpath_t", py_parse_xpath_t_1,
      return_value_policy<manage_new_object>());

  class_< item_predicate<transaction_t> >
    ("TransactionPredicate", init<std::string>())
    .def("__call__", &item_predicate<transaction_t>::operator())
    ;

  class_< item_predicate<account_t> >
    ("AccountPredicate", init<std::string>())
    .def("__call__", &item_predicate<account_t>::operator())
    ;

#define EXC_TRANSLATE(type)					\
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(xpath_t_error);
  EXC_TRANSLATE(calc_error);
#if 0
  EXC_TRANSLATE(mask_error);
#endif
}

#endif // USE_BOOST_PYTHON

#ifdef TEST

#if 0
#include "session.h"
#include "format.h"
#endif

int main(int argc, char *argv[])
{
  try {
    xml::xpath_t expr(argv[1]);
    if (expr) {
      std::cout << "Parsed:" << std::endl;
      expr.dump(std::cout);

#if 0
      {
	ledger::session_t session;
	std::auto_ptr<xml::xpath_t::scope_t>
	  locals(new xml::xpath_t::scope_t(&session.globals));
	expr.compile(locals.get());
      }

      std::cout << "Compiled:" << std::endl;
      expr.dump(std::cout);
#endif

      std::cout << std::endl;
    } else {
      std::cerr << "Failed to parse value expression!" << std::endl;
    }

#if 0
    {
      ledger::session_t session;
      std::auto_ptr<xml::xpath_t::scope_t>
	locals(new xml::xpath_t::scope_t(&session.globals));

      ledger::format_t fmt(std::string("%20|%40{") + argv[1] + "}\n");
      fmt.format(std::cout, locals.get());
    }
#endif
  }
  catch (error * err) {
    std::cout.flush();
    if (err->context.empty())
      err->context.push_front(new error_context(""));
    err->reveal_context(std::cerr, "Error");
    std::cerr << err->what() << std::endl;
    delete err;
    return 1;
  }
  catch (fatal * err) {
    std::cout.flush();
    if (err->context.empty())
      err->context.push_front(new error_context(""));
    err->reveal_context(std::cerr, "Fatal");
    std::cerr << err->what() << std::endl;
    delete err;
    return 1;
  }
  catch (const std::exception& err) {
    std::cout.flush();
    std::cerr << "Error: " << err.what() << std::endl;
    return 1;
  }
}

#endif // TEST
