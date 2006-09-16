#ifdef USE_PCH
#include "pch.h"
#else
#include "valexpr.h"
#include "debug.h"
#include "util.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif
#endif

namespace ledger {

#ifndef THREADED
valexpr_t::token_t valexpr_t::lookahead;
#endif

void valexpr_t::token_t::parse_ident(std::istream& in)
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

void valexpr_t::token_t::next(std::istream& in, unsigned short flags)
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

  if (c == '@' || (! (flags & PARSE_VALEXPR_RELAXED) &&
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
    char buf[256];
    READ_INTO_(in, buf, 255, c, length, c != ']');
    if (c != ']')
      unexpected(c, ']');
    in.get(c);
    length++;
    interval_t timespan(buf);
    kind = VALUE;
    value = timespan.first();
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
    if (in.peek() == '=') {
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
    if (flags & PARSE_VALEXPR_REGEXP) {
      char buf[1024];
      READ_INTO_(in, buf, 1023, c, length, c != '/');
      in.get(c);
      if (c != '/')
	unexpected(c, '/');
      kind = REGEXP;
      value.set_string(buf);
      break;
    }
    kind = SLASH;
    break;

  case '=':
    in.get(c);
    c = in.peek();
    if (c == '=') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = EQUAL;
      length = 2;
      break;
    } else if (c == '~') {
      in.get(c);
      symbol[1] = c;
      symbol[2] = '\0';
      kind = MATCH;
      length = 2;
      break;
    }
    kind = ASSIGN;
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

  default:
    if (! (flags & PARSE_VALEXPR_RELAXED)) {
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
	if (flags & PARSE_VALEXPR_NO_MIGRATE)
	  parse_flags |= AMOUNT_PARSE_NO_MIGRATE;
	if (flags & PARSE_VALEXPR_NO_REDUCE)
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
	} else {
	  throw err;
	}
      }
    }
    break;
  }
}

void valexpr_t::token_t::rewind(std::istream& in)
{
  for (int i = 0; i < length; i++)
    in.unget();
}


void valexpr_t::token_t::unexpected()
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

void valexpr_t::token_t::unexpected(char c, char wanted)
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

valexpr_t::node_t * valexpr_t::wrap_value(const value_t& val)
{
  valexpr_t::node_t * temp = new valexpr_t::node_t(valexpr_t::node_t::VALUE);
  temp->valuep = new value_t(val);
  return temp;
}

valexpr_t::node_t * valexpr_t::wrap_functor(functor_t * fobj)
{
  valexpr_t::node_t * temp = new valexpr_t::node_t(valexpr_t::node_t::FUNCTOR);
  temp->functor = fobj;
  return temp;
}

valexpr_t::node_t * valexpr_t::wrap_mask(const std::string& pattern)
{
  valexpr_t::node_t * temp = new valexpr_t::node_t(valexpr_t::node_t::MASK);
  temp->mask = new mask_t(pattern);
  return temp;
}

void valexpr_t::scope_t::define(const std::string& name, node_t * def)
{
  DEBUG_PRINT("ledger.valexpr.syms", "Defining '" << name << "' = " << def);

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

valexpr_t::node_t *
valexpr_t::scope_t::lookup(const std::string& name)
{
  symbol_map::const_iterator i = symbols.find(name);
  if (i != symbols.end())
    return (*i).second;
  else if (parent)
    return parent->lookup(name);
  return NULL;
}

void valexpr_t::scope_t::define(const std::string& name, functor_t * def) {
  define(name, wrap_functor(def));
}

valexpr_t::node_t::~node_t()
{
  TRACE_DTOR("valexpr_t::node_t");

  DEBUG_PRINT("ledger.valexpr.memory", "Destroying " << this);
  assert(refc == 0);

  switch (kind) {
  case VALUE:
    assert(! left);
    assert(valuep);
    delete valuep;
    break;

  case ARG_INDEX:
    break;

  case FUNCTOR:
    assert(! left);
    assert(functor);
    delete functor;
    break;

  case MASK:
    assert(! left);
    assert(mask);
    delete mask;
    break;

  default:
    assert(kind < LAST);
    if (left)
      left->release();
    if (kind > TERMINALS && right)
      right->release();
    break;
  }
}

void valexpr_t::node_t::get_value(value_t& result) const
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

valexpr_t::node_t *
valexpr_t::parse_value_term(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<node_t> node;

  token_t& tok = next_token(in, flags | PARSE_VALEXPR_REGEXP);

  switch (tok.kind) {
  case token_t::LPAREN:
    node.reset(parse_value_expr(in, flags | PARSE_VALEXPR_PARTIAL));
    if (! node.get())
	throw new parse_error(std::string(tok.symbol) +
			      " operator not followed by argument");
    tok = next_token(in, flags);
    if (tok.kind != token_t::RPAREN)
      tok.unexpected();		// jww (2006-09-09): wanted )
    break;

  case token_t::REGEXP:
    node.reset(new node_t(node_t::MASK));
    node->mask = new mask_t(tok.value.to_string());
    break;

  case token_t::VALUE:
    node.reset(new node_t(node_t::VALUE));
    node->valuep = new value_t(tok.value);
    break;

  case token_t::IDENT:
#ifdef USE_BOOST_PYTHON
    if (tok.value->to_string() == "lambda") // special
      try {
	char c, buf[4096];

	std::strcpy(buf, "lambda ");
	READ_INTO(in, &buf[7], 4000, c, true);

	node_t * eval = new node_t(node_t::O_EVAL);
	node_t * lambda = new node_t(node_t::FUNCTOR);
	lambda->functor = new python_functor_t(python_eval(buf));
	eval->set_left(lambda);
	node_t * sym = new node_t(node_t::SYMBOL);
	sym->valuep = new value_t("__ptr", true);
	eval->set_right(sym);

	node.reset(eval);

	goto done;
      }
      catch(const boost::python::error_already_set&) {
	throw new parse_error("Error parsing lambda expression");
      }
#endif

    node.reset(new node_t(node_t::SYMBOL));
    node->valuep = new value_t(tok.value);

    // An identifier followed by ( represents a function call
    tok = next_token(in, flags);
    if (tok.kind == token_t::LPAREN) {
      std::auto_ptr<node_t> call_node;
      call_node.reset(new node_t(node_t::O_EVAL));
      call_node->set_left(node.release());
      call_node->set_right(parse_value_expr(in, flags | PARSE_VALEXPR_PARTIAL));

      tok = next_token(in, flags);
      if (tok.kind != token_t::RPAREN)
	tok.unexpected();		// jww (2006-09-09): wanted )

      node.reset(call_node.release());
    } else {
      push_token(tok);
    }
    break;

  default:
    push_token(tok);
    break;
  }

 done:
  return node.release();
}

valexpr_t::node_t *
valexpr_t::parse_unary_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<node_t> node;

  token_t& tok = next_token(in, flags | PARSE_VALEXPR_REGEXP);

  switch (tok.kind) {
  case token_t::EXCLAM: {
    std::auto_ptr<node_t> expr(parse_value_term(in, flags));
    if (! expr.get())
      throw new parse_error(std::string(tok.symbol) +
			    " operator not followed by argument");
    // A very quick optimization
    if (expr->kind == node_t::VALUE) {
      *expr->valuep = ! *expr->valuep;
      node.reset(expr.release());
    } else {
      node.reset(new node_t(node_t::O_NOT));
      node->set_left(expr.release());
    }
    break;
  }

  case token_t::MINUS: {
    std::auto_ptr<node_t> expr(parse_value_term(in, flags));
    if (! expr.get())
      throw new parse_error(std::string(tok.symbol) +
			    " operator not followed by argument");
    // A very quick optimization
    if (expr->kind == node_t::VALUE) {
      expr->valuep->negate();
      node.reset(expr.release());
    } else {
      node.reset(new node_t(node_t::O_NEG));
      node->set_left(expr.release());
    }
    break;
  }

  case token_t::PERCENT: {
    std::auto_ptr<node_t> expr(parse_value_term(in, flags));
    if (! expr.get())
      throw new parse_error(std::string(tok.symbol) +
			    " operator not followed by argument");
    // A very quick optimization
    if (expr->kind == node_t::VALUE) {
      static value_t perc("100.0%");
      *expr->valuep = perc * *expr->valuep;
      node.reset(expr.release());
    } else {
      node.reset(new node_t(node_t::O_PERC));
      node->set_left(expr.release());
    }
    break;
  }

  default:
    push_token(tok);
    node.reset(parse_value_term(in, flags));
    break;
  }

  return node.release();
}

valexpr_t::node_t *
valexpr_t::parse_mul_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<node_t> node(parse_unary_expr(in, flags));

  if (node.get()) {
    token_t& tok = next_token(in, flags);
    if (tok.kind == token_t::STAR ||
	tok.kind == token_t::SLASH) {
      std::auto_ptr<node_t> prev(node.release());
      node.reset(new node_t(tok.kind == token_t::STAR ?
				       node_t::O_MUL :
				       node_t::O_DIV));
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

valexpr_t::node_t *
valexpr_t::parse_add_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<node_t> node(parse_mul_expr(in, flags));

  if (node.get()) {
    token_t& tok = next_token(in, flags);
    if (tok.kind == token_t::PLUS ||
	tok.kind == token_t::MINUS) {
      std::auto_ptr<node_t> prev(node.release());
      node.reset(new node_t(tok.kind == token_t::PLUS ?
				       node_t::O_ADD :
				       node_t::O_SUB));
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

valexpr_t::node_t *
valexpr_t::parse_logic_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<node_t> node(parse_add_expr(in, flags));

  if (node.get()) {
    node_t::kind_t kind = node_t::LAST;

    token_t& tok = next_token(in, flags);
    switch (tok.kind) {
    case token_t::ASSIGN:
      kind = node_t::O_DEFINE;
      break;
    case token_t::EQUAL:
      kind = node_t::O_EQ;
      break;
    case token_t::NEQUAL:
      kind = node_t::O_NEQ;
      break;
    case token_t::MATCH:
      kind = node_t::O_MATCH;
      break;
    case token_t::LESS:
      kind = node_t::O_LT;
      break;
    case token_t::LESSEQ:
      kind = node_t::O_LTE;
      break;
    case token_t::GREATER:
      kind = node_t::O_GT;
      break;
    case token_t::GREATEREQ:
      kind = node_t::O_GTE;
      break;
    default:
      push_token(tok);
      break;
    }

    if (kind != node_t::LAST) {
      std::auto_ptr<node_t> prev(node.release());
      node.reset(new node_t(kind));
      node->set_left(prev.release());
      if (kind == node_t::O_DEFINE)
	node->set_right(parse_boolean_expr(in, flags));
      else
	node->set_right(parse_add_expr(in, flags));

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

valexpr_t::node_t *
valexpr_t::parse_boolean_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<node_t> node(parse_logic_expr(in, flags));

  if (node.get()) {
    token_t& tok = next_token(in, flags);
    if (tok.kind == token_t::AMPER ||
	tok.kind == token_t::PIPE ||
	tok.kind == token_t::QUESTION) {
      switch (tok.kind) {
      case token_t::AMPER: {
	std::auto_ptr<node_t> prev(node.release());
	node.reset(new node_t(node_t::O_AND));
	node->set_left(prev.release());
	node->set_right(parse_boolean_expr(in, flags));
	if (! node->right)
	  throw new parse_error(std::string(tok.symbol) +
				" operator not followed by argument");
	break;
      }
      case token_t::PIPE: {
	std::auto_ptr<node_t> prev(node.release());
	node.reset(new node_t(node_t::O_OR));
	node->set_left(prev.release());
	node->set_right(parse_boolean_expr(in, flags));
	if (! node->right)
	  throw new parse_error(std::string(tok.symbol) +
				" operator not followed by argument");
	break;
      }

      case token_t::QUESTION: {
	std::auto_ptr<node_t> prev(node.release());
	node.reset(new node_t(node_t::O_QUES));
	node->set_left(prev.release());
	node->set_right(new node_t(node_t::O_COLON));
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

valexpr_t::node_t *
valexpr_t::parse_value_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<node_t> node(parse_boolean_expr(in, flags));

  if (node.get()) {
    token_t& tok = next_token(in, flags);
    if (tok.kind == token_t::COMMA) {
      std::auto_ptr<node_t> prev(node.release());
      node.reset(new node_t(node_t::O_COMMA));
      node->set_left(prev.release());
      node->set_right(parse_value_expr(in, flags));
      if (! node->right)
	throw new parse_error(std::string(tok.symbol) +
			      " operator not followed by argument");

      tok = next_token(in, flags);
    }

    if (tok.kind != token_t::TOK_EOF) {
      if (flags & PARSE_VALEXPR_PARTIAL)
	push_token(tok);
      else
	tok.unexpected();
    }
  }
  else if (! (flags & PARSE_VALEXPR_PARTIAL)) {
    throw new parse_error(std::string("Failed to parse value expression"));
  }

  return node.release();
}

valexpr_t::node_t *
valexpr_t::parse_expr(std::istream& in, unsigned short flags) const
{
  std::auto_ptr<node_t> node(parse_value_expr(in, flags));

  if (use_lookahead) {
    use_lookahead = false;
    lookahead.rewind(in);
  }
  lookahead.clear();

  return node.release();
}

valexpr_t::node_t *
valexpr_t::node_t::new_node(kind_t kind, node_t * left, node_t * right)
{
  std::auto_ptr<node_t> node(new node_t(kind));
  if (left)
    node->set_left(left);
  if (right)
    node->set_right(right);
  return node.release();
}

valexpr_t::node_t *
valexpr_t::node_t::copy(node_t * left, node_t * right) const
{
  std::auto_ptr<node_t> node(new node_t(kind));
  if (left)
    node->set_left(left);
  if (right)
    node->set_right(right);
  return node.release();
}

valexpr_t::node_t * valexpr_t::node_t::compile(scope_t * scope,
					       bool make_calls)
{
  // jww (2006-09-15): Don't optimize by overwriting the current node,
  // since this eliminates reusability.

  try {
  switch (kind) {
  case VALUE:
    return acquire();

  case SYMBOL:
    if (scope)
      if (node_t * def = scope->lookup(valuep->to_string()))
	return def->compile(scope, make_calls);
    return acquire();

  case ARG_INDEX:
    if (scope && scope->arg_scope) {
      if (arg_index < scope->args.size())
	return wrap_value(scope->args[arg_index])->acquire();
      else
	throw new calc_error("Reference to non-existing argument");
    } else {
      return acquire();
    }

  case FUNCTOR:
    if (make_calls) {
      value_t temp;
      (*functor)(temp, scope);
      return wrap_value(temp)->acquire();
    } else {
      return acquire();
    }
    break;

  case MASK:
    return acquire();

  case O_NOT: {
    assert(left);
    valexpr_t expr(left->compile(scope, make_calls));
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
    valexpr_t expr(left->compile(scope, make_calls));
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
    valexpr_t lexpr(left->compile(scope, make_calls));
    valexpr_t rexpr(right->compile(scope, make_calls));
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
    valexpr_t lexpr(left->compile(scope, make_calls));
    valexpr_t rexpr(right->compile(scope, make_calls));
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
    valexpr_t lexpr(left->compile(scope, make_calls));
    if (lexpr->constant() && ! lexpr->valuep->strip_annotations()) {
      *lexpr->valuep = false;
      return lexpr->acquire();
    }

    valexpr_t rexpr(right->compile(scope, make_calls));
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
    valexpr_t lexpr(left->compile(scope, make_calls));
    if (lexpr->constant() && lexpr->valuep->strip_annotations())
      return lexpr->acquire();

    valexpr_t rexpr(right->compile(scope, make_calls));
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
    valexpr_t lexpr(left->compile(scope, make_calls));
    if (! lexpr->constant()) {
      valexpr_t rexpr(right->compile(scope, make_calls));
      if (left == lexpr && right == rexpr)
	return acquire();
      else
	return copy(lexpr, rexpr)->acquire();
    }

    if (lexpr->valuep->strip_annotations())
      return right->left->compile(scope, make_calls);
    else
      return right->right->compile(scope, make_calls);
  }

  case O_COLON: {
    valexpr_t lexpr(left->compile(scope, make_calls));
    valexpr_t rexpr(right->compile(scope, make_calls));
    if (left == lexpr && right == rexpr)
      return acquire();
    else
      return copy(lexpr, rexpr)->acquire();
  }

  case O_COMMA: {
    assert(left);
    assert(right);
    valexpr_t lexpr(left->compile(scope, make_calls)); // for side-effects
    return right->compile(scope, make_calls);
  }

  case O_MATCH: {
    assert(left);
    assert(right);
    if (right->kind != MASK)
      throw new calc_error("Right operand of mask operator is not a mask");
    assert(right->mask);

    valexpr_t lexpr(left->compile(scope, make_calls));
    if (! lexpr->constant()) {
      if (left == lexpr)
	return acquire();
      else
	return copy(lexpr, right)->acquire();
    }

    if (lexpr->valuep->type != value_t::STRING)
      throw new calc_error("Left operand of mask operator is not a string");

    if (left == lexpr) {
      return wrap_value(mask->match(lexpr->valuep->to_string()))->acquire();
    } else {
      *lexpr->valuep = mask->match(lexpr->valuep->to_string());
      return lexpr->acquire();
    }
  }

  case O_DEFINE:
    assert(left);
    assert(right);
    if (left->kind == SYMBOL) {
      valexpr_t rexpr(right->compile(scope, make_calls));
      if (scope)
	scope->define(left->valuep->to_string(), rexpr);
      return rexpr->acquire();
    } else {
      assert(left->kind == O_EVAL);
      assert(left->left->kind == SYMBOL);

      std::auto_ptr<scope_t> arg_scope(new scope_t(scope));

      int index = 0;
      node_t * args = left->right;
      while (args) {
	node_t * arg = args;
	if (args->kind == O_COMMA) {
	  arg = args->left;
	  args = args->right;
	} else {
	  args = NULL;
	}

	// Define the parameter so that on lookup the parser will find
	// an ARG_INDEX value.
	node_t * ref = new valexpr_t::node_t(valexpr_t::node_t::ARG_INDEX);
	ref->arg_index = index++;

	assert(arg->kind == SYMBOL);
	arg_scope->define(arg->valuep->to_string(), ref);
      }

      // jww (2006-09-16): If I compile the definition of a function,
      // I eliminate the possibility of future lookups
      //valexpr_t rexpr(right->compile(arg_scope.get(), make_calls));

      if (scope)
	scope->define(left->left->valuep->to_string(), right);

      return right->acquire();
    }

  case O_EVAL: {
    assert(left);

    std::auto_ptr<scope_t> call_args(new scope_t(scope));
    call_args->arg_scope = true;

    int index = 0;
    node_t * args = right;
    while (args) {
      node_t * arg = args;
      if (args->kind == O_COMMA) {
	arg = args->left;
	args = args->right;
      } else {
	args = NULL;
      }
      // jww (2006-09-15): Need to return a reference to these, if
      // there are undetermined arguments!
      call_args->args.push_back(arg->compile(scope, make_calls)->value());
    }

    if (left->kind == SYMBOL) {
      valexpr_t func(left->compile(scope, false)); // don't resolve!
      if (func->kind == FUNCTOR) {
	value_t temp;
	(*func->functor)(temp, call_args.get());
	return wrap_value(temp)->acquire();
      } else {
	return func->compile(call_args.get(), make_calls);
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

  case O_PERC: {
    assert(left);
    valexpr_t expr(left->compile(scope, make_calls));
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
    // jww (2006-09-09): I need a reference to the parent valexpr_t
    if (err->context.empty() ||
	! dynamic_cast<context *>(err->context.back()))
      err->context.push_back(new context(this));
#endif
    throw err;
  }

  assert(0);
  return NULL;
}

void valexpr_t::calc(value_t& result, scope_t * scope) const
{
  try {
    valexpr_t final(ptr->compile(scope, true));
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
      ctxt->valexpr = *this;
      ctxt->desc = "While calculating value expression:";
    }
#endif
    throw err;
  }
}

valexpr_t::context::context(const valexpr_t&   _valexpr,
			    const node_t *     _err_node,
			    const std::string& desc) throw()
  : valexpr(_valexpr), err_node(_err_node), error_context(desc)
{
  _err_node->acquire();
}

valexpr_t::context::~context() throw()
{
  if (err_node) err_node->release();
}

void valexpr_t::context::describe(std::ostream& out) const throw()
{
  if (! valexpr) {
    out << "valexpr_t::context expr not set!" << std::endl;
    return;
  }

  if (! desc.empty())
    out << desc << std::endl;

  out << "  ";
  unsigned long start = (long)out.tellp() - 1;
  unsigned long begin;
  unsigned long end;
  bool found = false;
  if (valexpr)
    valexpr.write(out, true, err_node, &begin, &end);
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

bool valexpr_t::node_t::write(std::ostream&   out,
			      const bool      relaxed,
			      const node_t *  node_to_find,
			      unsigned long * start_pos,
			      unsigned long * end_pos) const
{
  int arg_index = 0;
  bool found = false;
  node_t * expr;

  if (start_pos && this == node_to_find) {
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
    assert(valuep->type == value_t::STRING);
    out << *(valuep);
    break;

  case FUNCTOR:
    out << functor->name();
    break;

  case MASK:
    out << '/' << mask->pattern << '/';
    break;

  case ARG_INDEX:
    out << '@' << arg_index;
    break;

  case O_NOT:
    out << "!";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_NEG:
    out << "-";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_ADD:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " + ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_SUB:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " - ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_MUL:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " * ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_DIV:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " / ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_NEQ:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " != ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_EQ:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " == ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_LT:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " < ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_LTE:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " <= ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_GT:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " > ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_GTE:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " >= ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_AND:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " & ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_OR:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " | ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_QUES:
    out << "(";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " ? ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;
  case O_COLON:
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " : ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_COMMA:
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ", ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_MATCH:
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << " =~ ";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    break;

  case O_DEFINE:
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << '=';
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    break;
  case O_EVAL:
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << "(";
    if (right && right->write(out, relaxed, node_to_find, start_pos, end_pos))
      found = true;
    out << ")";
    break;

  case O_PERC:
    out << "%";
    if (left && left->write(out, relaxed, node_to_find, start_pos, end_pos))
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

  if (end_pos && this == node_to_find)
    *end_pos = (long)out.tellp() - 1;

  return found;
}

void valexpr_t::node_t::dump(std::ostream& out, const int depth) const
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
    out << "SYMBOL - " << *(valuep);
    break;
  case ARG_INDEX:
    out << "ARG_INDEX - " << arg_index;
    break;

  case FUNCTOR:
    out << "FUNCTOR - " << functor->name();
    break;
  case MASK:
    out << "MASK - " << mask->pattern;
    break;

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

  case O_MATCH: out << "O_MATCH"; break;

  case O_DEFINE: out << "O_DEFINE"; break;
  case O_EVAL: out << "O_EVAL"; break;

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

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#ifndef USE_PCH
#include <boost/python.hpp>
#endif

using namespace boost::python;
using namespace ledger;

value_t py_calc_1(valexpr_t::node_t& valexpr_t, const details_t& item)
{
  value_t result;
  valexpr_t.calc(result, item);
  return result;
}

template <typename T>
value_t py_calc(valexpr_t::node_t& valexpr_t, const T& item)
{
  value_t result;
  valexpr_t.calc(result, details_t(item));
  return result;
}

valexpr_t::node_t * py_parse_valexpr_t_1(const std::string& str)
{
  return parse_valexpr_t(str);
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_RuntimeError, err.what());	\
  }

EXC_TRANSLATOR(valexpr_t_error)
EXC_TRANSLATOR(calc_error)
EXC_TRANSLATOR(mask_error)

void export_valexpr()
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

  class_< valexpr_t::node_t > ("ValueExpr", init<valexpr_t::node_t::kind_t>())
    .def("calc", py_calc_1)
    .def("calc", py_calc<account_t>)
    .def("calc", py_calc<entry_t>)
    .def("calc", py_calc<transaction_t>)
    ;

  def("parse_valexpr_t", py_parse_valexpr_t_1,
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

  EXC_TRANSLATE(valexpr_t_error);
  EXC_TRANSLATE(calc_error);
  EXC_TRANSLATE(mask_error);
}

#endif // USE_BOOST_PYTHON

#ifdef TEST

#include "session.h"
#include "format.h"

int main(int argc, char *argv[])
{
  try {
    ledger::valexpr_t expr(argv[1]);
    if (expr) {
      std::cout << "Parsed:" << std::endl;
      expr.dump(std::cout);

      {
	ledger::session_t session;
	std::auto_ptr<ledger::valexpr_t::scope_t>
	  locals(new ledger::valexpr_t::scope_t(&session.globals));
	expr.compile(locals.get());
      }

      std::cout << "Compiled:" << std::endl;
      expr.dump(std::cout);

      std::cout << std::endl;
    } else {
      std::cerr << "Failed to parse value expression!" << std::endl;
    }

    {
      ledger::session_t session;
      std::auto_ptr<ledger::valexpr_t::scope_t>
	locals(new ledger::valexpr_t::scope_t(&session.globals));

      ledger::format_t fmt(std::string("%20|%40{") + argv[1] + "}\n");
      fmt.format(std::cout, locals.get());
    }
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
