#include "valexpr.h"
#include "walk.h"
#include "format.h"
#include "error.h"
#include "datetime.h"
#include "debug.h"
#include "util.h"

#include <vector>

#include <pcre.h>

namespace ledger {

mask_t::mask_t(const std::string& pat) : exclude(false)
{
  DEBUG_PRINT("ledger.memory.ctors", "ctor mask_t");

  const char * p = pat.c_str();
  if (*p == '-') {
    exclude = true;
    p++;
    while (std::isspace(*p))
      p++;
  }
  else if (*p == '+') {
    p++;
    while (std::isspace(*p))
      p++;
  }
  pattern = p;

  DEBUG_PRINT("valexpr.mask.parse", "pattern = '" << pattern << "'");

  const char *error;
  int erroffset;
  regexp = pcre_compile(pattern.c_str(), PCRE_CASELESS,
			&error, &erroffset, NULL);
  if (! regexp)
    std::cerr << "Warning: Failed to compile regexp: " << pattern
	      << std::endl;
}

mask_t::mask_t(const mask_t& m) : exclude(m.exclude), pattern(m.pattern)
{
  DEBUG_PRINT("ledger.memory.ctors", "ctor mask_t");

  const char *error;
  int erroffset;
  regexp = pcre_compile(pattern.c_str(), PCRE_CASELESS,
			&error, &erroffset, NULL);
  assert(regexp);
}

mask_t::~mask_t() {
  DEBUG_PRINT("ledger.memory.dtors", "dtor mask_t");
  pcre_free((pcre *)regexp);
}

bool mask_t::match(const std::string& str) const
{
  static int ovec[30];
  int result = pcre_exec((pcre *)regexp, NULL,
			 str.c_str(), str.length(), 0, 0, ovec, 30);
  return result >= 0 && ! exclude;
}


void value_expr_t::compute(value_t& result, const details_t& details) const
{
  switch (kind) {
  case CONSTANT_I:
    result = constant_i;
    break;
  case CONSTANT_T:
    result = (unsigned int) constant_t;
    break;

  case CONSTANT_A:
    result = constant_a;
    break;

  case AMOUNT:
    if (details.xact)
      result = details.xact->amount;
    else if (details.account && ACCT_DATA(details.account))
      result = ACCT_DATA(details.account)->value;
    break;

  case COST:
    if (details.xact) {
      if (details.xact->cost)
	result = *details.xact->cost;
      else
	result = details.xact->amount;
    }
    else if (details.account && ACCT_DATA(details.account)) {
      result = ACCT_DATA(details.account)->value.cost();
    }
    break;

  case TOTAL:
    if (details.xact && XACT_DATA(details.xact))
      result = XACT_DATA(details.xact)->total;
    else if (details.account && ACCT_DATA(details.account))
      result = ACCT_DATA(details.account)->total;
    break;
  case COST_TOTAL:
    if (details.xact && XACT_DATA(details.xact))
      result = XACT_DATA(details.xact)->total.cost();
    else if (details.account && ACCT_DATA(details.account))
      result = ACCT_DATA(details.account)->total.cost();
    break;

  case VALUE_EXPR:
    assert(format_t::value_expr);
    format_t::value_expr->compute(result, details);
    break;
  case TOTAL_EXPR:
    assert(format_t::total_expr);
    format_t::total_expr->compute(result, details);
    break;

  case DATE:
    if (details.entry)
      result = (unsigned int) details.entry->date;
    else
      result = (unsigned int) now;
    break;

  case CLEARED:
    if (details.entry) {
      result = details.entry->state == entry_t::CLEARED;
    }
    else if (details.account) {
      bool all_clear = true;
      for (transactions_list::const_iterator i
	     = details.account->transactions.begin();
	   i != details.account->transactions.end();
	   i++)
	if ((*i)->entry->state != entry_t::CLEARED) {
	  all_clear = false;
	  break;
	}
      result = all_clear;
    }
    break;

  case REAL:
    if (details.xact) {
      result = ! (details.xact->flags & TRANSACTION_VIRTUAL);
    }
    else if (details.account) {
      bool all_real = true;
      for (transactions_list::const_iterator i
	     = details.account->transactions.begin();
	   i != details.account->transactions.end();
	   i++)
	if ((*i)->flags & TRANSACTION_VIRTUAL) {
	  all_real = false;
	  break;
	}
      result = all_real;
    }
    break;

  case INDEX:
    if (details.xact && XACT_DATA(details.xact))
      result = XACT_DATA(details.xact)->index + 1;
    else if (details.account && ACCT_DATA(details.account))
      result = ACCT_DATA(details.account)->subcount;
    break;

  case COUNT:
    if (details.xact && XACT_DATA(details.xact))
      result = XACT_DATA(details.xact)->index + 1;
    else if (details.account && ACCT_DATA(details.account))
      result = ACCT_DATA(details.account)->count;
    break;

  case DEPTH:
    if (details.account)
      result = (unsigned int) details.account->depth;
    else
      result = 0U;
    break;

  case F_ARITH_MEAN:
    if (details.xact && XACT_DATA(details.xact)) {
      assert(left);
      left->compute(result, details);
      result /= amount_t(XACT_DATA(details.xact)->index + 1);
    }
    else if (details.account &&
	     ACCT_DATA(details.account) &&
	     ACCT_DATA(details.account)->count) {
      assert(left);
      left->compute(result, details);
      result /= amount_t(ACCT_DATA(details.account)->count);
    }
    break;

  case F_NEG:
    assert(left);
    left->compute(result, details);
    result.negate();
    break;

  case F_ABS:
    assert(left);
    left->compute(result, details);
    result.abs();
    break;

  case F_STRIP: {
    assert(left);
    left->compute(result, details);
    if (result.type == value_t::BALANCE ||
	result.type == value_t::BALANCE_PAIR) {
      // jww (2004-08-17): do something smarter here?
      result.cast(value_t::AMOUNT);
    }
    if (result.type == value_t::AMOUNT) {
      amount_t amt = result;
      amt.commodity = commodity_t::null_commodity;
      result = amt;
    }
    break;
  }

  case F_PAYEE_MASK:
    assert(mask);
    if (details.entry)
      result = mask->match(details.entry->payee);
    break;

  case F_ACCOUNT_MASK:
    assert(mask);
    if (details.account)
      result = mask->match(details.account->fullname());
    break;

  case F_SHORT_ACCOUNT_MASK:
    assert(mask);
    if (details.account)
      result = mask->match(details.account->name);
    break;

  case F_VALUE: {
    assert(left);
    left->compute(result, details);

    std::time_t moment = now;
    if (right) {
      switch (right->kind) {
      case DATE:
	if (details.entry)
	  moment = details.entry->date;
	break;
      case CONSTANT_T:
	moment = right->constant_t;
	break;
      default:
	throw compute_error("Invalid date passed to P(value,date)");
      }
    }

    switch (result.type) {
    case value_t::BOOLEAN:
    case value_t::INTEGER:
      break;
    case value_t::AMOUNT:
      result = ((amount_t *)result.data)->value(moment);
      break;
    case value_t::BALANCE:
      result = ((balance_t *)result.data)->value(moment);
      break;
    case value_t::BALANCE_PAIR:
      result = ((balance_pair_t *)result.data)->quantity.value(moment);
      break;
    }
    break;
  }

  case O_NOT:
    left->compute(result, details);
    result.negate();
    break;

  case O_QUES:
    assert(left);
    assert(right);
    assert(right->kind == O_COL);
    left->compute(result, details);
    if (result)
      right->left->compute(result, details);
    else
      right->right->compute(result, details);
    break;

  case O_AND:
    assert(left);
    assert(right);
    left->compute(result, details);
    if (result)
      right->compute(result, details);
    break;

  case O_OR:
    assert(left);
    assert(right);
    left->compute(result, details);
    if (! result)
      right->compute(result, details);
    break;

  case O_EQ:
  case O_LT:
  case O_LTE:
  case O_GT:
  case O_GTE: {
    assert(left);
    assert(right);
    value_t temp;
    left->compute(temp, details);
    right->compute(result, details);
    switch (kind) {
    case O_EQ:  result = temp == result; break;
    case O_LT:  result = temp <  result; break;
    case O_LTE: result = temp <= result; break;
    case O_GT:  result = temp >  result; break;
    case O_GTE: result = temp >= result; break;
    default: assert(0); break;
    }
    break;
  }

  case O_ADD:
  case O_SUB:
  case O_MUL:
  case O_DIV: {
    assert(left);
    assert(right);
    value_t temp;
    right->compute(temp, details);
    left->compute(result, details);
    switch (kind) {
    case O_ADD: result += temp; break;
    case O_SUB: result -= temp; break;
    case O_MUL: result *= temp; break;
    case O_DIV: result /= temp; break;
    default: assert(0); break;
    }
    break;
  }

  case LAST:
  default:
    assert(0);
    break;
  }
}

value_expr_t * parse_value_term(std::istream& in);

inline value_expr_t * parse_value_term(const char * p) {
  std::istringstream stream(p);
  return parse_value_term(stream);
}

value_expr_t * parse_value_term(std::istream& in)
{
  static char buf[256];

  std::auto_ptr<value_expr_t> node;

  char c = peek_next_nonws(in);
  if (std::isdigit(c)) {
    READ_INTO(in, buf, 255, c, std::isdigit(c));

    node.reset(new value_expr_t(value_expr_t::CONSTANT_I));
    node->constant_i = std::atol(buf);
    return node.release();
  }
  else if (c == '{') {
    in.get(c);
    READ_INTO(in, buf, 255, c, c != '}');
    if (c == '}')
      in.get(c);
    else
      throw value_expr_error("Missing '}'");

    node.reset(new value_expr_t(value_expr_t::CONSTANT_A));
    node->constant_a.parse(buf);
    return node.release();
  }

  in.get(c);
  switch (c) {
  // Basic terms
  case 'm':
    node.reset(new value_expr_t(value_expr_t::CONSTANT_T));
    node->constant_t = now;
    break;

  case 'a': node.reset(new value_expr_t(value_expr_t::AMOUNT)); break;
  case 'c': node.reset(new value_expr_t(value_expr_t::COST)); break;
  case 'd': node.reset(new value_expr_t(value_expr_t::DATE)); break;
  case 'X': node.reset(new value_expr_t(value_expr_t::CLEARED)); break;
  case 'R': node.reset(new value_expr_t(value_expr_t::REAL)); break;
  case 'n': node.reset(new value_expr_t(value_expr_t::INDEX)); break;
  case 'N': node.reset(new value_expr_t(value_expr_t::COUNT)); break;
  case 'l': node.reset(new value_expr_t(value_expr_t::DEPTH)); break;
  case 'O': node.reset(new value_expr_t(value_expr_t::TOTAL)); break;
  case 'C': node.reset(new value_expr_t(value_expr_t::COST_TOTAL)); break;

  // Relating to format_t
  case 't': node.reset(new value_expr_t(value_expr_t::VALUE_EXPR)); break;
  case 'T': node.reset(new value_expr_t(value_expr_t::TOTAL_EXPR)); break;

  // Compound terms
  case 'v': node.reset(parse_value_expr("P(a,d)")); break;
  case 'V': node.reset(parse_value_term("P(O,d)")); break;
  case 'g': node.reset(parse_value_expr("v-c")); break;
  case 'G': node.reset(parse_value_expr("V-C")); break;

  // Functions
  case '-':
    node.reset(new value_expr_t(value_expr_t::F_NEG));
    node->left = parse_value_term(in);
    break;

  case 'A':
    node.reset(new value_expr_t(value_expr_t::F_ABS));
    node->left = parse_value_term(in);
    break;

  case 'S':
    node.reset(new value_expr_t(value_expr_t::F_STRIP));
    node->left = parse_value_term(in);
    break;

  case 'M':
    node.reset(new value_expr_t(value_expr_t::F_ARITH_MEAN));
    node->left = parse_value_term(in);
    break;

  case 'D': {
    node.reset(new value_expr_t(value_expr_t::O_SUB));
    node->left  = parse_value_term("a");
    node->right = parse_value_term(in);
    break;
  }

  case 'P':
    node.reset(new value_expr_t(value_expr_t::F_VALUE));
    if (peek_next_nonws(in) == '(') {
      in.get(c);
      node->left = parse_value_expr(in);
      if (peek_next_nonws(in) == ',') {
	in.get(c);
	node->right = parse_value_expr(in);
      }
      if (peek_next_nonws(in) == ')')
	in.get(c);
      else
	throw value_expr_error("Missing ')'");
    } else {
      node->left = parse_value_term(in);
    }
    break;

  // Other
  case '/': {
    bool payee_mask	    = false;
    bool short_account_mask = false;

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

    READ_INTO(in, buf, 255, c, c != '/');
    if (c != '/')
      throw value_expr_error("Missing closing '/'");

    in.get(c);
    node.reset(new value_expr_t(short_account_mask ?
			    value_expr_t::F_SHORT_ACCOUNT_MASK :
			    (payee_mask ? value_expr_t::F_PAYEE_MASK :
			     value_expr_t::F_ACCOUNT_MASK)));
    node->mask = new mask_t(buf);
    break;
  }

  case '(':
    node.reset(parse_value_expr(in));
    if (peek_next_nonws(in) == ')')
      in.get(c);
    else
      throw value_expr_error("Missing ')'");
    break;

  case '[': {
    READ_INTO(in, buf, 255, c, c != ']');
    if (c != ']')
      throw value_expr_error("Missing ']'");
    in.get(c);

    node.reset(new value_expr_t(value_expr_t::CONSTANT_T));

    std::string datespec = buf;
    std::istringstream stream(datespec);
    interval_t::parse(stream, &node->constant_t, NULL);
    break;
  }

  default:
    in.unget();
    break;
  }

  return node.release();
}

value_expr_t * parse_mul_expr(std::istream& in)
{
  std::auto_ptr<value_expr_t> node(parse_value_term(in));

  if (node.get() && ! in.eof()) {
    char c = peek_next_nonws(in);
    while (c == '*' || c == '/') {
      in.get(c);
      switch (c) {
      case '*': {
	std::auto_ptr<value_expr_t> prev(node.release());
	node.reset(new value_expr_t(value_expr_t::O_MUL));
	node->left  = prev.release();
	node->right = parse_value_term(in);
	break;
      }

      case '/': {
	std::auto_ptr<value_expr_t> prev(node.release());
	node.reset(new value_expr_t(value_expr_t::O_DIV));
	node->left  = prev.release();
	node->right = parse_value_term(in);
	break;
      }
      }
      c = peek_next_nonws(in);
    }
  }

  return node.release();
}

value_expr_t * parse_add_expr(std::istream& in)
{
  std::auto_ptr<value_expr_t> node(parse_mul_expr(in));

  if (node.get() && ! in.eof()) {
    char c = peek_next_nonws(in);
    while (c == '+' || c == '-') {
      in.get(c);
      switch (c) {
      case '+': {
	std::auto_ptr<value_expr_t> prev(node.release());
	node.reset(new value_expr_t(value_expr_t::O_ADD));
	node->left  = prev.release();
	node->right = parse_mul_expr(in);
	break;
      }

      case '-': {
	std::auto_ptr<value_expr_t> prev(node.release());
	node.reset(new value_expr_t(value_expr_t::O_SUB));
	node->left  = prev.release();
	node->right = parse_mul_expr(in);
	break;
      }
      }
      c = peek_next_nonws(in);
    }
  }

  return node.release();
}

value_expr_t * parse_logic_expr(std::istream& in)
{
  std::auto_ptr<value_expr_t> node;

  if (peek_next_nonws(in) == '!') {
    char c;
    in.get(c);
    node.reset(new value_expr_t(value_expr_t::O_NOT));
    node->left = parse_logic_expr(in);
    return node.release();
  }

  node.reset(parse_add_expr(in));

  if (node.get() && ! in.eof()) {
    char c = peek_next_nonws(in);
    if (c == '=' || c == '<' || c == '>') {
      in.get(c);
      switch (c) {
      case '=': {
	std::auto_ptr<value_expr_t> prev(node.release());
	node.reset(new value_expr_t(value_expr_t::O_EQ));
	node->left  = prev.release();
	node->right = parse_add_expr(in);
	break;
      }

      case '<': {
	std::auto_ptr<value_expr_t> prev(node.release());
	node.reset(new value_expr_t(value_expr_t::O_LT));
	if (peek_next_nonws(in) == '=') {
	  in.get(c);
	  node->kind = value_expr_t::O_LTE;
	}
	node->left  = prev.release();
	node->right = parse_add_expr(in);
	break;
      }

      case '>': {
	std::auto_ptr<value_expr_t> prev(node.release());
	node.reset(new value_expr_t(value_expr_t::O_GT));
	if (peek_next_nonws(in) == '=') {
	  in.get(c);
	  node->kind = value_expr_t::O_GTE;
	}
	node->left  = prev.release();
	node->right = parse_add_expr(in);
	break;
      }

      default:
	if (! in.eof()) {
	  std::ostringstream err;
	  err << "Unexpected character '" << c << "'";
	  throw value_expr_error(err.str());
	}
      }
    }
  }

  return node.release();
}

value_expr_t * parse_value_expr(std::istream& in)
{
  std::auto_ptr<value_expr_t> node(parse_logic_expr(in));

  if (node.get() && ! in.eof()) {
    char c = peek_next_nonws(in);
    while (c == '&' || c == '|' || c == '?') {
      in.get(c);
      switch (c) {
      case '&': {
	std::auto_ptr<value_expr_t> prev(node.release());
	node.reset(new value_expr_t(value_expr_t::O_AND));
	node->left  = prev.release();
	node->right = parse_logic_expr(in);
	break;
      }

      case '|': {
	std::auto_ptr<value_expr_t> prev(node.release());
	node.reset(new value_expr_t(value_expr_t::O_OR));
	node->left  = prev.release();
	node->right = parse_logic_expr(in);
	break;
      }

      case '?': {
	std::auto_ptr<value_expr_t> prev(node.release());
	node.reset(new value_expr_t(value_expr_t::O_QUES));
	node->left  = prev.release();
	value_expr_t * choices;
	node->right = choices = new value_expr_t(value_expr_t::O_COL);
	choices->left = parse_logic_expr(in);
	c = peek_next_nonws(in);
	if (c != ':') {
	  std::ostringstream err;
	  err << "Unexpected character '" << c << "'";
	  throw value_expr_error(err.str());
	}
	in.get(c);
	choices->right = parse_logic_expr(in);
	break;
      }

      default:
	if (! in.eof()) {
	  std::ostringstream err;
	  err << "Unexpected character '" << c << "'";
	  throw value_expr_error(err.str());
	}
      }
      c = peek_next_nonws(in);
    }
  }

  return node.release();
}

#ifdef DEBUG_ENABLED

void dump_value_expr(std::ostream& out, const value_expr_t * node)
{
  switch (node->kind) {
  case value_expr_t::CONSTANT_I:
    out << "UINT[" << node->constant_i << "]";
    break;
  case value_expr_t::CONSTANT_T:
    out << "DATE/TIME[" << node->constant_t << "]";
    break;
  case value_expr_t::CONSTANT_A:
    out << "CONST[" << node->constant_a << "]";
    break;

  case value_expr_t::AMOUNT:	   out << "AMOUNT"; break;
  case value_expr_t::COST:	   out << "COST"; break;
  case value_expr_t::DATE:	   out << "DATE"; break;
  case value_expr_t::CLEARED:	   out << "CLEARED"; break;
  case value_expr_t::REAL:	   out << "REAL"; break;
  case value_expr_t::INDEX:	   out << "INDEX"; break;
  case value_expr_t::COUNT:	   out << "COUNT"; break;
  case value_expr_t::DEPTH:	   out << "DEPTH"; break;
  case value_expr_t::TOTAL:        out << "TOTAL"; break;
  case value_expr_t::COST_TOTAL:   out << "COST_TOTAL"; break;

  case value_expr_t::F_ARITH_MEAN:
    out << "MEAN(";
    dump_value_expr(out, node->left);
    out << ")";
    break;

  case value_expr_t::F_NEG:
    out << "ABS(";
    dump_value_expr(out, node->left);
    out << ")";
    break;

  case value_expr_t::F_ABS:
    out << "ABS(";
    dump_value_expr(out, node->left);
    out << ")";
    break;

  case value_expr_t::F_STRIP:
    out << "STRIP(";
    dump_value_expr(out, node->left);
    out << ")";
    break;

  case value_expr_t::F_PAYEE_MASK:
    assert(node->mask);
    out << "P_MASK(" << node->mask->pattern << ")";
    break;

  case value_expr_t::F_ACCOUNT_MASK:
    assert(node->mask);
    out << "A_MASK(" << node->mask->pattern << ")";
    break;

  case value_expr_t::F_SHORT_ACCOUNT_MASK:
    assert(node->mask);
    out << "A_SMASK(" << node->mask->pattern << ")";
    break;

  case value_expr_t::F_VALUE:
    out << "VALUE(";
    dump_value_expr(out, node->left);
    if (node->right) {
      out << ", ";
      dump_value_expr(out, node->right);
    }
    out << ")";
    break;

  case value_expr_t::O_NOT:
    out << "!";
    dump_value_expr(out, node->left);
    break;

  case value_expr_t::O_QUES:
    dump_value_expr(out, node->left);
    out << "?";
    dump_value_expr(out, node->right->left);
    out << ":";
    dump_value_expr(out, node->right->right);
    break;

  case value_expr_t::O_AND:
  case value_expr_t::O_OR:
    out << "(";
    dump_value_expr(out, node->left);
    switch (node->kind) {
    case value_expr_t::O_AND: out << " & "; break;
    case value_expr_t::O_OR:  out << " | "; break;
    default: assert(0); break;
    }
    dump_value_expr(out, node->right);
    out << ")";
    break;

  case value_expr_t::O_EQ:
  case value_expr_t::O_LT:
  case value_expr_t::O_LTE:
  case value_expr_t::O_GT:
  case value_expr_t::O_GTE:
    out << "(";
    dump_value_expr(out, node->left);
    switch (node->kind) {
    case value_expr_t::O_EQ:  out << "="; break;
    case value_expr_t::O_LT:  out << "<"; break;
    case value_expr_t::O_LTE: out << "<="; break;
    case value_expr_t::O_GT:  out << ">"; break;
    case value_expr_t::O_GTE: out << ">="; break;
    default: assert(0); break;
    }
    dump_value_expr(out, node->right);
    out << ")";
    break;

  case value_expr_t::O_ADD:
  case value_expr_t::O_SUB:
  case value_expr_t::O_MUL:
  case value_expr_t::O_DIV:
    out << "(";
    dump_value_expr(out, node->left);
    switch (node->kind) {
    case value_expr_t::O_ADD: out << "+"; break;
    case value_expr_t::O_SUB: out << "-"; break;
    case value_expr_t::O_MUL: out << "*"; break;
    case value_expr_t::O_DIV: out << "/"; break;
    default: assert(0); break;
    }
    dump_value_expr(out, node->right);
    out << ")";
    break;

  case value_expr_t::LAST:
  default:
    assert(0);
    break;
  }
}

#endif // DEBUG_ENABLED

} // namespace ledger

#ifdef TEST

int main(int argc, char *argv[])
{
  ledger::dump_value_expr(std::cout, ledger::parse_value_expr(argv[1]));
  std::cout << std::endl;
}

#endif // TEST
