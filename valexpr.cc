#include "valexpr.h"
#include "error.h"
#include "datetime.h"
#include "debug.h"
#include "util.h"

#include <vector>

#include <pcre.h>

namespace ledger {

mask_t::mask_t(const std::string& pat) : exclude(false)
{
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
  const char *error;
  int erroffset;
  regexp = pcre_compile(pattern.c_str(), PCRE_CASELESS,
			&error, &erroffset, NULL);
  assert(regexp);
}

bool mask_t::match(const std::string& str) const
{
  static int ovec[30];
  int result = pcre_exec((pcre *)regexp, NULL,
			 str.c_str(), str.length(), 0, 0, ovec, 30);
  return result >= 0 && ! exclude;
}

mask_t::~mask_t() {
  pcre_free((pcre *)regexp);
}


void value_expr_t::compute(balance_t& result, const details_t& details) const
{
  switch (type) {
  case CONSTANT_A:
    result = constant_a;
    break;

  case CONSTANT_T:
    result = (unsigned int) constant_t;
    break;

  case AMOUNT:
    if (details.xact)
      result = details.xact->amount;
    else if (details.account)
      result = details.account->value.quantity;
    break;

  case COST:
    if (details.xact)
      result = details.xact->cost;
    else if (details.account)
      result = details.account->value.cost;
    break;

  case BALANCE:
    if (details.xact) {
      result = details.xact->total.quantity;
      result -= details.xact->amount;
    }
    else if (details.account) {
      result = details.account->total.quantity;
      result -= details.account->value.quantity;
    }
    break;

  case COST_BALANCE:
    if (details.xact) {
      result = details.xact->total.cost;
      result -= details.xact->cost;
    }
    else if (details.account) {
      result = details.account->total.cost;
      result -= details.account->value.cost;
    }
    break;

  case TOTAL:
    if (details.xact)
      result = details.xact->total.quantity;
    else if (details.account)
      result = details.account->total.quantity;
    break;
  case COST_TOTAL:
    if (details.xact)
      result = details.xact->total.cost;
    else if (details.account)
      result = details.account->total.cost;
    break;

  case DATE:
    if (details.entry)
      result = (unsigned int) details.entry->date;
    else
      result = (unsigned int) std::time(NULL);
    break;

  case TODAY:
    result = (unsigned int) std::time(NULL);
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
    if (details.xact)
      result = details.xact->index + 1;
    else if (details.account)
      result = details.account->depth - 1;
    break;

  case F_ARITH_MEAN:
    if (details.xact) {
      assert(left);
      left->compute(result, details);
      result /= amount_t(details.xact->index + 1);
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
    result = abs(result);
    break;

  case F_STRIP: {
    assert(left);
    left->compute(result, details);
    amount_t amt = result.amount();
    amt.commodity = commodity_t::null_commodity;
    result = amt;
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

  case F_VALUE: {
    assert(left);
    left->compute(result, details);

    std::time_t moment = -1;
    if (right) {
      switch (right->type) {
      case DATE:
	if (details.entry)
	  moment = details.entry->date;
	else
	  moment = std::time(NULL);
	break;

      case TODAY:
	moment = std::time(NULL);
	break;

      default:
	throw compute_error("Invalid date passed to P(value,date)");
      }
    } else {
      moment = std::time(NULL);
    }
    result = result.value(moment);
    break;
  }

  case O_NOT:
    left->compute(result, details);
    result = result ? false : true;
    break;

  case O_QUES:
    assert(left);
    assert(right);
    assert(right->type == O_COL);
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
    left->compute(result, details);
    balance_t temp = result;
    result = 0;
    right->compute(result, details);
    switch (type) {
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
    right->compute(result, details);
    balance_t temp = result;
    result = 0;
    left->compute(result, details);
    switch (type) {
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
  value_expr_t * node = NULL;

  char c = peek_next_nonws(in);
  if (std::isdigit(c) || c == '.' || c == '{') {
    static char buf[2048];
    if (c == '{') {
      in.get(c);
      READ_INTO(in, buf, 2048, c, c != '}');
      if (c == '}')
	in.get(c);
      else
	throw value_expr_error("Missing '}'");
    } else {
      READ_INTO(in, buf, 2048, c, std::isdigit(c) || c == '.');
    }

    node = new value_expr_t(value_expr_t::CONSTANT_A);
    node->constant_a.parse(buf);
    return node;
  }

  in.get(c);
  switch (c) {
  // Basic terms
  case 'a': node = new value_expr_t(value_expr_t::AMOUNT); break;
  case 'c': node = new value_expr_t(value_expr_t::COST); break;
  case 'd': node = new value_expr_t(value_expr_t::DATE); break;
  case 't': node = new value_expr_t(value_expr_t::TODAY); break;
  case 'X': node = new value_expr_t(value_expr_t::CLEARED); break;
  case 'R': node = new value_expr_t(value_expr_t::REAL); break;
  case 'n': node = new value_expr_t(value_expr_t::INDEX); break;
  case 'B': node = new value_expr_t(value_expr_t::BALANCE); break;
  case 'T': node = new value_expr_t(value_expr_t::TOTAL); break;
  case 'C': node = new value_expr_t(value_expr_t::COST_TOTAL); break;

  // Compound terms
  case 'v': node = parse_value_expr("P(a,d)"); break;
  case 'V': node = parse_value_term("P(T,d)"); break;
  case 'g': node = parse_value_expr("v-c"); break;
  case 'G': node = parse_value_expr("V-C"); break;
  case 'o': node = parse_value_expr("d-b"); break;
  case 'w': node = parse_value_expr("e-d"); break;

  // Functions
  case '-':
    node = new value_expr_t(value_expr_t::F_NEG);
    node->left = parse_value_term(in);
    break;

  case 'A':
    node = new value_expr_t(value_expr_t::F_ABS);
    node->left = parse_value_term(in);
    break;

  case 'S':
    node = new value_expr_t(value_expr_t::F_STRIP);
    node->left = parse_value_term(in);
    break;

  case 'M':
    node = new value_expr_t(value_expr_t::F_ARITH_MEAN);
    node->left = parse_value_term(in);
    break;

  case 'D': {
    node = new value_expr_t(value_expr_t::O_SUB);
    node->left  = parse_value_term("a");
    node->right = parse_value_term(in);
    break;
  }

  case 'P':
    node = new value_expr_t(value_expr_t::F_VALUE);
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
    bool        payee_mask = false;

    c = peek_next_nonws(in);
    if (c == '/') {
      payee_mask = true;
      in.get(c);
      c = in.peek();
    }

    static char buf[4096];
    READ_INTO(in, buf, 4096, c, c != '/');
    if (c != '/')
      throw value_expr_error("Missing closing '/'");

    in.get(c);
    node = new value_expr_t(payee_mask ? value_expr_t::F_PAYEE_MASK :
			    value_expr_t::F_ACCOUNT_MASK);
    node->mask = new mask_t(buf);
    break;
  }

  case '(':
    node = parse_value_expr(in);
    if (peek_next_nonws(in) == ')')
      in.get(c);
    else
      throw value_expr_error("Missing ')'");
    break;

  case '[': {
    static char buf[1024];
    READ_INTO(in, buf, 1024, c, c != ']');
    if (c != ']')
      throw value_expr_error("Missing ']'");

    in.get(c);
    node = new value_expr_t(value_expr_t::CONSTANT_T);
    if (! parse_date(buf, &node->constant_t))
      throw value_expr_error("Failed to parse date");
    break;
  }

  default:
    in.unget();
    break;
  }

  return node;
}

value_expr_t * parse_mul_expr(std::istream& in)
{
  value_expr_t * node = NULL;

  node = parse_value_term(in);

  if (node && ! in.eof()) {
    char c = peek_next_nonws(in);
    while (c == '*' || c == '/') {
      in.get(c);
      switch (c) {
      case '*': {
	value_expr_t * prev = node;
	node = new value_expr_t(value_expr_t::O_MUL);
	node->left  = prev;
	node->right = parse_value_term(in);
	break;
      }

      case '/': {
	value_expr_t * prev = node;
	node = new value_expr_t(value_expr_t::O_DIV);
	node->left  = prev;
	node->right = parse_value_term(in);
	break;
      }
      }
      c = peek_next_nonws(in);
    }
  }

  return node;
}

value_expr_t * parse_add_expr(std::istream& in)
{
  value_expr_t * node = NULL;

  node = parse_mul_expr(in);

  if (node && ! in.eof()) {
    char c = peek_next_nonws(in);
    while (c == '+' || c == '-') {
      in.get(c);
      switch (c) {
      case '+': {
	value_expr_t * prev = node;
	node = new value_expr_t(value_expr_t::O_ADD);
	node->left  = prev;
	node->right = parse_mul_expr(in);
	break;
      }

      case '-': {
	value_expr_t * prev = node;
	node = new value_expr_t(value_expr_t::O_SUB);
	node->left  = prev;
	node->right = parse_mul_expr(in);
	break;
      }
      }
      c = peek_next_nonws(in);
    }
  }

  return node;
}

value_expr_t * parse_logic_expr(std::istream& in)
{
  value_expr_t * node = NULL;

  if (peek_next_nonws(in) == '!') {
    char c;
    in.get(c);
    node = new value_expr_t(value_expr_t::O_NOT);
    node->left = parse_logic_expr(in);
    return node;
  }

  node = parse_add_expr(in);

  if (node && ! in.eof()) {
    char c = peek_next_nonws(in);
    if (c == '=' || c == '<' || c == '>') {
      in.get(c);
      switch (c) {
      case '=': {
	value_expr_t * prev = node;
	node = new value_expr_t(value_expr_t::O_EQ);
	node->left  = prev;
	node->right = parse_add_expr(in);
	break;
      }

      case '<': {
	value_expr_t * prev = node;
	node = new value_expr_t(value_expr_t::O_LT);
	if (peek_next_nonws(in) == '=') {
	  in.get(c);
	  node->type = value_expr_t::O_LTE;
	}
	node->left  = prev;
	node->right = parse_add_expr(in);
	break;
      }

      case '>': {
	value_expr_t * prev = node;
	node = new value_expr_t(value_expr_t::O_GT);
	if (peek_next_nonws(in) == '=') {
	  in.get(c);
	  node->type = value_expr_t::O_GTE;
	}
	node->left  = prev;
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

  return node;
}

value_expr_t * parse_value_expr(std::istream& in)
{
  value_expr_t * node = NULL;

  node = parse_logic_expr(in);

  if (node && ! in.eof()) {
    char c = peek_next_nonws(in);
    while (c == '&' || c == '|' || c == '?') {
      in.get(c);
      switch (c) {
      case '&': {
	value_expr_t * prev = node;
	node = new value_expr_t(value_expr_t::O_AND);
	node->left  = prev;
	node->right = parse_logic_expr(in);
	break;
      }

      case '|': {
	value_expr_t * prev = node;
	node = new value_expr_t(value_expr_t::O_OR);
	node->left  = prev;
	node->right = parse_logic_expr(in);
	break;
      }

      case '?': {
	value_expr_t * prev = node;
	node = new value_expr_t(value_expr_t::O_QUES);
	node->left  = prev;
	value_expr_t * choices = new value_expr_t(value_expr_t::O_COL);
	node->right = choices;
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

  return node;
}

std::string regexps_to_predicate(std::list<std::string>::const_iterator begin,
				 std::list<std::string>::const_iterator end,
				 const bool account_regexp)
{
  std::vector<std::string> regexps(2);
  std::string pred;

  // Treat the remaining command-line arguments as regular
  // expressions, used for refining report results.

  for (std::list<std::string>::const_iterator i = begin;
       i != end;
       i++)
    if ((*i)[0] == '-') {
      if (! regexps[1].empty())
	regexps[1] += "|";
      regexps[1] += (*i).substr(1);
    } else {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += *i;
    }

  for (std::vector<std::string>::const_iterator i = regexps.begin();
       i != regexps.end();
       i++)
    if (! (*i).empty()) {
      if (! pred.empty())
	pred += "&";
      if (i != regexps.begin())
	pred += "!";
      if (! account_regexp)
	pred += "/";
      pred += "/(?:";
      pred += *i;
      pred += ")/";
    }

  return pred;
}

#ifdef DEBUG_ENABLED

void dump_value_expr(std::ostream& out, const value_expr_t * node)
{
  switch (node->type) {
  case value_expr_t::CONSTANT_A:
    out << "CONST[" << node->constant_a << "]";
    break;
  case value_expr_t::CONSTANT_T:
    out << "DATE/TIME[" << node->constant_t << "]";
    break;

  case value_expr_t::AMOUNT:	   out << "AMOUNT"; break;
  case value_expr_t::COST:	   out << "COST"; break;
  case value_expr_t::DATE:	   out << "DATE"; break;
  case value_expr_t::TODAY:	   out << "TODAY"; break;
  case value_expr_t::CLEARED:	   out << "CLEARED"; break;
  case value_expr_t::REAL:	   out << "REAL"; break;
  case value_expr_t::INDEX:	   out << "INDEX"; break;
  case value_expr_t::BALANCE:      out << "BALANCE"; break;
  case value_expr_t::COST_BALANCE: out << "COST_BALANCE"; break;
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
    switch (node->type) {
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
    switch (node->type) {
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
    switch (node->type) {
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
