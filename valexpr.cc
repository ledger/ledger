#include "valexpr.h"
#include "error.h"
#include "datetime.h"

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

#if 1

bool matches(const masks_list& regexps, const std::string& str,
	     bool * by_exclusion)
{
  if (regexps.empty())
    return false;

  bool match    = false;
  bool definite = false;

  for (masks_list::const_iterator r = regexps.begin();
       r != regexps.end();
       r++) {
    static int ovec[30];
    int result = pcre_exec((pcre *)(*r).regexp, NULL,
			   str.c_str(), str.length(), 0, 0, ovec, 30);
    if (result >= 0) {
      match     = ! (*r).exclude;
      definite  = true;
    }
    else if ((*r).exclude) {
      if (! match)
	match = ! definite;
    }
    else {
      definite = true;
    }
  }

  if (by_exclusion)
    *by_exclusion = match && ! definite && by_exclusion;

  return match;
}

#endif

void node_t::compute(balance_t& result, const details_t& details) const
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
    break;

  case CLEARED:
    if (details.entry)
      result = details.entry->state == entry_t::CLEARED;
    break;

  case REAL:
    if (details.xact)
      result = ! (details.xact->flags & TRANSACTION_VIRTUAL);
    break;

  case INDEX:
    if (details.xact)
      result = details.xact->index + 1;
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
    if (right && details.entry) {
      switch (right->type) {
      case DATE: moment = details.entry->date; break;
      default:
	throw compute_error("Invalid date passed to P(v,d)");
      }
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

node_t * parse_term(std::istream& in);

inline node_t * parse_term(const char * p) {
  std::istringstream stream(p);
  return parse_term(stream);
}

node_t * parse_term(std::istream& in)
{
  node_t * node = NULL;

  char c = in.peek();
  if (std::isdigit(c) || c == '.' || c == '{') {
    std::string ident;

    if (c == '{') {
      in.get(c);
      c = in.peek();
      while (! in.eof() && c != '}') {
	in.get(c);
	ident += c;
	c = in.peek();
      }
      if (c == '}')
	in.get(c);
      else
	throw expr_error("Missing '}'");
    } else {
      while (! in.eof() && std::isdigit(c) || c == '.') {
	in.get(c);
	ident += c;
	c = in.peek();
      }
    }

    if (! ident.empty()) {
      node = new node_t(node_t::CONSTANT_A);
      node->constant_a.parse(ident);
    }
    return node;
  }

  in.get(c);
  switch (c) {
  // Basic terms
  case 'a': node = new node_t(node_t::AMOUNT); break;
  case 'c': node = new node_t(node_t::COST); break;
  case 'd': node = new node_t(node_t::DATE); break;
  case 'X': node = new node_t(node_t::CLEARED); break;
  case 'R': node = new node_t(node_t::REAL); break;
  case 'i': node = new node_t(node_t::INDEX); break;
  case 'B': node = new node_t(node_t::BALANCE); break;
  case 'T': node = new node_t(node_t::TOTAL); break;
  case 'C': node = new node_t(node_t::COST_TOTAL); break;

  // Compound terms
  case 'v': node = parse_expr("P(a,d)"); break;
  case 'V': node = parse_term("P(T,d)"); break;
  case 'g': node = parse_expr("v-c"); break;
  case 'G': node = parse_expr("V-C"); break;
  case 'o': node = parse_expr("d-b"); break;
  case 'w': node = parse_expr("e-d"); break;

  // Functions
  case '-':
    node = new node_t(node_t::F_NEG);
    node->left = parse_term(in);
    break;

  case 'A':
    node = new node_t(node_t::F_ABS);
    node->left = parse_term(in);
    break;

  case 'M':
    node = new node_t(node_t::F_ARITH_MEAN);
    node->left = parse_term(in);
    break;

  case 'D': {
    node = new node_t(node_t::O_SUB);
    node->left  = parse_term("a");
    node->right = parse_term(in);
    break;
  }

  case 'P':
    node = new node_t(node_t::F_VALUE);
    if (in.peek() == '(') {
      in.get(c);
      node->left = parse_expr(in);
      if (in.peek() == ',') {
	in.get(c);
	node->right = parse_expr(in);
      }
      if (in.peek() == ')')
	in.get(c);
      else
	throw expr_error("Missing ')'");
    } else {
      node->left = parse_term(in);
    }
    break;

  // Other
  case '/': {
    std::string ident;
    bool        payee_mask = false;

    c = in.peek();
    if (c == '/') {
      payee_mask = true;
      in.get(c);
      c = in.peek();
    }

    while (! in.eof() && c != '/') {
      in.get(c);
      if (c == '\\')
	in.get(c);
      ident += c;
      c = in.peek();
    }

    if (c == '/') {
      in.get(c);
      node = new node_t(payee_mask ?
			node_t::F_PAYEE_MASK : node_t::F_ACCOUNT_MASK);
      node->mask = new mask_t(ident);
    } else {
      throw expr_error("Missing closing '/'");
    }
    break;
  }

  case '(':
    node = parse_expr(in);
    if (in.peek() == ')')
      in.get(c);
    else
      throw expr_error("Missing ')'");
    break;

  case '[': {
    std::string ident;

    c = in.peek();
    while (! in.eof() && c != ']') {
      in.get(c);
      ident += c;
      c = in.peek();
    }
    if (c == ']') {
      in.get(c);
      node = new node_t(node_t::CONSTANT_T);
      if (! parse_date(ident.c_str(), &node->constant_t))
	throw expr_error("Failed to parse date");
    } else {
      throw expr_error("Missing ']'");
    }
    break;
  }

  default:
    in.unget();
    break;
  }

  return node;
}

node_t * parse_mul_expr(std::istream& in)
{
  node_t * node = NULL;

  node = parse_term(in);

  if (node && ! in.eof()) {
    char c = in.peek();
    while (c == '*' || c == '/') {
      in.get(c);
      switch (c) {
      case '*': {
	node_t * prev = node;
	node = new node_t(node_t::O_MUL);
	node->left  = prev;
	node->right = parse_term(in);
	break;
      }

      case '/': {
	node_t * prev = node;
	node = new node_t(node_t::O_DIV);
	node->left  = prev;
	node->right = parse_term(in);
	break;
      }
      }
      c = in.peek();
    }
  }

  return node;
}

node_t * parse_add_expr(std::istream& in)
{
  node_t * node = NULL;

  node = parse_mul_expr(in);

  if (node && ! in.eof()) {
    char c = in.peek();
    while (c == '+' || c == '-') {
      in.get(c);
      switch (c) {
      case '+': {
	node_t * prev = node;
	node = new node_t(node_t::O_ADD);
	node->left  = prev;
	node->right = parse_mul_expr(in);
	break;
      }

      case '-': {
	node_t * prev = node;
	node = new node_t(node_t::O_SUB);
	node->left  = prev;
	node->right = parse_mul_expr(in);
	break;
      }
      }
      c = in.peek();
    }
  }

  return node;
}

node_t * parse_logic_expr(std::istream& in)
{
  node_t * node = NULL;

  if (in.peek() == '!') {
    char c;
    in.get(c);
    node = new node_t(node_t::O_NOT);
    node->left = parse_logic_expr(in);
    return node;
  }

  node = parse_add_expr(in);

  if (node && ! in.eof()) {
    char c = in.peek();
    if (c == '=' || c == '<' || c == '>') {
      in.get(c);
      switch (c) {
      case '=': {
	node_t * prev = node;
	node = new node_t(node_t::O_EQ);
	node->left  = prev;
	node->right = parse_add_expr(in);
	break;
      }

      case '<': {
	node_t * prev = node;
	node = new node_t(node_t::O_LT);
	if (in.peek() == '=') {
	  in.get(c);
	  node->type = node_t::O_LTE;
	}
	node->left  = prev;
	node->right = parse_add_expr(in);
	break;
      }

      case '>': {
	node_t * prev = node;
	node = new node_t(node_t::O_GT);
	if (in.peek() == '=') {
	  in.get(c);
	  node->type = node_t::O_GTE;
	}
	node->left  = prev;
	node->right = parse_add_expr(in);
	break;
      }

      default:
	if (! in.eof()) {
	  std::ostringstream err;
	  err << "Unexpected character '" << c << "'";
	  throw expr_error(err.str());
	}
      }
    }
  }

  return node;
}

node_t * parse_expr(std::istream& in)
{
  node_t * node = NULL;

  node = parse_logic_expr(in);

  if (node && ! in.eof()) {
    char c = in.peek();
    while (c == '&' || c == '|' || c == '?') {
      in.get(c);
      switch (c) {
      case '&': {
	node_t * prev = node;
	node = new node_t(node_t::O_AND);
	node->left  = prev;
	node->right = parse_logic_expr(in);
	break;
      }

      case '|': {
	node_t * prev = node;
	node = new node_t(node_t::O_OR);
	node->left  = prev;
	node->right = parse_logic_expr(in);
	break;
      }

      case '?': {
	node_t * prev = node;
	node = new node_t(node_t::O_QUES);
	node->left  = prev;
	node_t * choices = new node_t(node_t::O_COL);
	node->right = choices;
	choices->left = parse_logic_expr(in);
	c = in.peek();
	if (c != ':') {
	  std::ostringstream err;
	  err << "Unexpected character '" << c << "'";
	  throw expr_error(err.str());
	}
	in.get(c);
	choices->right = parse_logic_expr(in);
	break;
      }

      default:
	if (! in.eof()) {
	  std::ostringstream err;
	  err << "Unexpected character '" << c << "'";
	  throw expr_error(err.str());
	}
      }
      c = in.peek();
    }
  }

  return node;
}

} // namespace ledger


#ifdef TEST

namespace ledger {

static void dump_tree(std::ostream& out, node_t * node)
{
  switch (node->type) {
  case node_t::CONSTANT_A:
    out << "CONST[" << node->constant_a << "]";
    break;
  case node_t::CONSTANT_T:
    out << "DATE/TIME[" << node->constant_t << "]";
    break;

  case node_t::AMOUNT:	     out << "AMOUNT"; break;
  case node_t::COST:	     out << "COST"; break;
  case node_t::DATE:	     out << "DATE"; break;
  case node_t::CLEARED:	     out << "CLEARED"; break;
  case node_t::REAL:	     out << "REAL"; break;
  case node_t::INDEX:	     out << "INDEX"; break;
  case node_t::BALANCE:      out << "BALANCE"; break;
  case node_t::COST_BALANCE: out << "COST_BALANCE"; break;
  case node_t::TOTAL:        out << "TOTAL"; break;
  case node_t::COST_TOTAL:   out << "COST_TOTAL"; break;

  case node_t::F_ARITH_MEAN:
    out << "MEAN(";
    dump_tree(out, node->left);
    out << ")";
    break;

  case node_t::F_NEG:
    out << "ABS(";
    dump_tree(out, node->left);
    out << ")";
    break;

  case node_t::F_ABS:
    out << "ABS(";
    dump_tree(out, node->left);
    out << ")";
    break;

  case node_t::F_PAYEE_MASK:
    assert(node->mask);
    out << "P_MASK(" << node->mask->pattern << ")";
    break;

  case node_t::F_ACCOUNT_MASK:
    assert(node->mask);
    out << "A_MASK(" << node->mask->pattern << ")";
    break;

  case node_t::F_VALUE:
    out << "VALUE(";
    dump_tree(out, node->left);
    if (node->right) {
      out << ", ";
      dump_tree(out, node->right);
    }
    out << ")";
    break;

  case node_t::O_NOT:
    out << "!";
    dump_tree(out, node->left);
    break;

  case node_t::O_QUES:
    dump_tree(out, node->left);
    out << "?";
    dump_tree(out, node->right->left);
    out << ":";
    dump_tree(out, node->right->right);
    break;

  case node_t::O_AND:
  case node_t::O_OR:
    out << "(";
    dump_tree(out, node->left);
    switch (node->type) {
    case node_t::O_AND: out << " & "; break;
    case node_t::O_OR:  out << " | "; break;
    default: assert(0); break;
    }
    dump_tree(out, node->right);
    out << ")";
    break;

  case node_t::O_EQ:
  case node_t::O_LT:
  case node_t::O_LTE:
  case node_t::O_GT:
  case node_t::O_GTE:
    out << "(";
    dump_tree(out, node->left);
    switch (node->type) {
    case node_t::O_EQ:  out << "="; break;
    case node_t::O_LT:  out << "<"; break;
    case node_t::O_LTE: out << "<="; break;
    case node_t::O_GT:  out << ">"; break;
    case node_t::O_GTE: out << ">="; break;
    default: assert(0); break;
    }
    dump_tree(out, node->right);
    out << ")";
    break;

  case node_t::O_ADD:
  case node_t::O_SUB:
  case node_t::O_MUL:
  case node_t::O_DIV:
    out << "(";
    dump_tree(out, node->left);
    switch (node->type) {
    case node_t::O_ADD: out << "+"; break;
    case node_t::O_SUB: out << "-"; break;
    case node_t::O_MUL: out << "*"; break;
    case node_t::O_DIV: out << "/"; break;
    default: assert(0); break;
    }
    dump_tree(out, node->right);
    out << ")";
    break;

  case node_t::LAST:
  default:
    assert(0);
    break;
  }
}

} // namespace ledger

int main(int argc, char *argv[])
{
  ledger::dump_tree(std::cout, ledger::parse_expr(argv[1]));
  std::cout << std::endl;
}

#endif // TEST
