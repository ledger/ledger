#include "expr.h"
#include "error.h"
#include "textual.h"

namespace ledger {

balance_t node_t::compute(const item_t *    item,
			  const std::time_t begin,
			  const std::time_t end) const
{
  balance_t temp;

  switch (type) {
  case CONSTANT_A:
    temp = constant_a;
    break;

  case CONSTANT_T:
    temp = amount_t((unsigned int) constant_t);
    break;

  case AMOUNT:
    temp = item->value.quantity;
    break;
  case COST:
    temp = item->value.cost;
    break;

  case BALANCE:
    temp = item->total.quantity - item->value.quantity;
    break;
  case COST_BALANCE:
    temp = item->total.cost - item->value.cost;
    break;

  case TOTAL:
    temp = item->total.quantity;
    break;
  case COST_TOTAL:
    temp = item->total.cost;
    break;

  case DATE:
    temp = amount_t((unsigned int) item->date);
    break;

  case INDEX:
    temp = amount_t(item->index + 1);
    break;

  case BEGIN_DATE:
    temp = amount_t((unsigned int) begin);
    break;

  case END_DATE:
    temp = amount_t((unsigned int) end);
    break;

  case F_ARITH_MEAN:
    assert(left);
    temp = left->compute(item, begin, end);
    temp /= amount_t(item->index + 1);
    break;

  case F_NEG:
    assert(left);
    temp = left->compute(item, begin, end).negated();
    break;

  case F_ABS:
    assert(left);
    temp = abs(left->compute(item, begin, end));
    break;

  case F_REGEXP:
    assert(mask);
    temp = (item->account &&
	    mask->match(item->account->fullname())) ? 1 : 0;
    break;

  case F_VALUE: {
    assert(left);
    temp = left->compute(item, begin, end);

    std::time_t moment = -1;
    if (right) {
      switch (right->type) {
      case DATE:       moment = item->date; break;
      case BEGIN_DATE: moment = begin; break;
      case END_DATE:   moment = end; break;
      default:
	throw compute_error("Invalid date passed to P(v,d)");
      }
    }
    temp = temp.value(moment);
    break;
  }

  case O_NOT:
    temp = left->compute(item, begin, end) ? 0 : 1;
    break;

  case O_QUES:
    temp = left->compute(item, begin, end);
    if (temp)
      temp = right->left->compute(item, begin, end);
    else
      temp = right->right->compute(item, begin, end);
    break;

  case O_AND:
  case O_OR:
  case O_EQ:
  case O_LT:
  case O_LTE:
  case O_GT:
  case O_GTE:
  case O_ADD:
  case O_SUB:
  case O_MUL:
  case O_DIV: {
    assert(left);
    assert(right);
    balance_t left_bal  = left->compute(item, begin, end);
    balance_t right_bal = right->compute(item, begin, end);
    switch (type) {
    case O_AND: temp = (left_bal && right_bal) ? 1 : 0; break;
    case O_OR:  temp = (left_bal || right_bal) ? 1 : 0; break;
    case O_EQ:  temp = (left_bal == right_bal) ? 1 : 0; break;
    case O_LT:  temp = (left_bal <  right_bal) ? 1 : 0; break;
    case O_LTE: temp = (left_bal <= right_bal) ? 1 : 0; break;
    case O_GT:  temp = (left_bal >  right_bal) ? 1 : 0; break;
    case O_GTE: temp = (left_bal >= right_bal) ? 1 : 0; break;
    case O_ADD: temp = left_bal + right_bal; break;
    case O_SUB: temp = left_bal - right_bal; break;
    case O_MUL: temp = left_bal * right_bal; break;
    case O_DIV: temp = left_bal / right_bal; break;
    default: assert(0); break;
    }
    break;
  }

  case LAST:
  default:
    assert(0);
    break;
  }

  return temp;
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
  case 'b': node = new node_t(node_t::BEGIN_DATE); break;
  case 'e': node = new node_t(node_t::END_DATE); break;
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

    c = in.peek();
    while (! in.eof() && c != '/') {
      in.get(c);
      if (c == '\\')
	in.get(c);
      ident += c;
      c = in.peek();
    }
    if (c == '/') {
      in.get(c);
      node = new node_t(node_t::F_REGEXP);
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
  case node_t::CONSTANT_A:   out << "CONST[" << node->constant_a << "]"; break;
  case node_t::CONSTANT_T:   out << "DATE/TIME[" << node->constant_t << "]"; break;
  case node_t::AMOUNT:	     out << "AMOUNT"; break;
  case node_t::COST:	     out << "COST"; break;
  case node_t::DATE:	     out << "DATE"; break;
  case node_t::INDEX:	     out << "INDEX"; break;
  case node_t::BALANCE:      out << "BALANCE"; break;
  case node_t::COST_BALANCE: out << "COST_BALANCE"; break;
  case node_t::TOTAL:        out << "TOTAL"; break;
  case node_t::COST_TOTAL:   out << "COST_TOTAL"; break;
  case node_t::BEGIN_DATE:   out << "BEGIN"; break;
  case node_t::END_DATE:     out << "END"; break;

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

  case node_t::F_REGEXP:
    assert(node->mask);
    out << "RE(" << node->mask->pattern << ")";
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
  case node_t::O_EQ:
  case node_t::O_LT:
  case node_t::O_LTE:
  case node_t::O_GT:
  case node_t::O_GTE:
  case node_t::O_ADD:
  case node_t::O_SUB:
  case node_t::O_MUL:
  case node_t::O_DIV:
    out << "(";
    dump_tree(out, node->left);
    switch (node->type) {
    case node_t::O_AND: out << " & "; break;
    case node_t::O_OR:  out << " | "; break;
    case node_t::O_EQ:  out << "="; break;
    case node_t::O_LT:  out << "<"; break;
    case node_t::O_LTE: out << "<="; break;
    case node_t::O_GT:  out << ">"; break;
    case node_t::O_GTE: out << ">="; break;
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
  ledger::dump_tree(std::cout, ledger::parse_expr(argv[1], NULL));
  std::cout << std::endl;
}

#endif // TEST
