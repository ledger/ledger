#include "format.h"
#include "error.h"

namespace ledger {

std::string truncated(const std::string& str, unsigned int width)
{
  char buf[256];
  std::memset(buf, '\0', 255);
  assert(width < 256);
  std::strncpy(buf, str.c_str(), str.length());
  if (buf[width])
    std::strcpy(&buf[width - 2], "..");
  return buf;
}

std::string maximal_account_name(const item_t * item, const item_t * parent)
{
  std::string name = item->account->name;
  for (const item_t * i = item->parent;
       i && i->account && i != parent;
       i = i->parent)
    name = i->account->name + ":" + name;
  return name;
}

std::auto_ptr<node_t> format_t::value_expr;
std::auto_ptr<node_t> format_t::total_expr;

element_t * format_t::parse_elements(const std::string& fmt)
{
  element_t * result  = NULL;
  element_t * current = NULL;
  std::string str;

  for (const char * p = fmt.c_str(); *p; p++) {
    if (*p == '%') {
      if (! result) {
	current = result = new element_t;
      } else {
	current->next = new element_t;
	current = current->next;
      }

      if (! str.empty()) {
	current->type  = element_t::STRING;
	current->chars = str;
	str = "";

	current->next  = new element_t;
	current = current->next;
      }

      ++p;
      if (*p == '-') {
	current->align_left = true;
	++p;
      }

      std::string num;
      while (*p && std::isdigit(*p))
	num += *p++;
      if (! num.empty())
	current->min_width = std::atol(num.c_str());

      if (*p == '.') {
	++p;
	num = "";
	while (*p && std::isdigit(*p))
	  num += *p++;
	if (! num.empty()) {
	  current->max_width = std::atol(num.c_str());
	  if (current->min_width == 0)
	    current->min_width = current->max_width;
	}
      }

      switch (*p) {
      case '%':
	current->type  = element_t::STRING;
	current->chars = "%";
	break;

      case '(':
	++p;
	num = "";
	while (*p && *p != ')')
	  num += *p++;
	if (*p != ')')
	  throw format_error("Missing ')'");

	current->type     = element_t::VALUE_EXPR;
	current->val_expr = parse_expr(num);
	break;

      case '[':
	++p;
	num = "";
	while (*p && *p != ']')
	  num += *p++;
	if (*p != ']')
	  throw format_error("Missing ']'");

	current->type  = element_t::DATE_STRING;
	current->chars = num;
	break;

      case 'd':
	current->type  = element_t::DATE_STRING;
	current->chars = "%Y/%m/%d";
	break;

      case 'X': current->type = element_t::CLEARED; break;
      case 'C': current->type = element_t::CODE; break;
      case 'p': current->type = element_t::PAYEE; break;
      case 'n': current->type = element_t::ACCOUNT_NAME; break;
      case 'N': current->type = element_t::ACCOUNT_FULLNAME; break;
      case 'o': current->type = element_t::OPT_AMOUNT; break;
      case 't': current->type = element_t::VALUE; break;
      case 'T': current->type = element_t::TOTAL; break;
      case '_': current->type = element_t::SPACER; break;
      }
    } else {
      str += *p;
    }
  }

  if (! str.empty()) {
    if (! result) {
      current = result = new element_t;
    } else {
      current->next = new element_t;
      current = current->next;
    }
    current->type  = element_t::STRING;
    current->chars = str;
  }

  return result;
}

void format_t::format_elements(std::ostream& out, const item_t * item,
			       const item_t * displayed_parent) const
{
  std::string result;

  for (const element_t * elem = elements;
       elem;
       elem = elem->next) {
    if (elem->align_left)
      out << std::left;
    else
      out << std::right;

    if (elem->min_width > 0)
      out.width(elem->min_width);

    switch (elem->type) {
    case element_t::STRING:
      out << elem->chars;;
      break;

    case element_t::VALUE_EXPR: {
      balance_t value = elem->val_expr->compute(item);
      value.write(out, elem->min_width,
		  elem->max_width > 0 ? elem->max_width : elem->min_width);
      break;
    }

    case element_t::DATE_STRING:
      if (item->date != -1) {
	char buf[256];
	std::strftime(buf, 255, elem->chars.c_str(), std::gmtime(&item->date));
	out << (elem->max_width == 0 ? buf : truncated(buf, elem->max_width));
      } else {
	out << " ";
      }
      break;

    case element_t::CLEARED:
      if (item->state == entry_t::CLEARED)
	out << "* ";
      else
	out << "";
      break;

    case element_t::CODE:
      if (! item->code.empty())
	out << "(" << item->code << ") ";
      else
	out << "";
      break;

    case element_t::PAYEE:
      out << (elem->max_width == 0 ?
	      item->payee : truncated(item->payee, elem->max_width));
      break;

    case element_t::ACCOUNT_NAME:
    case element_t::ACCOUNT_FULLNAME:
      if (item->account) {
	std::string name = (elem->type == element_t::ACCOUNT_FULLNAME ?
			    item->account->fullname() :
			    maximal_account_name(item, displayed_parent));
	if (elem->max_width > 0)
	  name = truncated(name, elem->max_width);

	if (item->flags & TRANSACTION_VIRTUAL) {
	  if (item->flags & TRANSACTION_BALANCE)
	    name = "[" + name + "]";
	  else
	    name = "(" + name + ")";
	}
	out << name;
      } else {
	out << " ";
      }
      break;

    case element_t::OPT_AMOUNT: {
      std::string disp;
      bool        use_disp = false;

      if (std::find(displayed_parent->subitems.begin(),
		    displayed_parent->subitems.end(), item) !=
	  displayed_parent->subitems.end()) {
	if (displayed_parent->subitems.size() == 2 &&
	    item == displayed_parent->subitems.back() &&
	    (displayed_parent->subitems.front()->value.quantity ==
	     displayed_parent->subitems.front()->value.cost) &&
	    (displayed_parent->subitems.front()->value ==
	     - displayed_parent->subitems.back()->value)) {
	  use_disp = true;
	}
	else if (displayed_parent->subitems.size() != 2 &&
		 item->value.quantity != item->value.cost &&
		 item->value.quantity.amounts.size() == 1 &&
		 item->value.cost.amounts.size() == 1 &&
		 ((*item->value.quantity.amounts.begin()).first !=
		  (*item->value.cost.amounts.begin()).first)) {
	  amount_t unit_cost
	    = ((*item->value.cost.amounts.begin()).second /
	       (*item->value.quantity.amounts.begin()).second);
	  std::ostringstream stream;
	  stream << item->value.quantity << " @ " << unit_cost;
	  disp = stream.str();
	  use_disp = true;
	}
      }

      if (use_disp)
	out << disp;
      else
	item->value.quantity.write(out, elem->min_width,
				   elem->max_width > 0 ?
				   elem->max_width : elem->min_width);

      // jww (2004-07-31): this should be handled differently
      if (! item->note.empty())
	out << "  ; " << item->note;
      break;
    }

    case element_t::VALUE: {
      balance_t value = compute_value(item);
      value.write(out, elem->min_width,
		  elem->max_width > 0 ? elem->max_width : elem->min_width);
      break;
    }

    case element_t::TOTAL: {
      balance_t value = compute_total(item);
      value.write(out, elem->min_width,
		  elem->max_width > 0 ? elem->max_width : elem->min_width);
      break;
    }

    case element_t::SPACER: {
      int depth = 0;
      for (const item_t * i = item; i->parent; i = i->parent)
	depth++;

      for (const item_t * i = item->parent;
	   i && i->account && i != displayed_parent;
	   i = i->parent)
	depth--;

      while (--depth >= 0) {
	if (elem->min_width > 0 || elem->max_width > 0)
	  out.width(elem->min_width > elem->max_width ?
		    elem->min_width : elem->max_width);
	out << " ";
      }
      break;
    }

    default:
      assert(0);
      break;
    }
  }
}

} // namespace ledger
