#include "format.h"
#include "error.h"
#include "util.h"

#include <cstdlib>

namespace ledger {

format_t::elision_style_t
     format_t::elision_style = ABBREVIATE;
int  format_t::abbrev_length = 2;

bool format_t::ansi_codes    = false;
bool format_t::ansi_invert   = false;

namespace {
  string partial_account_name(const account_t& account)
  {
    string name;

    for (const account_t * acct = &account;
	 acct && acct->parent;
	 acct = acct->parent) {
      if (account_has_xdata(*acct) &&
	  account_xdata_(*acct).dflags & ACCOUNT_DISPLAYED)
	break;

      if (name.empty())
	name = acct->name;
      else
	name = acct->name + ":" + name;
    }

    return name;
  }
}

format_t::element_t * format_t::parse_elements(const string& fmt)
{
  std::auto_ptr<element_t> result;

  element_t * current = NULL;

  char   buf[1024];
  char * q = buf;

  // The following format codes need to be implemented as functions:
  //
  //   d: COMPLETE_DATE_STRING
  //   D: DATE_STRING
  //   S: SOURCE; break
  //   B: ENTRY_BEG_POS
  //   b: ENTRY_BEG_LINE
  //   E: ENTRY_END_POS
  //   e: ENTRY_END_LINE
  //   X: CLEARED
  //   Y: ENTRY_CLEARED
  //   C: CODE
  //   P: PAYEE
  //   W: OPT_ACCOUNT
  //   a: ACCOUNT_NAME
  //   A: ACCOUNT_FULLNAME
  //   t: AMOUNT
  //   o: OPT_AMOUNT
  //   T: TOTAL
  //   N: NOTE
  //   n: OPT_NOTE
  //   |: SPACER
  //   _: DEPTH_SPACER
  //   
  //   xB: XACT_BEG_POS
  //   xb: XACT_BEG_LINE
  //   xE: XACT_END_POS
  //   xe: XACT_END_LINE

  for (const char * p = fmt.c_str(); *p; p++) {
    if (*p != '%' && *p != '\\') {
      *q++ = *p;
      continue;
    }

    if (! result.get()) {
      result.reset(new element_t);
      current = result.get();
    } else {
      current->next.reset(new element_t);
      current = current->next.get();
    }

    if (q != buf) {
      current->type  = element_t::STRING;
      current->chars = string(buf, q);
      q = buf;

      current->next.reset(new element_t);
      current = current->next.get();
    }

    if (*p == '\\') {
      p++;
      current->type = element_t::STRING;
      switch (*p) {
      case 'b': current->chars = "\b"; break;
      case 'f': current->chars = "\f"; break;
      case 'n': current->chars = "\n"; break;
      case 'r': current->chars = "\r"; break;
      case 't': current->chars = "\t"; break;
      case 'v': current->chars = "\v"; break;
      }
      continue;
    }

    ++p;
    while (*p == '!' || *p == '-') {
      switch (*p) {
      case '-':
	current->flags |= ELEMENT_ALIGN_LEFT;
	break;
      case '!':
	current->flags |= ELEMENT_HIGHLIGHT;
	break;
      }
      ++p;
    }

    int num = 0;
    while (*p && std::isdigit(*p)) {
      num *= 10;
      num += *p++ - '0';
    }
    current->min_width = num;

    if (*p == '.') {
      ++p;
      num = 0;
      while (*p && std::isdigit(*p)) {
	num *= 10;
	num += *p++ - '0';
      }
      current->max_width = num;
      if (current->min_width == 0)
	current->min_width = current->max_width;
    }

    switch (*p) {
    case '%':
      current->type  = element_t::STRING;
      current->chars = "%";
      break;

    case '(':
    case '[': {
      std::istringstream str(p);
      current->type = element_t::EXPR;
      current->expr.parse(str);
      current->expr.set_text(string(p, p + str.tellg()));
      p += str.tellg();
      break;
    }

    default:
      current->type = element_t::EXPR;
      current->expr.parse(string("format_") + *p);
      break;
    }
  }

  if (q != buf) {
    if (! result.get()) {
      result.reset(new element_t);
      current = result.get();
    } else {
      current->next.reset(new element_t);
      current = current->next.get();
    }
    current->type  = element_t::STRING;
    current->chars = string(buf, q);
  }

  return result.release();
}

namespace {
  inline void mark_plain(std::ostream& out) {
    out << "\e[0m";
  }
}

void format_t::format(std::ostream& out_str, scope_t& scope) const
{
  for (const element_t * elem = elements.get(); elem; elem = elem->next.get()) {
    std::ostringstream out;
    string name;
    bool ignore_max_width = false;

    if (elem->flags & ELEMENT_ALIGN_LEFT)
      out << std::left;
    else
      out << std::right;

    if (elem->min_width > 0)
      out.width(elem->min_width);

    switch (elem->type) {
    case element_t::STRING:
      out << elem->chars;
      break;

    case element_t::EXPR:
      out << elem->expr.calc(scope);
      break;

#if 0
    case element_t::ACCOUNT_FULLNAME:
      scope.resolve("account").dump(out, elem->min_width);
      break;
    case element_t::ACCOUNT_NAME:
      scope.resolve("account_base").dump(out, elem->min_width);
      break;

    case element_t::AMOUNT:
      out << "a";
      //out << scope.resolve("amount");
      break;
    case element_t::TOTAL:
      out << "T";
      //out << scope.resolve("total");
      break;

    case element_t::VALUE_EXPR: {
      expr_t * calc;
      switch (elem->type) {
      case element_t::AMOUNT:
	assert(value_expr::amount_expr.get());
	calc = value_expr::amount_expr.get();
	break;
      case element_t::TOTAL:
	assert(value_expr::total_expr.get());
	calc = value_expr::total_expr.get();
	break;
      case element_t::VALUE_EXPR:
	calc = const_cast<value_expr *>(&elem->val_expr);
	break;
      default:
	assert(false);
	break;
      }
      if (! calc)
	break;

      value_t		value;
      const balance_t * bal = NULL;

      calc->compute(value, details);

      if (! amount_t::keep_price ||
	  ! amount_t::keep_date ||
	  ! amount_t::keep_tag) {
	switch (value.type()) {
	case value_t::AMOUNT:
	case value_t::BALANCE:
	case value_t::BALANCE_PAIR:
	  value = value.strip_annotations();
	  break;
	default:
	  break;
	}
      }

      bool highlighted = false;

      switch (value.type()) {
      case value_t::BOOLEAN:
	out << (value.as_boolean() ? "true" : "false");
	break;

      case value_t::INTEGER:
	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (value.as_long() > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (value.as_long() < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	out << value.as_long();
	break;

      case value_t::DATETIME:
	out << value.as_datetime();
	break;

      case value_t::AMOUNT:
	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (value.as_amount().sign() > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (value.as_amount().sign() < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	out << value.as_amount();
	break;

      case value_t::BALANCE:
	bal = &(value.as_balance());
	// fall through...

      case value_t::BALANCE_PAIR:
	if (! bal)
	  bal = &(value.as_balance_pair().quantity());

	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (*bal > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (*bal < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	bal->print(out, elem->min_width,
		   (elem->max_width > 0 ?
		    elem->max_width : elem->min_width));

	ignore_max_width = true;
	break;
      default:
	assert(false);
	break;
      }

      if (highlighted)
	mark_plain(out);
      break;
    }

    case element_t::OPT_AMOUNT:
      if (details.xact) {
	string disp;
	bool use_disp = false;

	if (details.xact->cost && details.xact->amount) {
	  std::ostringstream stream;
	  if (! details.xact->amount_expr.expr_str.empty())
	    stream << details.xact->amount_expr.expr_str;
	  else
	    stream << details.xact->amount.strip_annotations();

	  if (details.xact->cost_expr)
	    stream << details.xact->cost_expr->expr_str;
	  else
	    stream << " @ " << amount_t(*details.xact->cost /
					details.xact->amount).unround();
	  disp = stream.str();
	  use_disp = true;
	}
	else if (details.entry) {
	  unsigned int    xacts_count = 0;
	  xact_t * first = NULL;
	  xact_t * last  = NULL;

	  foreach (const transaction_t * xact, details.entry->xacts) {
	    if (xact_has_xdata(*xact) &&
		xact_xdata_(*xact).dflags & XACT_TO_DISPLAY) {
	      xacts_count++;
	      if (! first)
		first = xact;
	      last = xact;
	    }
	  }

	  use_disp = (xacts_count == 2 && details.xact == last &&
		      first->amount == - last->amount);
	}

	if (! use_disp) {
	  if (! details.xact->amount_expr.expr_str.empty())
	    out << details.xact->amount_expr.expr_str;
	  else
	    out << details.xact->amount.strip_annotations();
	} else {
	  out << disp;
	}
      }
      break;

    case element_t::SOURCE:
      if (details.entry && details.entry->journal) {
	int idx = details.entry->src_idx;
	foreach (const path& path, details.entry->journal->sources)
	  if (! idx--) {
	    out << path;
	    break;
	  }
      }
      break;

    case element_t::ENTRY_BEG_POS:
      if (details.entry)
	out << (unsigned long)details.entry->beg_pos;
      break;

    case element_t::ENTRY_BEG_LINE:
      if (details.entry)
	out << details.entry->beg_line;
      break;

    case element_t::ENTRY_END_POS:
      if (details.entry)
	out << (unsigned long)details.entry->end_pos;
      break;

    case element_t::ENTRY_END_LINE:
      if (details.entry)
	out << details.entry->end_line;
      break;

    case element_t::XACT_BEG_POS:
      if (details.xact)
	out << (unsigned long)details.xact->beg_pos;
      break;

    case element_t::XACT_BEG_LINE:
      if (details.xact)
	out << details.xact->beg_line;
      break;

    case element_t::XACT_END_POS:
      if (details.xact)
	out << (unsigned long)details.xact->end_pos;
      break;

    case element_t::XACT_END_LINE:
      if (details.xact)
	out << details.xact->end_line;
      break;

    case element_t::DATE_STRING:
      out << format_datetime(scope.resolve("date").as_datetime());
      break;

    case element_t::COMPLETE_DATE_STRING: {
      datetime_t actual_date;
      datetime_t effective_date;
      if (details.xact) {
	actual_date    = details.xact->actual_date();
	effective_date = details.xact->effective_date();
      }
      else if (details.entry) {
	actual_date    = details.entry->actual_date();
	effective_date = details.entry->effective_date();
      }

      char abuf[256];
#if 0
      // jww (2008-04-20): This needs to be rewritten
      std::strftime(abuf, 255, elem->chars.c_str(), actual_date.localtime());
#else
      abuf[0] = '\0';
#endif

      if (is_valid(effective_date) && effective_date != actual_date) {
	char buf[512];
	char ebuf[256];
#if 0
      // jww (2008-04-20): This needs to be rewritten
	std::strftime(ebuf, 255, elem->chars.c_str(),
		      effective_date.localtime());
#else
	ebuf[0] = '\0';
#endif

	std::strcpy(buf, abuf);
	std::strcat(buf, "=");
	std::strcat(buf, ebuf);

	out << (elem->max_width == 0 ? buf : truncate(buf, elem->max_width));
      } else {
	out << (elem->max_width == 0 ? abuf : truncate(abuf, elem->max_width));
      }
      break;
    }

    case element_t::CLEARED:
      if (details.xact) {
	switch (details.xact->state) {
	case xact_t::CLEARED:
	  out << "* ";
	  break;
	case xact_t::PENDING:
	  out << "! ";
	  break;
	case xact_t::UNCLEARED:
	  break;
	}
      }
      break;

    case element_t::ENTRY_CLEARED:
      if (details.entry) {
	xact_t::state_t state;
	if (details.entry->get_state(&state))
	  switch (state) {
	  case xact_t::CLEARED:
	    out << "* ";
	    break;
	  case xact_t::PENDING:
	    out << "! ";
	    break;
	  case xact_t::UNCLEARED:
	    break;
	  }
      }
      break;

    case element_t::CODE: {
      string temp;
      if (details.entry && details.entry->code) {
	temp += "(";
	temp += *details.entry->code;
	temp += ") ";
      }
      out << temp;
      break;
    }

    case element_t::PAYEE:
      scope.resolve("payee").dump(out, elem->min_width);
      break;

    case element_t::OPT_NOTE:
      if (details.xact && details.xact->note)
	out << "  ; ";
      // fall through...

    case element_t::NOTE:
      if (details.xact)
	out << (elem->max_width == 0 ?
		details.xact->note : truncate(*details.xact->note,
					      elem->max_width));
      break;

    case element_t::OPT_ACCOUNT:
      if (details.entry && details.xact) {
	xact_t::state_t state;
	if (! details.entry->get_state(&state))
	  switch (details.xact->state) {
	  case xact_t::CLEARED:
	    name = "* ";
	    break;
	  case xact_t::PENDING:
	    name = "! ";
	    break;
	  case xact_t::UNCLEARED:
	    break;
	  }
      }
      // fall through...

    case element_t::ACCOUNT_NAME:
    case element_t::ACCOUNT_FULLNAME:
      if (details.account) {
	name += (elem->type == element_t::ACCOUNT_FULLNAME ?
		 details.account->fullname() :
		 partial_account_name(*details.account));

	if (details.xact && details.xact->has_flags(XACT_VIRTUAL)) {
	  if (elem->max_width > 2)
	    name = truncate(name, elem->max_width - 2, true);

	  if (details.xact->has_flags(XACT_BALANCE))
	    name = string("[") + name + "]";
	  else
	    name = string("(") + name + ")";
	}
	else if (elem->max_width > 0)
	  name = truncate(name, elem->max_width, true);

	out << name;
      } else {
	out << " ";
      }
      break;

    case element_t::SPACER:
      out << " ";
      break;

    case element_t::DEPTH_SPACER:
      for (const account_t * acct = details.account;
	   acct;
	   acct = acct->parent)
	if (account_has_xdata(*acct) &&
	    account_xdata_(*acct).dflags & ACCOUNT_DISPLAYED) {
	  if (elem->min_width > 0 || elem->max_width > 0)
	    out.width(elem->min_width > elem->max_width ?
		      elem->min_width : elem->max_width);
	  out << " ";
	}
      break;
#endif

    default:
      assert(false);
      break;
    }

    string temp = out.str();
    if (! ignore_max_width &&
	elem->max_width > 0 && elem->max_width < temp.length())
      truncate(temp, elem->max_width);
    out_str << temp;
  }
}

string format_t::truncate(const string& str, unsigned int width,
			  const bool is_account)
{
  const unsigned int len = str.length();
  if (len <= width)
    return str;

  assert(width < 4095);

  char buf[4096];

  switch (elision_style) {
  case TRUNCATE_LEADING:
    // This method truncates at the beginning.
    std::strncpy(buf, str.c_str() + (len - width), width);
    buf[0] = '.';
    buf[1] = '.';
    break;

  case TRUNCATE_MIDDLE:
    // This method truncates in the middle.
    std::strncpy(buf, str.c_str(), width / 2);
    std::strncpy(buf + width / 2,
		 str.c_str() + (len - (width / 2 + width % 2)),
		 width / 2 + width % 2);
    buf[width / 2 - 1] = '.';
    buf[width / 2] = '.';
    break;

  case ABBREVIATE:
    if (is_account) {
      std::list<string> parts;
      string::size_type beg = 0;
      for (string::size_type pos = str.find(':');
	   pos != string::npos;
	   beg = pos + 1, pos = str.find(':', beg))
	parts.push_back(string(str, beg, pos - beg));
      parts.push_back(string(str, beg));

      string result;
      unsigned int newlen = len;
      for (std::list<string>::iterator i = parts.begin();
	   i != parts.end();
	   i++) {
	// Don't contract the last element
	std::list<string>::iterator x = i;
	if (++x == parts.end()) {
	  result += *i;
	  break;
	}

	if (newlen > width) {
	  result += string(*i, 0, abbrev_length);
	  result += ":";
	  newlen -= (*i).length() - abbrev_length;
	} else {
	  result += *i;
	  result += ":";
	}
      }

      if (newlen > width) {
	// Even abbreviated its too big to show the last account, so
	// abbreviate all but the last and truncate at the beginning.
	std::strncpy(buf, result.c_str() + (result.length() - width), width);
	buf[0] = '.';
	buf[1] = '.';
      } else {
	std::strcpy(buf, result.c_str());
      }
      break;
    }
    // fall through...

  case TRUNCATE_TRAILING:
    // This method truncates at the end (the default).
    std::strncpy(buf, str.c_str(), width - 2);
    buf[width - 2] = '.';
    buf[width - 1] = '.';
    break;
  }
  buf[width] = '\0';

  return buf;
}

} // namespace ledger
