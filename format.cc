#include "format.h"
#include "error.h"
#include "util.h"

#include <cstdlib>

namespace ledger {

format_t::elision_style_t format_t::elision_style = ABBREVIATE;
int format_t::abbrev_length = 2;

bool format_t::ansi_codes  = false;
bool format_t::ansi_invert = false;

std::string format_t::truncate(const std::string& str, unsigned int width,
			       const bool is_account)
{
  const int len = str.length();
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
      std::list<std::string> parts;
      std::string::size_type beg = 0;
      for (std::string::size_type pos = str.find(':');
	   pos != std::string::npos;
	   beg = pos + 1, pos = str.find(':', beg))
	parts.push_back(std::string(str, beg, pos - beg));
      parts.push_back(std::string(str, beg));

      std::string result;
      int newlen = len;
      for (std::list<std::string>::iterator i = parts.begin();
	   i != parts.end();
	   i++) {
	// Don't contract the last element
	std::list<std::string>::iterator x = i;
	if (++x == parts.end()) {
	  result += *i;
	  break;
	}

	if (newlen > width) {
	  result += std::string(*i, 0, abbrev_length);
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

std::string partial_account_name(const account_t& account)
{
  std::string name;

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

element_t * format_t::parse_elements(const std::string& fmt)
{
  std::auto_ptr<element_t> result;

  element_t * current = NULL;

  char   buf[1024];
  char * q = buf;

  for (const char * p = fmt.c_str(); *p; p++) {
    if (*p != '%' && *p != '\\') {
      *q++ = *p;
      continue;
    }

    if (! result.get()) {
      result.reset(new element_t);
      current = result.get();
    } else {
      current->next = new element_t;
      current = current->next;
    }

    if (q != buf) {
      current->type  = element_t::STRING;
      current->chars = std::string(buf, q);
      q = buf;

      current->next  = new element_t;
      current = current->next;
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

    case '(': {
      ++p;
      const char * b = p;
      int depth = 1;
      while (*p) {
	if (*p == ')' && --depth == 0)
	  break;
	else if (*p == '(')
	  ++depth;
	p++;
      }
      if (*p != ')')
	throw new format_error("Missing ')'");

      current->type = element_t::VALUE_EXPR;

      assert(! current->val_expr);
      current->val_expr = std::string(b, p);
      break;
    }

    case '[': {
      ++p;
      const char * b = p;
      int depth = 1;
      while (*p) {
	if (*p == ']' && --depth == 0)
	  break;
	else if (*p == '[')
	  ++depth;
	p++;
      }
      if (*p != ']')
	throw new format_error("Missing ']'");

      current->type  = element_t::DATE_STRING;
      current->chars = std::string(b, p);
      break;
    }

    case 'x':
      switch (*++p) {
      case 'B': current->type = element_t::XACT_BEG_POS; break;
      case 'b': current->type = element_t::XACT_BEG_LINE; break;
      case 'E': current->type = element_t::XACT_END_POS; break;
      case 'e': current->type = element_t::XACT_END_LINE; break;
      case '\0':
	goto END;
      }
      break;

    case 'd':
      current->type  = element_t::COMPLETE_DATE_STRING;
      current->chars = datetime_t::output_format;
      break;
    case 'D':
      current->type  = element_t::DATE_STRING;
      current->chars = datetime_t::output_format;
      break;

    case 'S': current->type = element_t::SOURCE; break;
    case 'B': current->type = element_t::ENTRY_BEG_POS; break;
    case 'b': current->type = element_t::ENTRY_BEG_LINE; break;
    case 'E': current->type = element_t::ENTRY_END_POS; break;
    case 'e': current->type = element_t::ENTRY_END_LINE; break;
    case 'X': current->type = element_t::CLEARED; break;
    case 'Y': current->type = element_t::ENTRY_CLEARED; break;
    case 'C': current->type = element_t::CODE; break;
    case 'P': current->type = element_t::PAYEE; break;
    case 'W': current->type = element_t::OPT_ACCOUNT; break;
    case 'a': current->type = element_t::ACCOUNT_NAME; break;
    case 'A': current->type = element_t::ACCOUNT_FULLNAME; break;
    case 't': current->type = element_t::AMOUNT; break;
    case 'o': current->type = element_t::OPT_AMOUNT; break;
    case 'T': current->type = element_t::TOTAL; break;
    case 'N': current->type = element_t::NOTE; break;
    case 'n': current->type = element_t::OPT_NOTE; break;
    case '|': current->type = element_t::SPACER; break;
    case '_': current->type = element_t::DEPTH_SPACER; break;
    }
  }

 END:
  if (q != buf) {
    if (! result.get()) {
      result.reset(new element_t);
      current = result.get();
    } else {
      current->next = new element_t;
      current = current->next;
    }
    current->type  = element_t::STRING;
    current->chars = std::string(buf, q);
  }

  return result.release();
}

namespace {
  inline void mark_red(std::ostream& out, const element_t * elem) {
    out.setf(std::ios::left);
    out.width(0);
    out << "\e[31m";

    if (elem->flags & ELEMENT_ALIGN_LEFT)
      out << std::left;
    else
      out << std::right;

    if (elem->min_width > 0)
      out.width(elem->min_width);
  }

  inline void mark_plain(std::ostream& out) {
    out << "\e[0m";
  }
}

void format_t::format(std::ostream& out_str, const details_t& details) const
{
  for (const element_t * elem = elements; elem; elem = elem->next) {
    std::ostringstream out;
    std::string name;
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

    case element_t::AMOUNT:
    case element_t::TOTAL:
    case element_t::VALUE_EXPR: {
      value_expr calc;
      switch (elem->type) {
      case element_t::AMOUNT:     calc = amount_expr; break;
      case element_t::TOTAL:      calc = total_expr; break;
      case element_t::VALUE_EXPR: calc = elem->val_expr; break;
      default:
	assert(0);
	break;
      }
      if (! calc)
	break;

      value_t     value;
      balance_t * bal = NULL;

      calc->compute(value, details);

      if (! amount_t::keep_price ||
	  ! amount_t::keep_date ||
	  ! amount_t::keep_tag) {
	switch (value.type) {
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

      switch (value.type) {
      case value_t::BOOLEAN:
	out << (*((bool *) value.data) ? "true" : "false");
	break;

      case value_t::INTEGER:
	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (*((long *) value.data) > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (*((long *) value.data) < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	out << *((long *) value.data);
	break;

      case value_t::DATETIME:
	out << *((datetime_t *) value.data);
	break;

      case value_t::AMOUNT:
	if (ansi_codes && elem->flags & ELEMENT_HIGHLIGHT) {
	  if (ansi_invert) {
	    if (*((amount_t *) value.data) > 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  } else {
	    if (*((amount_t *) value.data) < 0) {
	      mark_red(out, elem);
	      highlighted = true;
	    }
	  }
	}
	out << *((amount_t *) value.data);
	break;

      case value_t::BALANCE:
	bal = (balance_t *) value.data;
	// fall through...

      case value_t::BALANCE_PAIR:
	if (! bal)
	  bal = &((balance_pair_t *) value.data)->quantity;

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
	bal->write(out, elem->min_width,
		   (elem->max_width > 0 ?
		    elem->max_width : elem->min_width));

	ignore_max_width = true;
	break;
      default:
	assert(0);
	break;
      }

      if (highlighted)
	mark_plain(out);
      break;
    }

    case element_t::OPT_AMOUNT:
      if (details.xact) {
	std::string disp;
	bool use_disp = false;

	if (details.xact->cost && details.xact->amount) {
	  std::ostringstream stream;
	  if (! details.xact->amount_expr.expr.empty())
	    stream << details.xact->amount_expr.expr;
	  else
	    stream << details.xact->amount.strip_annotations();

	  if (! details.xact->cost_expr.empty())
	    stream << details.xact->cost_expr;
	  else
	    stream << " @ " << amount_t(*details.xact->cost /
					details.xact->amount).unround();
	  disp = stream.str();
	  use_disp = true;
	}
	else if (details.entry) {
	  unsigned int    xacts_count = 0;
	  transaction_t * first = NULL;
	  transaction_t * last  = NULL;

	  for (transactions_list::const_iterator i
		 = details.entry->transactions.begin();
	       i != details.entry->transactions.end();
	       i++)
	    if (transaction_has_xdata(**i) &&
		transaction_xdata_(**i).dflags & TRANSACTION_TO_DISPLAY) {
	      xacts_count++;
	      if (! first)
		first = *i;
	      last = *i;
	    }

	  use_disp = (xacts_count == 2 && details.xact == last &&
		      first->amount == - last->amount);
	}

	if (! use_disp) {
	  if (! details.xact->amount_expr.expr.empty())
	    out << details.xact->amount_expr.expr;
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
	for (strings_list::iterator i = details.entry->journal->sources.begin();
	     i != details.entry->journal->sources.end();
	     i++)
	  if (! idx--) {
	    out << *i;
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

    case element_t::DATE_STRING: {
      datetime_t date;
      if (details.xact)
	date = details.xact->date();
      else if (details.entry)
	date = details.entry->date();

      char buf[256];
      std::strftime(buf, 255, elem->chars.c_str(), date.localtime());
      out << (elem->max_width == 0 ? buf : truncate(buf, elem->max_width));
      break;
    }

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
      std::strftime(abuf, 255, elem->chars.c_str(), actual_date.localtime());

      if (effective_date && effective_date != actual_date) {
	char buf[512];
	char ebuf[256];
	std::strftime(ebuf, 255, elem->chars.c_str(),
		      effective_date.localtime());

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
	case transaction_t::CLEARED:
	  out << "* ";
	  break;
	case transaction_t::PENDING:
	  out << "! ";
	  break;
	default:
	  out << "";
	  break;
	}
      }
      break;

    case element_t::ENTRY_CLEARED:
      if (details.entry) {
	transaction_t::state_t state;
	if (details.entry->get_state(&state))
	  switch (state) {
	  case transaction_t::CLEARED:
	    out << "* ";
	    break;
	  case transaction_t::PENDING:
	    out << "! ";
	    break;
	  default:
	    out << "";
	    break;
	  }
      }
      break;

    case element_t::CODE: {
      std::string temp;
      if (details.entry && ! details.entry->code.empty()) {
	temp += "(";
	temp += details.entry->code;
	temp += ") ";
      }
      out << temp;
      break;
    }

    case element_t::PAYEE:
      if (details.entry)
	out << (elem->max_width == 0 ?
		details.entry->payee : truncate(details.entry->payee,
						elem->max_width));
      break;

    case element_t::OPT_NOTE:
      if (details.xact && ! details.xact->note.empty())
	out << "  ; ";
      // fall through...

    case element_t::NOTE:
      if (details.xact)
	out << (elem->max_width == 0 ?
		details.xact->note : truncate(details.xact->note,
					      elem->max_width));
      break;

    case element_t::OPT_ACCOUNT:
      if (details.entry && details.xact) {
	transaction_t::state_t state;
	if (! details.entry->get_state(&state))
	  switch (details.xact->state) {
	  case transaction_t::CLEARED:
	    name = "* ";
	    break;
	  case transaction_t::PENDING:
	    name = "! ";
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

	if (details.xact && details.xact->flags & TRANSACTION_VIRTUAL) {
	  if (elem->max_width > 2)
	    name = truncate(name, elem->max_width - 2, true);

	  if (details.xact->flags & TRANSACTION_BALANCE)
	    name = "[" + name + "]";
	  else
	    name = "(" + name + ")";
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

    default:
      assert(0);
      break;
    }

    std::string temp = out.str();
    if (! ignore_max_width &&
	elem->max_width > 0 && elem->max_width < temp.length())
      temp.erase(elem->max_width);
    out_str << temp;
  }
}

format_transactions::format_transactions(std::ostream& _output_stream,
					 const std::string& format)
  : output_stream(_output_stream), last_entry(NULL), last_xact(NULL)
{
  const char * f = format.c_str();
  if (const char * p = std::strstr(f, "%/")) {
    first_line_format.reset(std::string(f, 0, p - f));
    next_lines_format.reset(std::string(p + 2));
  } else {
    first_line_format.reset(format);
    next_lines_format.reset(format);
  }
}

void format_transactions::operator()(transaction_t& xact)
{
  if (! transaction_has_xdata(xact) ||
      ! (transaction_xdata_(xact).dflags & TRANSACTION_DISPLAYED)) {
    if (last_entry != xact.entry) {
      first_line_format.format(output_stream, details_t(xact));
      last_entry = xact.entry;
    }
    else if (last_xact && last_xact->date() != xact.date()) {
      first_line_format.format(output_stream, details_t(xact));
    }
    else {
      next_lines_format.format(output_stream, details_t(xact));
    }

    transaction_xdata(xact).dflags |= TRANSACTION_DISPLAYED;
    last_xact = &xact;
  }
}

void format_entries::format_last_entry()
{
  bool first = true;
  for (transactions_list::const_iterator i = last_entry->transactions.begin();
       i != last_entry->transactions.end();
       i++) {
    if (transaction_has_xdata(**i) &&
	transaction_xdata_(**i).dflags & TRANSACTION_TO_DISPLAY) {
      if (first) {
	first_line_format.format(output_stream, details_t(**i));
	first = false;
      } else {
	next_lines_format.format(output_stream, details_t(**i));
      }
      transaction_xdata_(**i).dflags |= TRANSACTION_DISPLAYED;
    }
  }
}

void format_entries::operator()(transaction_t& xact)
{
  transaction_xdata(xact).dflags |= TRANSACTION_TO_DISPLAY;

  if (last_entry && xact.entry != last_entry)
    format_last_entry();

  last_entry = xact.entry;
}

void print_entry(std::ostream& out, const entry_base_t& entry_base,
		 const std::string& prefix)
{
  std::string print_format;

  if (const entry_t * entry = dynamic_cast<const entry_t *>(&entry_base)) {
    print_format = (prefix + "%D %X%C%P\n" +
		    prefix + "    %-34A  %12o\n%/" +
		    prefix + "    %-34A  %12o\n");
  }
  else if (const auto_entry_t * entry =
	   dynamic_cast<const auto_entry_t *>(&entry_base)) {
    out << "= " << entry->predicate_string << '\n';
    print_format = prefix + "    %-34A  %12o\n";
  }
  else if (const period_entry_t * entry =
	   dynamic_cast<const period_entry_t *>(&entry_base)) {
    out << "~ " << entry->period_string << '\n';
    print_format = prefix + "    %-34A  %12o\n";
  }
  else {
    assert(0);
  }

  format_entries formatter(out, print_format);
  walk_transactions(const_cast<transactions_list&>(entry_base.transactions),
		    formatter);
  formatter.flush();

  clear_transaction_xdata cleaner;
  walk_transactions(const_cast<transactions_list&>(entry_base.transactions),
		    cleaner);
}

bool disp_subaccounts_p(const account_t&		 account,
			const item_predicate<account_t>& disp_pred,
			const account_t *&		 to_show)
{
  bool	       display  = false;
  unsigned int counted  = 0;
  bool         matches  = disp_pred(account);
  value_t      acct_total;
  bool         computed = false;
  value_t      result;

  to_show = NULL;

  for (accounts_map::const_iterator i = account.accounts.begin();
       i != account.accounts.end();
       i++) {
    if (! disp_pred(*(*i).second))
      continue;

    compute_total(result, details_t(*(*i).second));
    if (! computed) {
      compute_total(acct_total, details_t(account));
      computed = true;
    }

    if ((result != acct_total) || counted > 0) {
      display = matches;
      break;
    }
    to_show = (*i).second;
    counted++;
  }

  return display;
}

bool display_account(const account_t& account,
		     const item_predicate<account_t>& disp_pred)
{
  // Never display an account that has already been displayed.
  if (account_has_xdata(account) &&
      account_xdata_(account).dflags & ACCOUNT_DISPLAYED)
    return false;

  // At this point, one of two possibilities exists: the account is a
  // leaf which matches the predicate restrictions; or it is a parent
  // and two or more children must be subtotaled; or it is a parent
  // and its child has been hidden by the predicate.  So first,
  // determine if it is a parent that must be displayed regardless of
  // the predicate.

  const account_t * account_to_show = NULL;
  if (disp_subaccounts_p(account, disp_pred, account_to_show))
    return true;

  return ! account_to_show && disp_pred(account);
}

void format_account::operator()(account_t& account)
{
  if (display_account(account, disp_pred)) {
    if (! account.parent) {
      account_xdata(account).dflags |= ACCOUNT_TO_DISPLAY;
    } else {
      format.format(output_stream, details_t(account));
      account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
    }
  }
}

format_equity::format_equity(std::ostream&      _output_stream,
			     const std::string& _format,
			     const std::string& display_predicate)
  : output_stream(_output_stream), disp_pred(display_predicate)
{
  const char * f = _format.c_str();
  if (const char * p = std::strstr(f, "%/")) {
    first_line_format.reset(std::string(f, 0, p - f));
    next_lines_format.reset(std::string(p + 2));
  } else {
    first_line_format.reset(_format);
    next_lines_format.reset(_format);
  }

  entry_t header_entry;
  header_entry.payee = "Opening Balances";
  header_entry._date = datetime_t::now;
  first_line_format.format(output_stream, details_t(header_entry));
}

void format_equity::flush()
{
  account_xdata_t xdata;
  xdata.value = total;
  xdata.value.negate();
  account_t summary(NULL, "Equity:Opening Balances");
  summary.data = &xdata;

  if (total.type >= value_t::BALANCE) {
    balance_t * bal;
    if (total.type == value_t::BALANCE)
      bal = (balance_t *) total.data;
    else if (total.type == value_t::BALANCE_PAIR)
      bal = &((balance_pair_t *) total.data)->quantity;
    else
      assert(0);

    for (amounts_map::const_iterator i = bal->amounts.begin();
	 i != bal->amounts.end();
	 i++) {
      xdata.value = (*i).second;
      xdata.value.negate();
      next_lines_format.format(output_stream, details_t(summary));
    }
  } else {
    next_lines_format.format(output_stream, details_t(summary));
  }
  output_stream.flush();
}

void format_equity::operator()(account_t& account)
{
  if (display_account(account, disp_pred)) {
    if (account_has_xdata(account)) {
      value_t val = account_xdata_(account).value;

      if (val.type >= value_t::BALANCE) {
	balance_t * bal;
	if (val.type == value_t::BALANCE)
	  bal = (balance_t *) val.data;
	else if (val.type == value_t::BALANCE_PAIR)
	  bal = &((balance_pair_t *) val.data)->quantity;
	else
	  assert(0);

	for (amounts_map::const_iterator i = bal->amounts.begin();
	     i != bal->amounts.end();
	     i++) {
	  account_xdata_(account).value = (*i).second;
	  next_lines_format.format(output_stream, details_t(account));
	}
	account_xdata_(account).value = val;
      } else {
	next_lines_format.format(output_stream, details_t(account));
      }
      total += val;
    }
    account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
  }
}

} // namespace ledger
