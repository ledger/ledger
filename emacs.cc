#include "emacs.h"

namespace ledger {

void format_emacs_transactions::write_entry(entry_t& entry)
{
  int idx = entry.src_idx;
  for (strings_list::iterator i = entry.journal->sources.begin();
       i != entry.journal->sources.end();
       i++)
    if (! idx--) {
      out << "\"" << *i << "\" ";
      break;
    }

  out << ((unsigned long)entry.beg_line) << " ";

  std::time_t date = entry.date().when;
  out << "(" << (date / 65536) << " " << (date % 65536) << " 0) ";

  if (entry.code.empty())
    out << "nil ";
  else
    out << "\"" << entry.code << "\" ";

  if (entry.payee.empty())
    out << "nil";
  else
    out << "\"" << entry.payee << "\"";

  out << "\n";
}

void format_emacs_transactions::operator()(transaction_t& xact)
{
  if (! transaction_has_xdata(xact) ||
      ! (transaction_xdata_(xact).dflags & TRANSACTION_DISPLAYED)) {
    if (! last_entry) {
      out << "((";
      write_entry(*xact.entry);
    }
    else if (xact.entry != last_entry) {
      out << ")\n (";
      write_entry(*xact.entry);
    }
    else {
      out << "\n";
    }

    out << "  (" << ((unsigned long)xact.beg_line) << " ";
    out << "\"" << xact_account(xact)->fullname() << "\" \""
	<< xact.amount << "\"";

    switch (xact.state) {
    case transaction_t::CLEARED:
      out << " t";
      break;
    case transaction_t::PENDING:
      out << " pending";
      break;
    default:
      out << " nil";
      break;
    }

    if (xact.cost)
      out << " \"" << *xact.cost << "\"";
    else if (! xact.note.empty())
      out << " nil";
    if (! xact.note.empty())
      out << " \"" << xact.note << "\"";
    out << ")";

    last_entry = xact.entry;

    transaction_xdata(xact).dflags |= TRANSACTION_DISPLAYED;
  }
}

} // namespace ledger
