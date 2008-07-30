#include "emacs.h"

namespace ledger {

void format_emacs_xacts::write_entry(entry_t& entry)
{
  int idx = entry.src_idx;
  for (paths_list::const_iterator i = entry.journal->sources.begin();
       i != entry.journal->sources.end();
       i++)
    if (! idx--) {
      out << "\"" << *i << "\" ";
      break;
    }

  out << (static_cast<unsigned long>(entry.beg_line) + 1) << " ";

  tm when = boost::posix_time::to_tm(entry.date());
  std::time_t date = std::mktime(&when); // jww (2008-04-20): Is this GMT or local?

  out << "(" << (date / 65536) << " " << (date % 65536) << " 0) ";

  if (! entry.code)
    out << "nil ";
  else
    out << "\"" << *entry.code << "\" ";

  if (entry.payee.empty())
    out << "nil";
  else
    out << "\"" << entry.payee << "\"";

  out << "\n";
}

void format_emacs_xacts::operator()(xact_t& xact)
{
  if (! xact_has_xdata(xact) ||
      ! (xact_xdata_(xact).dflags & XACT_DISPLAYED)) {
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

    out << "  (" << (static_cast<unsigned long>(xact.beg_line) + 1) << " ";
    out << "\"" << xact_account(xact)->fullname() << "\" \""
	<< xact.amount << "\"";

    switch (xact.state) {
    case xact_t::CLEARED:
      out << " t";
      break;
    case xact_t::PENDING:
      out << " pending";
      break;
    default:
      out << " nil";
      break;
    }

    if (xact.cost)
      out << " \"" << *xact.cost << "\"";
    if (xact.note)
      out << " \"" << *xact.note << "\"";
    out << ")";

    last_entry = xact.entry;

    xact_xdata(xact).dflags |= XACT_DISPLAYED;
  }
}

} // namespace ledger
