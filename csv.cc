#include "csv.h"

namespace ledger {

namespace {
  inline void write_escaped_string(std::ostream& out, const std::string& xact)
  {
    out << "\"";
    for (std::string::const_iterator i = xact.begin(); i != xact.end(); i++)
      if (*i == '"') {
	out << "\\";
	out << "\"";
      } else {
	out << *i;
      }
    out << "\"";
  }
}

void format_csv_transactions::operator()(transaction_t& xact)
{
  if (! transaction_has_xdata(xact) ||
      ! (transaction_xdata_(xact).dflags & TRANSACTION_DISPLAYED)) {

    {
      format_t fmt("%D");
      std::ostringstream str;
      fmt.format(str, details_t(xact));
      write_escaped_string(out, str.str());
    }
    out << ',';

    {
      format_t fmt("%P");
      std::ostringstream str;
      fmt.format(str, details_t(xact));
      write_escaped_string(out, str.str());
    }
    out << ',';

    {
      format_t fmt("%A");
      std::ostringstream str;
      fmt.format(str, details_t(xact));
      write_escaped_string(out, str.str());
    }
    out << ',';

    {
      format_t fmt("%t");
      std::ostringstream str;
      fmt.format(str, details_t(xact));
      write_escaped_string(out, str.str());
    }
    out << ',';

    {
      format_t fmt("%T");
      std::ostringstream str;
      fmt.format(str, details_t(xact));
      write_escaped_string(out, str.str());
    }
    out << ',';

    switch (xact.state) {
    case transaction_t::CLEARED:
      write_escaped_string(out, "*");
      break;
    case transaction_t::PENDING:
      write_escaped_string(out, "!");
      break;
    default: {
      transaction_t::state_t state;
      if (xact.entry->get_state(&state))
	switch (state) {
	case transaction_t::CLEARED:
	  write_escaped_string(out, "*");
	  break;
	case transaction_t::PENDING:
	  write_escaped_string(out, "!");
	  break;
	default:
	  write_escaped_string(out, "");
	  break;
	}
    }
    }
    out << ',';

    write_escaped_string(out, xact.entry->code);
    out << ',';

    {
      format_t fmt("%N");
      std::ostringstream str;
      fmt.format(str, details_t(xact));
      write_escaped_string(out, str.str());
    }
    out << '\n';

    transaction_xdata(xact).dflags |= TRANSACTION_DISPLAYED;
  }
}

} // namespace ledger
