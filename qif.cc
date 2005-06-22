#include "journal.h"
#include "qif.h"
#include "datetime.h"
#include "error.h"
#include "util.h"

#include <cstring>
#include <memory>

namespace ledger {

#define MAX_LINE 1024

static char         line[MAX_LINE + 1];
static std::string  path;
static unsigned int linenum;

static inline char * get_line(std::istream& in) {
  in.getline(line, MAX_LINE);
  int len = std::strlen(line);
  if (line[len - 1] == '\r')
    line[len - 1] = '\0';
  linenum++;
  return line;
}

bool qif_parser_t::test(std::istream& in) const
{
  char magic[sizeof(unsigned int) + 1];
  in.read(magic, sizeof(unsigned int));
  magic[sizeof(unsigned int)] = '\0';
  in.seekg(0, std::ios::beg);

  return (std::strcmp(magic, "!Typ") == 0 ||
	  std::strcmp(magic, "\n!Ty") == 0 ||
	  std::strcmp(magic, "\r\n!T") == 0);
}

unsigned int qif_parser_t::parse(std::istream&	     in,
				 journal_t *	     journal,
				 account_t *	     master,
				 const std::string * original_file)
{
  std::auto_ptr<entry_t>  entry;
  std::auto_ptr<amount_t> amount;
  transaction_t *	  xact;
  unsigned int            count = 0;
  account_t *             misc = NULL;
  commodity_t *           def_commodity = NULL;

  entry.reset(new entry_t);
  xact = new transaction_t(master);
  entry->add_transaction(xact);

  path	  = journal->sources.back();
  linenum = 1;

  while (! in.eof()) {
    char c;
    in.get(c);
    switch (c) {
    case ' ':
    case '\t':
      if (peek_next_nonws(in) != '\n') {
	get_line(in);
	throw parse_error(path, linenum, "Line begins with whitespace");
      }
      // fall through...

    case '\n':
      linenum++;
    case '\r':                  // skip blank lines
      break;

    case '!':
      in >> line;

      // jww (2004-08-19): these types are not supported yet
      assert(std::strcmp(line, "Type:Invst") != 0 &&
	     std::strcmp(line, "Account") != 0 &&
	     std::strcmp(line, "Type:Cat") != 0 &&
	     std::strcmp(line, "Type:Class") != 0 &&
	     std::strcmp(line, "Type:Memorized") != 0);

      get_line(in);
      break;

    case 'D':
      in >> line;
      if (! parse_date(line, &entry->date))
	throw parse_error(path, linenum, "Failed to parse date");
      break;

    case 'T':
    case '$': {
      in >> line;
      xact->amount.parse(line);

      unsigned long  flags = xact->amount.commodity().flags;
      unsigned short prec  = xact->amount.commodity().precision;

      if (! def_commodity)
	def_commodity = commodity_t::find_commodity("$", true);
      xact->amount.set_commodity(*def_commodity);

      def_commodity->flags |= flags;
      if (prec > def_commodity->precision)
	def_commodity->precision = prec;

      if (c == '$')
	xact->amount.negate();
      break;
    }

    case 'C':
      c = in.peek();
      if (c == '*' || c == 'X') {
	in.get(c);
	entry->state = entry_t::CLEARED;
      }
      break;

    case 'N':
      if (std::isdigit(in.peek())) {
	in >> line;
	entry->code = line;
      }
      break;

    case 'P':
    case 'M':
    case 'L':
    case 'S':
    case 'E': {
      get_line(in);

      switch (c) {
      case 'P':
	entry->payee = line;
	break;

      case 'S':
	xact = new transaction_t(NULL);
	entry->add_transaction(xact);
	// fall through...
      case 'L': {
	int len = std::strlen(line);
	if (line[len - 1] == ']')
	  line[len - 1] = '\0';
	xact->account = journal->find_account(line[0] == '[' ?
					      line + 1 : line);
	break;
      }

      case 'M':
      case 'E':
	xact->note = line;
	break;
      }
      break;
    }

    case 'A':
      // jww (2004-08-19): these are ignored right now
      get_line(in);
      break;

    case '^': {
      account_t * other;
      if (xact->account == master) {
	if (! misc)
	  misc = journal->find_account("Miscellaneous");
	other = misc;
      } else {
	other = master;
      }

      transaction_t * nxact = new transaction_t(other);
      // The amount doesn't need to be set because the code below will
      // balance this transaction against the other.
      entry->add_transaction(nxact);

      if (journal->add_entry(entry.get())) {
	entry.release();
	count++;
      }

      entry.reset(new entry_t);
      xact = new transaction_t(master);
      entry->add_transaction(xact);
      break;
    }
    }
  }

  return count;
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(qif_parse_overloads,
				       qif_parser_t::parse, 2, 4)

void export_qif() {
  class_< qif_parser_t, bases<parser_t> > ("QifParser")
    .def("test", &qif_parser_t::test)
    .def("parse", &qif_parser_t::parse, qif_parse_overloads())
    ;
}

#endif // USE_BOOST_PYTHON
