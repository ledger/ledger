#include "ledger.h"
#include "datetime.h"
#include "error.h"
#include "util.h"

#include <cstring>

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

unsigned int parse_qif_file(std::istream& in, journal_t * journal,
			    account_t * master, commodity_t * def_commodity)
{
  std::auto_ptr<entry_t>  entry;
  std::auto_ptr<amount_t> amount;
  transaction_t *	  xact;
  account_t *             misc = journal->find_account("Miscellaneous");
  unsigned int            count;

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
      entry.reset(new entry_t);
      xact = new transaction_t(master);
      entry->add_transaction(xact);

      in >> line;
      if (! parse_date(line, &entry->date))
	throw parse_error(path, linenum, "Failed to parse date");
      break;

    case 'T':
    case '$':
      in >> line;
      xact->amount.parse(line);
      if (def_commodity)
	xact->amount.commodity = def_commodity;
      if (c == '$')
	xact->amount.negate();
      xact->cost = xact->amount;
      break;

    case 'C':
      if (in.peek() == '*') {
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
      char b = c;
      int len;
      c = in.peek();
      if (! std::isspace(c) && c != '\n') {
	get_line(in);

	switch (b) {
	case 'P':
	  entry->payee = line;
	  break;

	case 'S':
	  xact = new transaction_t(NULL);
	  entry->add_transaction(xact);
	  // fall through...
	case 'L':
	  len = std::strlen(line);
	  if (line[len - 1] == ']')
	    line[len - 1] = '\0';
	  xact->account = journal->find_account(line[0] == '[' ?
						line + 1 : line);
	  break;

	case 'M':
	case 'E':
	  xact->note = line;
	  break;
	}
      }
      break;
    }

    case 'A':
      // jww (2004-08-19): these are ignored right now
      get_line(in);
      break;

    case '^':
      if (xact->account == master) {
	transaction_t * nxact = new transaction_t(misc);
	entry->add_transaction(nxact);
	nxact->amount = nxact->cost = - xact->amount;
      }

      if (journal->add_entry(entry.release()))
	count++;
      break;
    }
  }

  return count;
}

} // namespace ledger
