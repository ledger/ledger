#include "gnucash.h"
#include "journal.h"
#include "format.h"
#include "error.h"
#include "acconf.h"

#include <iostream>
#include <sstream>
#include <cstring>

extern "C" {
#if defined(HAVE_EXPAT)
#include <expat.h>           // expat XML parser
#elif defined(HAVE_XMLPARSE)
#include <xmlparse.h>        // expat XML parser
#else
#error "No XML parser library defined."
#endif
}

namespace ledger {

typedef std::map<const std::string, account_t *>  accounts_map;
typedef std::pair<const std::string, account_t *> accounts_pair;

typedef std::map<account_t *, commodity_t *>  account_comm_map;
typedef std::pair<account_t *, commodity_t *> account_comm_pair;

static journal_t *	curr_journal;
static account_t *	master_account;
static account_t *	curr_account;
static std::string	curr_account_id;
static entry_t *	curr_entry;
static commodity_t *	entry_comm;
static commodity_t *	curr_comm;
static amount_t		curr_value;
static amount_t		curr_quant;
static XML_Parser	current_parser;
static accounts_map	accounts_by_id;
static account_comm_map	account_comms;
static unsigned int	count;
static std::string	have_error;

static std::istream *   instreamp;
static unsigned int     offset;
static XML_Parser       parser;
static std::string      path;
static unsigned int     src_idx;
static istream_pos_type beg_pos;
static unsigned long    beg_line;

static transaction_t::state_t curr_state;

static enum action_t {
  NO_ACTION,
  ACCOUNT_NAME,
  ACCOUNT_ID,
  ACCOUNT_PARENT,
  COMM_SYM,
  COMM_NAME,
  COMM_PREC,
  ENTRY_NUM,
  ALMOST_ENTRY_DATE,
  ENTRY_DATE,
  ENTRY_DESC,
  XACT_STATE,
  XACT_AMOUNT,
  XACT_VALUE,
  XACT_QUANTITY,
  XACT_ACCOUNT,
  XACT_NOTE
} action;

static void startElement(void *userData, const char *name, const char **atts)
{
  if (std::strcmp(name, "gnc:account") == 0) {
    curr_account = new account_t(master_account);
  }
  else if (std::strcmp(name, "act:name") == 0)
    action = ACCOUNT_NAME;
  else if (std::strcmp(name, "act:id") == 0)
    action = ACCOUNT_ID;
  else if (std::strcmp(name, "act:parent") == 0)
    action = ACCOUNT_PARENT;
  else if (std::strcmp(name, "gnc:commodity") == 0)
    curr_comm = NULL;
  else if (std::strcmp(name, "cmdty:id") == 0)
    action = COMM_SYM;
  else if (std::strcmp(name, "cmdty:name") == 0)
    action = COMM_NAME;
  else if (std::strcmp(name, "cmdty:fraction") == 0)
    action = COMM_PREC;
  else if (std::strcmp(name, "gnc:transaction") == 0) {
    assert(! curr_entry);
    curr_entry = new entry_t;
  }
  else if (std::strcmp(name, "trn:num") == 0)
    action = ENTRY_NUM;
  else if (std::strcmp(name, "trn:date-posted") == 0)
    action = ALMOST_ENTRY_DATE;
  else if (action == ALMOST_ENTRY_DATE && std::strcmp(name, "ts:date") == 0)
    action = ENTRY_DATE;
  else if (std::strcmp(name, "trn:description") == 0)
    action = ENTRY_DESC;
  else if (std::strcmp(name, "trn:split") == 0) {
    assert(curr_entry);
    curr_entry->add_transaction(new transaction_t(curr_account));
  }
  else if (std::strcmp(name, "split:reconciled-state") == 0)
    action = XACT_STATE;
  else if (std::strcmp(name, "split:amount") == 0)
    action = XACT_AMOUNT;
  else if (std::strcmp(name, "split:value") == 0)
    action = XACT_VALUE;
  else if (std::strcmp(name, "split:quantity") == 0)
    action = XACT_QUANTITY;
  else if (std::strcmp(name, "split:account") == 0)
    action = XACT_ACCOUNT;
  else if (std::strcmp(name, "split:memo") == 0)
    action = XACT_NOTE;
}

static void endElement(void *userData, const char *name)
{
  if (std::strcmp(name, "gnc:account") == 0) {
    assert(curr_account);
    if (curr_account->parent == master_account)
      curr_journal->add_account(curr_account);
    accounts_by_id.insert(accounts_pair(curr_account_id, curr_account));
    curr_account = NULL;
  }
  else if (std::strcmp(name, "gnc:commodity") == 0) {
    curr_comm = NULL;
  }
  else if (std::strcmp(name, "gnc:transaction") == 0) {
    assert(curr_entry);

    // Add the new entry (what gnucash calls a 'transaction') to the
    // journal
    if (! curr_journal->add_entry(curr_entry)) {
      print_entry(std::cerr, *curr_entry);
      have_error = "The above entry does not balance";
      delete curr_entry;
    } else {
      curr_entry->src_idx  = src_idx;
      curr_entry->beg_pos  = beg_pos;
      curr_entry->beg_line = beg_line;
      curr_entry->end_pos  = instreamp->tellg();
      curr_entry->end_line = XML_GetCurrentLineNumber(parser) - offset;
      count++;
    }

    // Clear the relevant variables for the next run
    curr_entry = NULL;
    entry_comm = NULL;
  }
  else if (std::strcmp(name, "trn:split") == 0) {
    transaction_t * xact = curr_entry->transactions.back();

    // Identify the commodity to use for the value of this
    // transaction.  The quantity indicates how many times that value
    // the transaction is worth.
    amount_t value;
    commodity_t * default_commodity = NULL;
    account_comm_map::iterator ac = account_comms.find(xact->account);
    if (ac != account_comms.end())
      default_commodity = (*ac).second;

    if (default_commodity) {
      curr_quant.set_commodity(*default_commodity);
      value = curr_quant.round();

      if (curr_value.commodity() == *default_commodity)
	curr_value = value;
    } else {
      value = curr_quant;
    }

    xact->state  = curr_state;
    xact->amount = value;
    if (value != curr_value)
      xact->cost = new amount_t(curr_value);

    xact->beg_pos  = beg_pos;
    xact->beg_line = beg_line;
    xact->end_pos  = instreamp->tellg();
    xact->end_line = XML_GetCurrentLineNumber(parser) - offset;

    // Clear the relevant variables for the next run
    curr_state = transaction_t::UNCLEARED;
    curr_value = amount_t();
    curr_quant = amount_t();
  }

  action = NO_ACTION;
}


static amount_t convert_number(const std::string& number,
			       int * precision = NULL)
{
  const char * num = number.c_str();

  if (char * p = std::strchr(num, '/')) {
    std::string numer_str(num, p - num);
    std::string denom_str(p + 1);

    amount_t amt(numer_str);
    amount_t den(denom_str);

    if (precision)
      *precision = denom_str.length() - 1;

    if (! den) {
      have_error = "Denominator in entry is zero!";
      return amt;
    } else {
      return amt / den;
    }
  } else {
    return amount_t(number);
  }
}

static void dataHandler(void *userData, const char *s, int len)
{
  switch (action) {
  case ACCOUNT_NAME:
    curr_account->name = std::string(s, len);
    break;

  case ACCOUNT_ID:
    curr_account_id = std::string(s, len);
    break;

  case ACCOUNT_PARENT: {
    accounts_map::iterator i = accounts_by_id.find(std::string(s, len));
    assert(i != accounts_by_id.end());
    curr_account->parent = (*i).second;
    curr_account->depth  = curr_account->parent->depth + 1;
    (*i).second->add_account(curr_account);
    break;
  }

  case COMM_SYM: {
      std::string symbol(s, len);
    if (symbol == "USD") symbol = "$";

    curr_comm = commodity_t::find_or_create(symbol);
    assert(curr_comm);

    if (symbol != "$")
      curr_comm->add_flags(COMMODITY_STYLE_SEPARATED);

    if (curr_account)
      account_comms.insert(account_comm_pair(curr_account, curr_comm));
    else if (curr_entry)
      entry_comm = curr_comm;
    break;
  }

  case COMM_NAME:
    curr_comm->set_name(std::string(s, len));
    break;

  case COMM_PREC:
    curr_comm->set_precision(len - 1);
    break;

  case ENTRY_NUM:
    curr_entry->code = std::string(s, len);
    break;

  case ENTRY_DATE:
    curr_entry->_date = std::string(s, len);
    break;

  case ENTRY_DESC:
    curr_entry->payee = std::string(s, len);
    break;

  case XACT_STATE:
    if (*s == 'y')
      curr_state = transaction_t::CLEARED;
    else if (*s == 'n')
      curr_state = transaction_t::UNCLEARED;
    else
      curr_state = transaction_t::PENDING;
    break;

  case XACT_VALUE: {
    int precision;
    assert(entry_comm);
    curr_value = convert_number(std::string(s, len), &precision);
    curr_value.set_commodity(*entry_comm);

    if (precision > entry_comm->precision())
      entry_comm->set_precision(precision);
    break;
  }

  case XACT_QUANTITY:
    curr_quant = convert_number(std::string(s, len));
    break;

  case XACT_ACCOUNT: {
    transaction_t * xact = curr_entry->transactions.back();

    accounts_map::iterator i = accounts_by_id.find(std::string(s, len));
    if (i != accounts_by_id.end()) {
      xact->account = (*i).second;
    } else {
      xact->account = curr_journal->find_account("<Unknown>");

      have_error = (std::string("Could not find account ") +
		    std::string(s, len));
    }
    break;
  }

  case XACT_NOTE:
    curr_entry->transactions.back()->note = std::string(s, len);
    break;

  case NO_ACTION:
  case ALMOST_ENTRY_DATE:
  case XACT_AMOUNT:
    break;

  default:
    assert(0);
    break;
  }
}

bool gnucash_parser_t::test(std::istream& in) const
{
  char buf[5];
  in.read(buf, 5);
  in.clear();
  in.seekg(0, std::ios::beg);

  return std::strncmp(buf, "<?xml", 5) == 0;
}

unsigned int gnucash_parser_t::parse(std::istream&	 in,
				     config_t&           config,
				     journal_t *	 journal,
				     account_t *	 master,
				     const std::string * original_file)
{
  char buf[BUFSIZ];

  // This is the date format used by Gnucash, so override whatever the
  // user specified.
  date_t::input_format = "%Y-%m-%d %H:%M:%S %z";

  count		 = 0;
  action	 = NO_ACTION;
  curr_journal	 = journal;
  master_account = master ? master : journal->master;
  curr_account	 = NULL;
  curr_entry	 = NULL;
  curr_comm	 = NULL;
  entry_comm	 = NULL;
  curr_state	 = transaction_t::UNCLEARED;

  instreamp = &in;
  path	    = original_file ? *original_file : "<gnucash>";
  src_idx   = journal->sources.size() - 1;

  // GnuCash uses the USD commodity without defining it, which really
  // means $.
  commodity_t * usd = commodity_t::find_or_create("$");
  usd->set_precision(2);
  usd->add_flags(COMMODITY_STYLE_THOUSANDS);

  offset = 2;
  parser = current_parser = XML_ParserCreate(NULL);

  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  while (in.good() && ! in.eof()) {
    beg_pos  = in.tellg();
    beg_line = (XML_GetCurrentLineNumber(parser) - offset) + 1;

    in.getline(buf, BUFSIZ - 1);
    std::strcat(buf, "\n");
    if (! XML_Parse(parser, buf, std::strlen(buf), in.eof())) {
      unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      const char *  msg  = XML_ErrorString(XML_GetErrorCode(parser));
      XML_ParserFree(parser);
      throw new parse_error(msg);
    }

    if (! have_error.empty()) {
      unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      parse_error err(have_error);
      std::cerr << "Error: " << err.what() << std::endl;
      have_error = "";
    }
  }

  XML_ParserFree(parser);

  accounts_by_id.clear();
  curr_account_id.clear();

  return count;
}

} // namespace ledger
