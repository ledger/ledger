#include <sstream>
#include <cstring>

#include "ledger.h"

extern "C" {
#include <xmlparse.h>           // expat XML parser
}

namespace ledger {

static account *    curr_account;
static std::string  curr_account_id;
static entry *      curr_entry;
static commodity *  entry_comm;
static commodity *  curr_comm;
static amount *     curr_value;
static std::string  curr_quant;
static XML_Parser   current_parser;
static accounts_map accounts_by_id;
static bool	    do_compute;

static enum {
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
    assert(! curr_account);
    curr_account = new account;
  }
  else if (std::strcmp(name, "act:name") == 0)
    action = ACCOUNT_NAME;
  else if (std::strcmp(name, "act:id") == 0)
    action = ACCOUNT_ID;
  else if (std::strcmp(name, "act:parent") == 0)
    action = ACCOUNT_PARENT;
  else if (std::strcmp(name, "gnc:commodity") == 0) {
    assert(! curr_comm);
    curr_comm = new commodity;
  }
  else if (std::strcmp(name, "cmdty:id") == 0)
    action = COMM_SYM;
  else if (std::strcmp(name, "cmdty:name") == 0)
    action = COMM_NAME;
  else if (std::strcmp(name, "cmdty:fraction") == 0)
    action = COMM_PREC;
  else if (std::strcmp(name, "gnc:transaction") == 0) {
    assert(! curr_entry);
    curr_entry = new entry;
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
    curr_entry->xacts.push_back(new transaction());
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
    if (! curr_account->parent)
      main_ledger->accounts.insert(accounts_map_pair(curr_account->name,
						    curr_account));
    accounts_by_id.insert(accounts_map_pair(curr_account_id, curr_account));
    curr_account = NULL;
  }
  else if (std::strcmp(name, "gnc:commodity") == 0) {
    assert(curr_comm);
    main_ledger->commodities.insert(commodities_map_pair(curr_comm->symbol,
							curr_comm));
    curr_comm = NULL;
  }
  else if (std::strcmp(name, "gnc:transaction") == 0) {
    assert(curr_entry);
    assert(curr_entry->validate());
    main_ledger->entries.push_back(curr_entry);
    curr_entry = NULL;
  }
  action = NO_ACTION;
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
    accounts_map_iterator i = accounts_by_id.find(std::string(s, len));
    assert(i != accounts_by_id.end());
    curr_account->parent = (*i).second;
    (*i).second->children.insert(accounts_map_pair(curr_account->name,
						   curr_account));
    break;
  }

  case COMM_SYM:
    if (curr_comm)
      curr_comm->symbol = std::string(s, len);
    else if (curr_account)
      curr_account->comm = main_ledger->commodities[std::string(s, len)];
    else if (curr_entry)
      entry_comm = main_ledger->commodities[std::string(s, len)];
    break;

  case COMM_NAME:
    curr_comm->name = std::string(s, len);
    break;

  case COMM_PREC:
    curr_comm->precision = len - 1;
    break;

  case ENTRY_NUM:
    curr_entry->code = std::string(s, len);
    break;

  case ENTRY_DATE: {
    struct tm when;
    strptime(std::string(s, len).c_str(), "%Y-%m-%d %H:%M:%S %z", &when);
    curr_entry->date = std::mktime(&when);
    break;
  }

  case ENTRY_DESC:
    curr_entry->desc = std::string(s, len);
    break;

  case XACT_STATE:
    curr_entry->cleared = (*s == 'y' || *s == 'c');
    break;

  case XACT_VALUE: {
    assert(entry_comm);
    std::string value = std::string(s, len) + " " + entry_comm->symbol;
    curr_value = create_amount(value.c_str());
    break;
  }

  case XACT_QUANTITY:
    curr_quant = std::string(s, len);
    break;

  case XACT_ACCOUNT: {
    accounts_map_iterator i = accounts_by_id.find(std::string(s, len));
    if (i == accounts_by_id.end()) {
      std::cerr << "Could not find account " << std::string(s, len)
		<< std::endl;
      std::exit(1);
    }

    transaction * xact = curr_entry->xacts.back();
    xact->acct = (*i).second;

    std::string value = curr_quant + " " + xact->acct->comm->symbol;

    if (curr_value->commdty() == xact->acct->comm) {
      // assert: value must be equal to curr_value.
      delete curr_value;
      curr_value = NULL;
    }

    xact->cost = create_amount(value.c_str(), curr_value);

    if (curr_value) {
      delete curr_value;
      curr_value = NULL;
    }

    if (do_compute)
      xact->acct->balance.credit(xact->cost);
    break;
  }

  case XACT_NOTE:
    curr_entry->xacts.back()->note = std::string(s, len);
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

book * parse_gnucash(std::istream& in, bool compute_balances)
{
  char buf[BUFSIZ];

  book * ledger = new book;

  main_ledger  = ledger;
  do_compute   = compute_balances;
  action       = NO_ACTION;
  curr_account = NULL;
  curr_entry   = NULL;
  curr_value   = NULL;
  curr_comm    = NULL;
  entry_comm   = NULL;

  // GnuCash uses the USD commodity without defining it, which really
  // means to use $.
  commodity * usd = new commodity("$", true, false, true, false, 2);
  main_ledger->commodities.insert(commodities_map_pair("USD", usd));

  XML_Parser parser = XML_ParserCreate(NULL);
  current_parser = parser;

  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  while (! in.eof()) {
    in.getline(buf, BUFSIZ - 1);

    if (! XML_Parse(parser, buf, std::strlen(buf), in.eof())) {
      std::cerr << XML_ErrorString(XML_GetErrorCode(parser))
		<< " at line " << XML_GetCurrentLineNumber(parser)
		<< std::endl;
      return NULL;
    }
  }
  XML_ParserFree(parser);

  accounts_by_id.clear();
  curr_account_id.clear();
  curr_quant.clear();

  main_ledger->commodities.erase("USD");

  return ledger;
}

} // namespace ledger
