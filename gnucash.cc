#include <sstream>
#include <vector>
#include <cstring>
#include <cassert>

extern "C" {
#include <xmlparse.h>           // expat XML parser
}

#include "ledger.h"

namespace ledger {

static account *   curr_account;
static std::string curr_account_id;
static entry *     curr_entry;
static commodity * entry_comm;
static commodity * curr_comm;
static amount *    curr_value;
static std::string curr_quant;
static XML_Parser  current_parser;

static std::vector<entry *> * current_ledger;

enum {
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
    curr_account = new account(name);
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
    accounts.insert(accounts_entry(curr_account->name, curr_account));
    accounts.insert(accounts_entry(curr_account_id, curr_account));
    curr_account = NULL;
  }
  else if (std::strcmp(name, "gnc:commodity") == 0) {
    assert(curr_comm);
    commodities.insert(commodities_entry(curr_comm->symbol, curr_comm));
    curr_comm = NULL;
  }
  else if (std::strcmp(name, "gnc:transaction") == 0) {
    assert(curr_entry);
    if (! curr_entry->validate()) {
      std::cerr << "Failed to balance the following transaction, "
		<< "ending on line "
		<< XML_GetCurrentLineNumber(current_parser) << std::endl;
      curr_entry->print(std::cerr);
    } else {
      current_ledger->push_back(curr_entry);
    }
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
    accounts_iterator i = accounts.find(std::string(s, len));
    assert(i != accounts.end());
    curr_account->parent = (*i).second;
    (*i).second->children.insert(account::pair(curr_account->name,
					       curr_account));
    break;
  }

  case COMM_SYM:
    if (curr_comm)
      curr_comm->symbol = std::string(s, len);
    else if (curr_account)
      curr_account->comm = commodities[std::string(s, len)];
    else if (curr_entry)
      entry_comm = commodities[std::string(s, len)];
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
    accounts_iterator i = accounts.find(std::string(s, len));
    assert(i != accounts.end());
    curr_entry->xacts.back()->acct = (*i).second;

    std::string value = curr_quant + " " + (*i).second->comm->symbol;
    curr_entry->xacts.back()->cost = create_amount(value.c_str(), curr_value);
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

bool parse_gnucash(std::istream& in, std::vector<entry *>& ledger)
{
  char buf[BUFSIZ];

  XML_Parser parser = XML_ParserCreate(NULL);
  current_parser = parser;

  //XML_SetUserData(parser, &depth);
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  current_ledger = &ledger;

  curr_account = NULL;
  curr_entry   = NULL;
  curr_comm    = NULL;

  action = NO_ACTION;

  while (! in.eof()) {
    in.getline(buf, BUFSIZ - 1);
    if (! XML_Parse(parser, buf, std::strlen(buf), in.eof())) {
      std::cerr << XML_ErrorString(XML_GetErrorCode(parser))
		<< " at line " << XML_GetCurrentLineNumber(parser)
		<< std::endl;
      return false;
    }
  }
  XML_ParserFree(parser);

  return true;
}

} // namespace ledger
