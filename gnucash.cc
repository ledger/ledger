#include <sstream>
#include <cstring>

#include "ledger.h"

extern "C" {
#include <xmlparse.h>           // expat XML parser
}

namespace ledger {

typedef std::map<const std::string, account_t *> accounts_map;
typedef std::pair<const std::string, account_t *> accounts_pair;

typedef std::map<account_t *, commodity_t *> account_comm_map;
typedef std::pair<account_t *, commodity_t *> account_comm_pair;

static ledger_t *	curr_ledger;
static account_t *	curr_account;
static commodity_t *	curr_account_comm;
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
    curr_account = new account_t(curr_account);
  }
  else if (std::strcmp(name, "act:name") == 0)
    action = ACCOUNT_NAME;
  else if (std::strcmp(name, "act:id") == 0)
    action = ACCOUNT_ID;
  else if (std::strcmp(name, "act:parent") == 0)
    action = ACCOUNT_PARENT;
  else if (std::strcmp(name, "gnc:commodity") == 0) {
    assert(! curr_comm);
    curr_comm = new commodity_t("");
  }
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
    curr_entry->add_transaction(new transaction_t(curr_entry, curr_account,
						  amount_t()));
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
      curr_ledger->add_account(curr_account);
    accounts_by_id.insert(accounts_pair(curr_account_id, curr_account));
    curr_account = NULL;
  }
  else if (std::strcmp(name, "gnc:commodity") == 0) {
    assert(curr_comm);
    commodity_t::add_commodity(curr_comm);
    curr_comm = NULL;
  }
  else if (std::strcmp(name, "gnc:transaction") == 0) {
    assert(curr_entry);
    if (! curr_ledger->add_entry(curr_entry))
      assert(0);
    curr_entry = NULL;
  }
  action = NO_ACTION;
}


static amount_t convert_number(const std::string& number)
{
  const char * num = number.c_str();

  if (char * p = std::strchr(num, '/')) {
    std::string numer_str(num, p - num);
    std::string denom_str(p + 1);

    amount_t amt(numer_str);
    amount_t den(denom_str);

    return amt / den;
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
    (*i).second->add_account(curr_account);
    break;
  }

  case COMM_SYM:
    if (curr_comm)
      curr_comm->symbol = std::string(s, len);
    else if (curr_account)
      curr_account_comm = commodity_t::find_commodity(std::string(s, len));
    else if (curr_entry)
      entry_comm = commodity_t::find_commodity(std::string(s, len));
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
    curr_entry->payee = std::string(s, len);
    break;

  case XACT_STATE:
    if (*s == 'y')
      curr_entry->state = entry_t::PENDING;
    else
      curr_entry->state = entry_t::CLEARED;
    break;

  case XACT_VALUE:
    assert(entry_comm);
    curr_value = convert_number(std::string(s, len) + " " +
				entry_comm->symbol);
    break;

  case XACT_QUANTITY:
    curr_quant = convert_number(std::string(s, len));
    break;

  case XACT_ACCOUNT: {
    accounts_map::iterator i = accounts_by_id.find(std::string(s, len));
    if (i == accounts_by_id.end()) {
      std::cerr << "Could not find account " << std::string(s, len)
		<< std::endl;
      std::exit(1);
    }

    transaction_t * xact = curr_entry->transactions.back();
    xact->account = (*i).second;

    account_comm_map::iterator ac = account_comms.find(xact->account);
    if (ac == account_comms.end()) {
      std::cerr << "Could not find account " << *(xact->account)
		<< std::endl;
      std::exit(1);
    }
    commodity_t * default_commodity = (*ac).second;

    curr_quant.commodity = default_commodity;
    amount_t value = curr_quant.round(default_commodity->precision);

    if (curr_value.commodity == default_commodity)
      curr_value = value;

    xact->amount = value;
    xact->cost   = curr_value;
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

int parse_gnucash(std::istream& in, ledger_t * ledger, account_t * master)
{
  char buf[BUFSIZ];

  count        = 0;
  action       = NO_ACTION;
  curr_ledger  = ledger;
  curr_account = NULL;
  curr_entry   = NULL;
  curr_comm    = NULL;
  entry_comm   = NULL;

  // GnuCash uses the USD commodity without defining it, which really
  // means $.
  commodity_t * usd = new commodity_t("$", 2, COMMODITY_STYLE_THOUSANDS);
  commodity_t::add_commodity(usd);
  commodity_t::add_commodity(usd, "USD");

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

  return count;
}

} // namespace ledger
