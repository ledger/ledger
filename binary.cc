#include "ledger.h"
#include "binary.h"

#include <ctime>
#include <sys/stat.h>

#define TIMELOG_SUPPORT 1

namespace ledger {

const unsigned long	   binary_magic_number = 0xFFEED765;
static const unsigned long format_version      = 0x0002000c;

bool binary_parser_t::test(std::istream& in) const
{
  unsigned long magic;
  in.read((char *)&magic, sizeof(magic));
  in.seekg(0);

  return magic == binary_magic_number;
}

static std::deque<account_t *>   accounts;
static account_t::ident_t	 ident;
static std::deque<commodity_t *> commodities;
static commodity_t::ident_t	 c_ident;
std::deque<amount_t::bigint_t *> bigints;

#if RELEASE_LEVEL >= ALPHA
#define read_binary_guard(in, id) {		\
  unsigned short guard;				\
  in.read((char *)&guard, sizeof(guard));	\
  assert(guard == id);				\
}
#else
#define read_binary_guard(in, id)
#endif

template <typename T>
inline void read_binary_number(std::istream& in, T& num) {
  in.read((char *)&num, sizeof(num));
}

template <typename T>
inline T read_binary_number(std::istream& in) {
  T num;
  in.read((char *)&num, sizeof(num));
  return num;
}

inline void read_binary_string(std::istream& in, std::string& str)
{
  read_binary_guard(in, 0x3001);

  unsigned char len;
  read_binary_number(in, len);
  if (len == 0xff) {
    unsigned short slen;
    read_binary_number(in, slen);
    char * buf = new char[slen + 1];
    in.read(buf, slen);
    buf[slen] = '\0';
    str = buf;
    delete[] buf;
  }
  else if (len) {
    char buf[256];
    in.read(buf, len);
    buf[len] = '\0';
    str = buf;
  } else {
    str = "";
  }

  read_binary_guard(in, 0x3002);
}

inline std::string read_binary_string(std::istream& in)
{
  std::string temp;
  read_binary_string(in, temp);
  return temp;
}

void read_binary_amount(std::istream& in, amount_t& amt)
{
  commodity_t::ident_t id;
  read_binary_number(in, id);
  if (id == 0xffffffff)
    amt.commodity = NULL;
  else
    amt.commodity = commodities[id];

  amt.read_quantity(in);
}

transaction_t * read_binary_transaction(std::istream& in, entry_t * entry)
{
  transaction_t * xact = new transaction_t(NULL);

  xact->account = accounts[read_binary_number<account_t::ident_t>(in)];
  xact->account->add_transaction(xact);

  read_binary_amount(in, xact->amount);
  read_binary_amount(in, xact->cost);
  read_binary_number(in, xact->flags);
  read_binary_string(in, xact->note);

  return xact;
}

entry_t * read_binary_entry(std::istream& in, journal_t * journal)
{
  entry_t * entry = new entry_t;

  read_binary_number(in, entry->date);
  read_binary_number(in, entry->state);
  read_binary_string(in, entry->code);
  read_binary_string(in, entry->payee);

  for (unsigned long i = 0, count = read_binary_number<unsigned long>(in);
       i < count;
       i++) {
    transaction_t * xact = read_binary_transaction(in, entry);
    entry->add_transaction(xact);
  }

  return entry;
}

commodity_t * read_binary_commodity(std::istream& in)
{
  commodity_t * commodity = new commodity_t;
  commodities.push_back(commodity);

  commodity->ident = read_binary_number<commodity_t::ident_t>(in);
  assert(commodity->ident == commodities.size() - 1);

  read_binary_string(in, commodity->symbol);
  read_binary_string(in, commodity->name);
  read_binary_string(in, commodity->note);
  read_binary_number(in, commodity->precision);
  read_binary_number(in, commodity->flags);

  for (unsigned long i = 0, count = read_binary_number<unsigned long>(in);
       i < count;
       i++) {
    std::time_t when;
    read_binary_number(in, when);
    amount_t amt;
    read_binary_amount(in, amt);
    commodity->history.insert(history_pair(when, amt));
  }

  read_binary_amount(in, commodity->conversion);

  return commodity;
}

account_t * read_binary_account(std::istream& in, account_t * master = NULL)
{
  account_t * acct = new account_t(NULL);
  accounts.push_back(acct);

  acct->ident = read_binary_number<account_t::ident_t>(in);
  assert(acct->ident == accounts.size() - 1);

  account_t::ident_t id;
  read_binary_number(in, id);	// parent id
  if (id == 0xffffffff)
    acct->parent = NULL;
  else
    acct->parent = accounts[id];

  read_binary_string(in, acct->name);
  read_binary_string(in, acct->note);
  read_binary_number(in, acct->depth);

  // If all of the subaccounts will be added to a different master
  // account, throw away what we've learned about the recorded
  // journal's own master account.

  if (master) {
    delete acct;
    acct = master;
  }

  for (account_t::ident_t i = 0,
	 count = read_binary_number<account_t::ident_t>(in);
       i < count;
       i++) {
    account_t * child = read_binary_account(in);
    child->parent = acct;
    acct->add_account(child);
  }

  return acct;
}

unsigned int read_binary_journal(std::istream&	    in,
				 const std::string& file,
				 journal_t *	    journal,
				 account_t *	    master)
{
  ident   = 0;
  c_ident = 0;

  if (read_binary_number<unsigned long>(in) != binary_magic_number ||
      read_binary_number<unsigned long>(in) != format_version)
    return 0;

  if (! file.empty()) {
    for (unsigned short i = 0,
	   count = read_binary_number<unsigned short>(in);
	 i < count;
	 i++) {
      std::string path = read_binary_string(in);
      if (i == 0 && path != file)
	return 0;

      std::time_t old_mtime;
      read_binary_number(in, old_mtime);
      struct stat info;
      stat(path.c_str(), &info);
      if (std::difftime(info.st_mtime, old_mtime) > 0)
	return 0;

      journal->sources.push_back(path);
    }
  }

  journal->master = read_binary_account(in, master);

  for (account_t::ident_t i = 0,
	 count = read_binary_number<account_t::ident_t>(in);
       i < count;
       i++) {
    commodity_t * commodity = read_binary_commodity(in);
    std::pair<commodities_map::iterator, bool> result
      = commodity_t::commodities.insert(commodities_pair(commodity->symbol,
							 commodity));
    assert(result.second || master);
  }

  unsigned int count = read_binary_number<unsigned long>(in);
  for (unsigned long i = 0;
       i < count;
       i++) {
    entry_t * entry = read_binary_entry(in, journal);
    journal->entries.push_back(entry);
  }

  accounts.clear();
  commodities.clear();
  bigints.clear();

  return count;
}

unsigned int binary_parser_t::parse(std::istream&	in,
				    journal_t *		journal,
				    account_t *		master,
				    const std::string * original_file)
{
  return read_binary_journal(in, original_file ? *original_file : "",
			     journal, master);
}

#if RELEASE_LEVEL >= ALPHA
#define write_binary_guard(in, id) {		\
  unsigned short guard = id;			\
  out.write((char *)&guard, sizeof(guard));	\
}
#else
#define write_binary_guard(in, id)
#endif

template <typename T>
inline void write_binary_number(std::ostream& out, T num) {
  out.write((char *)&num, sizeof(num));
}

inline void write_binary_string(std::ostream& out, const std::string& str)
{
  write_binary_guard(out, 0x3001);

  unsigned long len = str.length();
  if (len > 255) {
    assert(len < 65536);
    write_binary_number<unsigned char>(out, 0xff);
    write_binary_number<unsigned short>(out, len);
  } else {
    write_binary_number<unsigned char>(out, len);
  }

  if (len)
    out.write(str.c_str(), len);

  write_binary_guard(out, 0x3002);
}

void write_binary_amount(std::ostream& out, const amount_t& amt)
{
  if (amt.commodity)
    write_binary_number(out, amt.commodity->ident);
  else
    write_binary_number<commodity_t::ident_t>(out, 0xffffffff);

  amt.write_quantity(out);
}

void write_binary_transaction(std::ostream& out, transaction_t * xact)
{
  write_binary_number(out, xact->account->ident);
  write_binary_amount(out, xact->amount);
  write_binary_amount(out, xact->cost);
  write_binary_number(out, xact->flags);
  write_binary_string(out, xact->note);
}

void write_binary_entry(std::ostream& out, entry_t * entry)
{
  write_binary_number(out, entry->date);
  write_binary_number(out, entry->state);
  write_binary_string(out, entry->code);
  write_binary_string(out, entry->payee);

  write_binary_number<unsigned long>(out, entry->transactions.size());
  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    write_binary_transaction(out, *i);
}

void write_binary_commodity(std::ostream& out, commodity_t * commodity)
{
  write_binary_number(out, c_ident);
  commodity->ident = c_ident;
  ++c_ident;

  write_binary_string(out, commodity->symbol);
  write_binary_string(out, commodity->name);
  write_binary_string(out, commodity->note);
  write_binary_number(out, commodity->precision);
  write_binary_number(out, commodity->flags);

  write_binary_number<unsigned long>(out, commodity->history.size());
  for (history_map::const_iterator i = commodity->history.begin();
       i != commodity->history.end();
       i++) {
    write_binary_number(out, (*i).first);
    write_binary_amount(out, (*i).second);
  }

  write_binary_amount(out, commodity->conversion);
}

void write_binary_account(std::ostream& out, account_t * account)
{
  write_binary_number(out, ident);
  account->ident = ident;
  ++ident;

  if (account->parent)
    write_binary_number(out, account->parent->ident);
  else
    write_binary_number<account_t::ident_t>(out, 0xffffffff);

  write_binary_string(out, account->name);
  write_binary_string(out, account->note);
  write_binary_number(out, account->depth);

  write_binary_number<account_t::ident_t>(out, account->accounts.size());
  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    write_binary_account(out, (*i).second);
}

void write_binary_journal(std::ostream& out, journal_t * journal,
			  strings_list * files)
{
  write_binary_number(out, binary_magic_number);
  write_binary_number(out, format_version);

  if (! files) {
    write_binary_number<unsigned short>(out, 0);
  } else {
    write_binary_number<unsigned short>(out, files->size());
    for (strings_list::const_iterator i = files->begin();
	 i != files->end();
	 i++) {
      write_binary_string(out, *i);
      struct stat info;
      stat((*i).c_str(), &info);
      write_binary_number(out, std::time_t(info.st_mtime));
    }
  }

  write_binary_account(out, journal->master);

  write_binary_number<commodity_t::ident_t>(out, commodity_t::commodities.size() - 1);
  for (commodities_map::const_iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++)
    if (! (*i).first.empty())
      write_binary_commodity(out, (*i).second);

  write_binary_number<unsigned long>(out, journal->entries.size());
  for (entries_list::const_iterator i = journal->entries.begin();
       i != journal->entries.end();
       i++)
    write_binary_entry(out, *i);
}

} // namespace ledger
