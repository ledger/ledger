#include "ledger.h"

#include <vector>
#include <fstream>
#include <sstream>
#include <cstring>
#include <ctime>
#include <cctype>

#include <sys/stat.h>

#define TIMELOG_SUPPORT 1

namespace ledger {

       unsigned long		  binary_magic_number   = 0xFFEED765;
static unsigned long		  format_version = 0x00020009;

static char			  buf[4096];

static std::vector<account_t *>   accounts;
static std::vector<commodity_t *> commodities;

static unsigned long		  ident;
static unsigned long		  c_ident;


void read_binary_amount(std::istream& in, amount_t& amt)
{
  unsigned long id;

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1001);
  }
#endif

  in.read((char *)&id, sizeof(id));
  if (id == 0xffffffff)
    amt.commodity = NULL;
  else
    amt.commodity = commodities[id];

  amt.read_quantity(in);

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1002);
  }
#endif
}

transaction_t * read_binary_transaction(std::istream& in, entry_t * entry)
{
  transaction_t * xact = new transaction_t(entry, NULL);

  unsigned long id;

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1003);
  }
#endif

  in.read((char *)&id, sizeof(id));
  xact->account = accounts[id];
  xact->account->add_transaction(xact);

  read_binary_amount(in, xact->amount);
  read_binary_amount(in, xact->cost);

  in.read((char *)&xact->flags, sizeof(xact->flags));

  unsigned short len;
  in.read((char *)&len, sizeof(len));
  if (len) {
    in.read(buf, len);
    buf[len] = '\0';
    xact->note = buf;
  }

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1004);
  }
#endif

  return xact;
}

entry_t * read_binary_entry(std::istream& in, journal_t * journal)
{
  entry_t * entry = new entry_t;

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1005);
  }
#endif

  in.read((char *)&entry->date, sizeof(entry->date));
  in.read((char *)&entry->state, sizeof(entry->state));

  unsigned short len;
  in.read((char *)&len, sizeof(len));
  if (len) {
    in.read(buf, len);
    buf[len] = '\0';
    entry->code = buf;
  }

  in.read((char *)&len, sizeof(len));
  if (len) {
    in.read(buf, len);
    buf[len] = '\0';
    entry->payee = buf;
  }

  unsigned long count;
  in.read((char *)&count, sizeof(count));

  for (int i = count; --i >= 0; ) {
    transaction_t * xact = read_binary_transaction(in, entry);
    entry->transactions.push_back(xact);
  }

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1006);
  }
#endif

  return entry;
}

commodity_t * read_binary_commodity(std::istream& in)
{
  unsigned long id;

  commodity_t * commodity = new commodity_t;
  commodities.push_back(commodity);

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1007);
  }
#endif

  in.read((char *)&id, sizeof(id));
  commodity->ident = id;
  assert(id == commodities.size() - 1);

  unsigned short len;
  in.read((char *)&len, sizeof(len));
  if (len) {
    in.read(buf, len);
    buf[len] = '\0';
    commodity->symbol = buf;
  }

  in.read((char *)&len, sizeof(len));
  if (len) {
    in.read(buf, len);
    buf[len] = '\0';
    commodity->name = buf;
  }

  in.read((char *)&len, sizeof(len));
  if (len) {
    in.read(buf, len);
    buf[len] = '\0';
    commodity->note = buf;
  }

  in.read((char *)&commodity->precision, sizeof(commodity->precision));
  in.read((char *)&commodity->flags, sizeof(commodity->flags));

  unsigned long count;
  in.read((char *)&count, sizeof(count));

  for (int i = count; --i >= 0; ) {
    std::time_t when;
    in.read((char *)&when, sizeof(std::time_t));
    amount_t amt;
    read_binary_amount(in, amt);
    commodity->history.insert(history_pair(when, amt));
  }

  read_binary_amount(in, commodity->conversion);

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1008);
  }
#endif

  return commodity;
}

account_t * read_binary_account(std::istream& in, account_t * master = NULL)
{
  unsigned long id;

  account_t * acct = new account_t(NULL);
  accounts.push_back(acct);

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1009);
  }
#endif

  in.read((char *)&id, sizeof(id));
  acct->ident = id;
  assert(id == accounts.size() - 1);

  in.read((char *)&id, sizeof(id));
  if (id == 0xffffffff)
    acct->parent = NULL;
  else
    acct->parent = accounts[id];

  unsigned short len;
  in.read((char *)&len, sizeof(len));
  if (len) {
    in.read(buf, len);
    buf[len] = '\0';
    acct->name = buf;
  }

  in.read((char *)&len, sizeof(len));
  if (len) {
    in.read(buf, len);
    buf[len] = '\0';
    acct->note = buf;
  }

  in.read((char *)&len, sizeof(len));

  // If all of the subaccounts will be added to a different master
  // account, throw away what we've learned about the recorded
  // journal's own master account.

  if (master) {
    delete acct;
    acct = master;
  }

  for (int i = 0; i < len; i++) {
    account_t * child = read_binary_account(in);
    child->parent = acct;
    acct->add_account(child);
  }

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1010);
  }
#endif

  return acct;
}

unsigned int read_binary_journal(std::istream&	   in,
				const std::string& leader,
				journal_t *	   journal,
				account_t *	   master)
{
  ident = 0;
  c_ident = 0;

  unsigned long magic;
  in.read((char *)&magic, sizeof(magic));
  if (magic != binary_magic_number)
    return 0;

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1011);
  }
#endif

  unsigned long this_ver;
  in.read((char *)&this_ver, sizeof(this_ver));
  if (this_ver != format_version)
    return 0;

  unsigned short len;
  in.read((char *)&len, sizeof(len));
  if (! len)
    return 0;
  in.read(buf, len);

  if (leader != buf)
    return 0;

  in.read((char *)&len, sizeof(len));

  for (int i = len; --i >= 0; ) {
    in.read((char *)&len, sizeof(len));
    assert(len);
    in.read(buf, len);
    buf[len] = '\0';

    journal->sources.push_back(buf);

    std::time_t old_mtime;
    struct stat info;
    in.read((char *)&old_mtime, sizeof(old_mtime));
    stat(buf, &info);
    if (info.st_mtime > old_mtime)
      return 0;
  }

  journal->master = read_binary_account(in, master);

  unsigned long count;
  in.read((char *)&count, sizeof(count));

  for (int i = count; --i >= 0; ) {
    commodity_t * commodity = read_binary_commodity(in);
    std::pair<commodities_map::iterator, bool> result
      = commodity_t::commodities.insert(commodities_pair(commodity->symbol,
							 commodity));
    assert(result.second || master);
  }

  in.read((char *)&count, sizeof(count));

  for (int i = count; --i >= 0; ) {
    entry_t * entry = read_binary_entry(in, journal);
    journal->entries.push_back(entry);
  }

#ifdef DEBUG
  {
    unsigned short guard;
    in.read((char *)&guard, sizeof(guard));
    assert(guard == 0x1012);
  }
#endif

  accounts.clear();
  commodities.clear();

  return count;
}


void write_binary_amount(std::ostream& out, const amount_t& amt)
{
#ifdef DEBUG
  {
    unsigned short guard = 0x1001;
    out.write((char *)&guard, sizeof(guard));
  }
#endif

  if (amt.commodity) {
    out.write((char *)&amt.commodity->ident, sizeof(amt.commodity->ident));
  } else {
    unsigned long end = 0xffffffff;
    out.write((char *)&end, sizeof(end));
  }

  amt.write_quantity(out);

#ifdef DEBUG
  {
    unsigned short guard = 0x1002;
    out.write((char *)&guard, sizeof(guard));
  }
#endif
}

void write_binary_transaction(std::ostream& out, transaction_t * xact)
{
#ifdef DEBUG
  {
    unsigned short guard = 0x1003;
    out.write((char *)&guard, sizeof(guard));
  }
#endif

  out.write((char *)&xact->account->ident, sizeof(xact->account->ident));
  write_binary_amount(out, xact->amount);
  write_binary_amount(out, xact->cost);
  out.write((char *)&xact->flags, sizeof(xact->flags));

  unsigned short len = xact->note.length();
  out.write((char *)&len, sizeof(len));
  if (len)
    out.write(xact->note.c_str(), len);

#ifdef DEBUG
  {
    unsigned short guard = 0x1004;
    out.write((char *)&guard, sizeof(guard));
  }
#endif
}

void write_binary_entry(std::ostream& out, entry_t * entry)
{
#ifdef DEBUG
  {
    unsigned short guard = 0x1005;
    out.write((char *)&guard, sizeof(guard));
  }
#endif

  out.write((char *)&entry->date, sizeof(entry->date));
  out.write((char *)&entry->state, sizeof(entry->state));

  unsigned short len = entry->code.length();
  out.write((char *)&len, sizeof(len));
  if (len)
    out.write(entry->code.c_str(), len);

  len = entry->payee.length();
  out.write((char *)&len, sizeof(len));
  if (len)
    out.write(entry->payee.c_str(), len);

  unsigned long count = entry->transactions.size();
  out.write((char *)&count, sizeof(count));

  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    write_binary_transaction(out, *i);

#ifdef DEBUG
  {
    unsigned short guard = 0x1006;
    out.write((char *)&guard, sizeof(guard));
  }
#endif
}

void write_binary_commodity(std::ostream& out, commodity_t * commodity)
{
#ifdef DEBUG
  {
    unsigned short guard = 0x1007;
    out.write((char *)&guard, sizeof(guard));
  }
#endif

  out.write((char *)&c_ident, sizeof(c_ident));
  commodity->ident = c_ident;
  ++c_ident;

  unsigned short len = commodity->symbol.length();
  out.write((char *)&len, sizeof(len));
  out.write(commodity->symbol.c_str(), len);

  len = commodity->name.length();
  out.write((char *)&len, sizeof(len));
  if (len)
    out.write(commodity->name.c_str(), len);

  len = commodity->note.length();
  out.write((char *)&len, sizeof(len));
  if (len)
    out.write(commodity->note.c_str(), len);

  out.write((char *)&commodity->precision, sizeof(commodity->precision));
  out.write((char *)&commodity->flags, sizeof(commodity->flags));

  unsigned long count = commodity->history.size();
  out.write((char *)&count, sizeof(count));

  for (history_map::const_iterator i = commodity->history.begin();
       i != commodity->history.end();
       i++) {
    out.write((char *)&((*i).first), sizeof(std::time_t));
    write_binary_amount(out, (*i).second);
  }

  write_binary_amount(out, commodity->conversion);

#ifdef DEBUG
  {
    unsigned short guard = 0x1008;
    out.write((char *)&guard, sizeof(guard));
  }
#endif
}

void write_binary_account(std::ostream& out, account_t * account)
{
#ifdef DEBUG
  {
    unsigned short guard = 0x1009;
    out.write((char *)&guard, sizeof(guard));
  }
#endif

  out.write((char *)&ident, sizeof(ident));
  account->ident = ident;
  ++ident;

  if (account->parent) {
    out.write((char *)&account->parent->ident, sizeof(account->parent->ident));
  } else {
    unsigned long end = 0xffffffff;
    out.write((char *)&end, sizeof(end));
  }

  unsigned short len = account->name.length();
  out.write((char *)&len, sizeof(len));
  if (len)
    out.write(account->name.c_str(), len);

  len = account->note.length();
  out.write((char *)&len, sizeof(len));
  if (len)
    out.write(account->note.c_str(), len);

  len = account->accounts.size();
  out.write((char *)&len, sizeof(len));

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    write_binary_account(out, (*i).second);

#ifdef DEBUG
  {
    unsigned short guard = 0x1010;
    out.write((char *)&guard, sizeof(guard));
  }
#endif
}

void write_binary_journal(std::ostream& out, journal_t * journal,
			 const std::string& leader)
{
  out.write((char *)&binary_magic_number, sizeof(binary_magic_number));

#ifdef DEBUG
  {
    unsigned short guard = 0x1011;
    out.write((char *)&guard, sizeof(guard));
  }
#endif

  out.write((char *)&format_version, sizeof(format_version));

  unsigned short len = leader.length();
  assert(len > 0);
  out.write((char *)&len, sizeof(len));
  out.write(leader.c_str(), len);

  len = journal->sources.size();
  out.write((char *)&len, sizeof(len));

  for (std::list<std::string>::const_iterator i = journal->sources.begin();
       i != journal->sources.end();
       i++) {
    len = (*i).length();
    out.write((char *)&len, sizeof(len));
    assert(len);
    out.write((*i).c_str(), len);
    struct stat info;
    stat((*i).c_str(), &info);
    out.write((char *)&info.st_mtime, sizeof(info.st_mtime));
  }

  write_binary_account(out, journal->master);

  unsigned long count = commodity_t::commodities.size();
  out.write((char *)&count, sizeof(count));

  for (commodities_map::const_iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++)
    write_binary_commodity(out, (*i).second);

  count = journal->entries.size();
  out.write((char *)&count, sizeof(count));

  for (entries_list::const_iterator i = journal->entries.begin();
       i != journal->entries.end();
       i++)
    write_binary_entry(out, *i);

#ifdef DEBUG
  {
    unsigned short guard = 0x1012;
    out.write((char *)&guard, sizeof(guard));
  }
#endif
}

} // namespace ledger
