#include "journal.h"
#include "valexpr.h"
#include "binary.h"

#include <fstream>
#include <ctime>
#include <sys/stat.h>

#define TIMELOG_SUPPORT 1

namespace ledger {

static unsigned long  binary_magic_number = 0xFFEED765;
static unsigned long  format_version      = 0x00020025;

static account_t **   accounts;
static account_t **   accounts_next;
static unsigned int   account_index;

static commodity_t ** commodities;
static commodity_t ** commodities_next;
static unsigned int   commodity_index;

extern char *         bigints;
extern char *	      bigints_next;
extern unsigned int   bigints_index;
extern unsigned int   bigints_count;

#if DEBUG_LEVEL >= ALPHA
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
  read_binary_number(in, num);
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

template <typename T>
inline void read_binary_number(char *& data, T& num) {
  num = *((T *) data);
  data += sizeof(T);
}

template <typename T>
inline T read_binary_number(char *& data) {
  T num;
  read_binary_number(data, num);
  return num;
}

inline void read_binary_string(char *& data, std::string& str)
{
#if DEBUG_LEVEL >= ALPHA
  unsigned short guard;
  guard = *((unsigned short *) data);
  data += sizeof(unsigned short);
  assert(guard == 0x3001);
#endif

  unsigned char len = *data++;
  if (len == 0xff) {
    unsigned short slen = *((unsigned short *) data);
    str = std::string(data + sizeof(unsigned short), slen);
    data += sizeof(unsigned short) + slen;
  }
  else if (len) {
    str = std::string(data, len);
    data += len;
  }
  else {
    str = "";
  }

#if DEBUG_LEVEL >= ALPHA
  guard = *((unsigned short *) data);
  data += sizeof(unsigned short);
  assert(guard == 0x3002);
#endif
}

inline std::string read_binary_string(char *& data)
{
  std::string temp;
  read_binary_string(data, temp);
  return temp;
}

inline void read_binary_string(char *& data, std::string * str)
{
#if DEBUG_LEVEL >= ALPHA
  unsigned short guard;
  guard = *((unsigned short *) data);
  data += sizeof(unsigned short);
  assert(guard == 0x3001);
#endif

  unsigned char len = *data++;
  if (len == 0xff) {
    unsigned short slen = *((unsigned short *) data);
    new(str) std::string(data + sizeof(unsigned short), slen);
    data += sizeof(unsigned short) + slen;
  }
  else if (len) {
    new(str) std::string(data, len);
    data += len;
  }
  else {
    new(str) std::string("");
  }

#if DEBUG_LEVEL >= ALPHA
  guard = *((unsigned short *) data);
  data += sizeof(unsigned short);
  assert(guard == 0x3002);
#endif
}

inline void read_binary_amount(char *& data, amount_t& amt)
{
  commodity_t::ident_t ident;
  read_binary_number(data, ident);
  if (ident == 0xffffffff)
    amt.commodity_ = NULL;
  else
    amt.commodity_ = commodities[ident - 1];

  amt.read_quantity(data);
}

inline void read_binary_transaction(char *& data, transaction_t * xact)
{
  xact->account = accounts[read_binary_number<account_t::ident_t>(data) - 1];

  read_binary_amount(data, xact->amount);

  if (*data++ == 1) {
    xact->cost = new amount_t;
    read_binary_amount(data, *xact->cost);
  } else {
    xact->cost = NULL;
  }
  read_binary_number(data, xact->flags);
  xact->flags |= TRANSACTION_BULK_ALLOC;
  read_binary_string(data, &xact->note);

  xact->data = NULL;
}

inline void read_binary_entry_base(char *& data, entry_base_t * entry,
				   transaction_t *& xact_pool)
{
  for (unsigned long i = 0, count = read_binary_number<unsigned long>(data);
       i < count;
       i++) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor transaction_t");
    read_binary_transaction(data, xact_pool);
    entry->add_transaction(xact_pool++);
  }
}

inline void read_binary_entry(char *& data, entry_t * entry,
			      transaction_t *& xact_pool)
{
  read_binary_number(data, entry->date);
  read_binary_number(data, entry->state);
  read_binary_string(data, &entry->code);
  read_binary_string(data, &entry->payee);
  read_binary_entry_base(data, entry, xact_pool);
}

inline void read_binary_auto_entry(char *& data, auto_entry_t * entry,
				   transaction_t *& xact_pool)
{
  read_binary_string(data, &entry->predicate_string);
  entry->predicate = new item_predicate<transaction_t>(entry->predicate_string);
  read_binary_entry_base(data, entry, xact_pool);
}

inline void read_binary_period_entry(char *& data, period_entry_t * entry,
				     transaction_t *& xact_pool)
{
  read_binary_string(data, &entry->period_string);
  std::istringstream stream(entry->period_string);
  entry->period.parse(stream);
  read_binary_entry_base(data, entry, xact_pool);
}

inline commodity_t * read_binary_commodity(char *& data)
{
  commodity_t * commodity = new commodity_t;
  *commodities_next++ = commodity;

  read_binary_string(data, *(const_cast<std::string *>(&commodity->symbol)));
  read_binary_number(data, commodity->quote);
  read_binary_string(data, commodity->name);
  read_binary_string(data, commodity->note);
  read_binary_number(data, commodity->precision);
  read_binary_number(data, commodity->flags);
  read_binary_number(data, commodity->ident);

  return commodity;
}

inline void read_binary_commodity_extra(char *& data,
					commodity_t::ident_t ident)
{
  commodity_t * commodity = commodities[ident];

  for (unsigned long i = 0, count = read_binary_number<unsigned long>(data);
       i < count;
       i++) {
    std::time_t when;
    read_binary_number(data, when);
    amount_t amt;
    read_binary_amount(data, amt);

    // Upon insertion, amt will be copied, which will cause the amount
    // to be duplicated (and thus not lost when the journal's
    // item_pool is deleted.
    if (! commodity->history)
      commodity->history = new commodity_t::history_t;
    commodity->history->prices.insert(history_pair(when, amt));
  }
  if (commodity->history)
    read_binary_number(data, commodity->history->last_lookup);

  unsigned char flag;

  flag = read_binary_number<unsigned char>(data);
  if (flag) {
    amount_t amt;
    read_binary_amount(data, amt);
    commodity->smaller = new amount_t(amt);
  }

  flag = read_binary_number<unsigned char>(data);
  if (flag) {
    amount_t amt;
    read_binary_amount(data, amt);
    commodity->larger = new amount_t(amt);
  }
}

inline
account_t * read_binary_account(char *& data, account_t * master = NULL)
{
  account_t * acct = new account_t(NULL);
  *accounts_next++ = acct;

  acct->ident = read_binary_number<account_t::ident_t>(data);

  account_t::ident_t id;
  read_binary_number(data, id);	// parent id
  if (id == 0xffffffff)
    acct->parent = NULL;
  else
    acct->parent = accounts[id - 1];

  read_binary_string(data, acct->name);
  read_binary_string(data, acct->note);
  read_binary_number(data, acct->depth);

  // If all of the subaccounts will be added to a different master
  // account, throw away what we've learned about the recorded
  // journal's own master account.

  if (master) {
    delete acct;
    acct = master;
  }

  for (account_t::ident_t i = 0,
	 count = read_binary_number<account_t::ident_t>(data);
       i < count;
       i++) {
    account_t * child = read_binary_account(data);
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
  account_index   =
  commodity_index = 0;

  // Read in the files that participated in this journal, so that they
  // can be checked for changes on reading.

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

  // Read all of the data in at once, so that we're just dealing with
  // a big data buffer.

  unsigned long data_size = read_binary_number<unsigned long>(in);

  char * data_pool = new char[data_size];
  char * data = data_pool;
  in.read(data, data_size);

  // Read in the accounts

  account_t::ident_t a_count = read_binary_number<account_t::ident_t>(data);
  accounts = accounts_next = new account_t *[a_count];
  journal->master = read_binary_account(data, master);

  // Allocate the memory needed for the entries and transactions in
  // one large block, which is then chopped up and custom constructed
  // as necessary.

  unsigned long count        = read_binary_number<unsigned long>(data);
  unsigned long auto_count   = read_binary_number<unsigned long>(data);
  unsigned long period_count = read_binary_number<unsigned long>(data);
  unsigned long xact_count   = read_binary_number<unsigned long>(data);
  unsigned long bigint_count = read_binary_number<unsigned long>(data);

  std::size_t pool_size = (sizeof(entry_t) * count +
			   sizeof(transaction_t) * xact_count +
			   sizeof_bigint_t() * bigint_count);

  char * item_pool = new char[pool_size];

  entry_t *	  entry_pool = (entry_t *) item_pool;
  transaction_t * xact_pool  = (transaction_t *) (item_pool +
						  sizeof(entry_t) * count);
  bigints_index = 0;
  bigints = bigints_next = (item_pool + sizeof(entry_t) * count +
			    sizeof(transaction_t) * xact_count);

  // Read in the commodities

  commodity_t::ident_t c_count = read_binary_number<commodity_t::ident_t>(data);
  commodities = commodities_next = new commodity_t *[c_count];
  for (commodity_t::ident_t i = 0; i < c_count; i++) {
    commodity_t * commodity = read_binary_commodity(data);
    std::pair<commodities_map::iterator, bool> result
      = commodity_t::commodities.insert(commodities_pair(commodity->symbol,
							 commodity));
    assert(result.second);
  }

  for (commodity_t::ident_t i = 0; i < c_count; i++)
    read_binary_commodity_extra(data, i);

  // Read in the entries and transactions

  for (unsigned long i = 0; i < count; i++) {
    new(entry_pool) entry_t;
    read_binary_entry(data, entry_pool, xact_pool);
    journal->entries.push_back(entry_pool++);
  }

  for (unsigned long i = 0; i < auto_count; i++) {
    auto_entry_t * auto_entry = new auto_entry_t;
    read_binary_auto_entry(data, auto_entry, xact_pool);
    journal->auto_entries.push_back(auto_entry);
  }

  for (unsigned long i = 0; i < period_count; i++) {
    period_entry_t * period_entry = new period_entry_t;
    read_binary_period_entry(data, period_entry, xact_pool);
    journal->period_entries.push_back(period_entry);
  }

  // Clean up and return the number of entries read

  journal->item_pool	 = item_pool;
  journal->item_pool_end = item_pool + pool_size;

  delete[] accounts;
  delete[] commodities;
  delete[] data_pool;

  return count;
}

bool binary_parser_t::test(std::istream& in) const
{
  if (read_binary_number<unsigned long>(in) == binary_magic_number &&
      read_binary_number<unsigned long>(in) == format_version)
    return true;

  in.seekg(0, std::ios::beg);
  return false;
}

unsigned int binary_parser_t::parse(std::istream&	in,
				    journal_t *		journal,
				    account_t *		master,
				    const std::string * original_file)
{
  return read_binary_journal(in, original_file ? *original_file : "",
			     journal, master);
}

#if DEBUG_LEVEL >= ALPHA
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
  if (amt.commodity_)
    write_binary_number(out, amt.commodity().ident);
  else
    write_binary_number<commodity_t::ident_t>(out, 0xffffffff);

  amt.write_quantity(out);
}

void write_binary_transaction(std::ostream& out, transaction_t * xact)
{
  write_binary_number(out, xact->account->ident);
  write_binary_amount(out, xact->amount);
  if (xact->cost) {
    write_binary_number<char>(out, 1);
    write_binary_amount(out, *xact->cost);
  } else {
    write_binary_number<char>(out, 0);
  }
  write_binary_number(out, xact->flags);
  write_binary_string(out, xact->note);
}

void write_binary_entry_base(std::ostream& out, entry_base_t * entry)
{
  write_binary_number<unsigned long>(out, entry->transactions.size());
  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    write_binary_transaction(out, *i);
}

void write_binary_entry(std::ostream& out, entry_t * entry)
{
  write_binary_number(out, entry->date);
  write_binary_number(out, entry->state);
  write_binary_string(out, entry->code);
  write_binary_string(out, entry->payee);
  write_binary_entry_base(out, entry);
}

void write_binary_auto_entry(std::ostream& out, auto_entry_t * entry)
{
  write_binary_string(out, entry->predicate_string);
  write_binary_entry_base(out, entry);
}

void write_binary_period_entry(std::ostream& out, period_entry_t * entry)
{
  write_binary_string(out, entry->period_string);
  write_binary_entry_base(out, entry);
}

void write_binary_commodity(std::ostream& out, commodity_t * commodity)
{
  write_binary_string(out, commodity->symbol);
  write_binary_number(out, commodity->quote);
  write_binary_string(out, commodity->name);
  write_binary_string(out, commodity->note);
  write_binary_number(out, commodity->precision);
  write_binary_number(out, commodity->flags);
  commodity->ident = ++commodity_index;
  write_binary_number(out, commodity->ident);
}

void write_binary_commodity_extra(std::ostream& out, commodity_t * commodity)
{
  if (! commodity->history) {
    write_binary_number<unsigned long>(out, 0);
  } else {
    write_binary_number<unsigned long>(out, commodity->history->prices.size());
    for (history_map::const_iterator i = commodity->history->prices.begin();
	 i != commodity->history->prices.end();
	 i++) {
      write_binary_number(out, (*i).first);
      write_binary_amount(out, (*i).second);
    }
    write_binary_number(out, commodity->history->last_lookup);
  }

  if (commodity->smaller) {
    write_binary_number<unsigned char>(out, 1);
    write_binary_amount(out, *commodity->smaller);
  } else {
    write_binary_number<unsigned char>(out, 0);
  }

  if (commodity->larger) {
    write_binary_number<unsigned char>(out, 1);
    write_binary_amount(out, *commodity->larger);
  } else {
    write_binary_number<unsigned char>(out, 0);
  }
}

static inline account_t::ident_t count_accounts(account_t * account)
{
  account_t::ident_t count = 1;

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    count += count_accounts((*i).second);

  return count;
}

void write_binary_account(std::ostream& out, account_t * account)
{
  account->ident = ++account_index;

  write_binary_number(out, account->ident);
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

  // Write out the files that participated in this journal, so that
  // they can be checked for changes on reading.

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

  std::ostream::pos_type data_val = out.tellp();
  write_binary_number<unsigned long>(out, 0);

  // Write out the accounts

  write_binary_number<account_t::ident_t>(out, count_accounts(journal->master));
  write_binary_account(out, journal->master);

  // Write out the number of entries, transactions, and amounts

  write_binary_number<unsigned long>(out, journal->entries.size());
  write_binary_number<unsigned long>(out, journal->auto_entries.size());
  write_binary_number<unsigned long>(out, journal->period_entries.size());

  std::ostream::pos_type xacts_val = out.tellp();
  write_binary_number<unsigned long>(out, 0);
  std::ostream::pos_type bigints_val = out.tellp();
  write_binary_number<unsigned long>(out, 0);
  bigints_count = 0;

  // Write out the commodities

  write_binary_number<commodity_t::ident_t>
    (out, commodity_t::commodities.size() - 1);

  for (commodities_map::const_iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++)
    if (! (*i).first.empty())
      write_binary_commodity(out, (*i).second);

  for (commodities_map::const_iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++)
    if (! (*i).first.empty())
      write_binary_commodity_extra(out, (*i).second);

  // Write out the entries and transactions

  unsigned long xact_count = 0;

  for (entries_list::const_iterator i = journal->entries.begin();
       i != journal->entries.end();
       i++) {
    write_binary_entry(out, *i);
    xact_count += (*i)->transactions.size();
  }

  for (auto_entries_list::const_iterator i = journal->auto_entries.begin();
       i != journal->auto_entries.end();
       i++) {
    write_binary_auto_entry(out, *i);
    xact_count += (*i)->transactions.size();
  }

  for (period_entries_list::const_iterator i = journal->period_entries.begin();
       i != journal->period_entries.end();
       i++) {
    write_binary_period_entry(out, *i);
    xact_count += (*i)->transactions.size();
  }

  // Back-patch the count for amounts

  unsigned long data_size = (((unsigned long) out.tellp()) -
			     ((unsigned long) data_val) -
			     sizeof(unsigned long));
  out.seekp(data_val);
  write_binary_number<unsigned long>(out, data_size);
  out.seekp(xacts_val);
  write_binary_number<unsigned long>(out, xact_count);
  out.seekp(bigints_val);
  write_binary_number<unsigned long>(out, bigints_count);
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(binary_parse_overloads,
				       binary_parser_t::parse, 2, 4)

void py_write_binary_journal(const std::string& path, journal_t * journal)
{
  std::ofstream out(path.c_str());
  write_binary_journal(out, journal, &journal->sources);
}

void export_binary() {
  class_< binary_parser_t, bases<parser_t> > ("BinaryParser")
    .def("test", &binary_parser_t::test)
    .def("parse", &binary_parser_t::parse, binary_parse_overloads())
    ;

  def("write_binary_journal", py_write_binary_journal);
}

#endif // USE_BOOST_PYTHON
