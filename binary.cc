#include "journal.h"
#include "valexpr.h"
#include "binary.h"

#include <fstream>
#include <ctime>
#include <sys/stat.h>

#define TIMELOG_SUPPORT 1

namespace ledger {

static unsigned long binary_magic_number = 0xFFEED765;
#ifdef DEBUG_ENABLED
static unsigned long format_version      = 0x00020603;
#else
static unsigned long format_version      = 0x00020602;
#endif

static account_t **	   accounts;
static account_t **	   accounts_next;
static unsigned int	   account_index;

static commodity_base_t ** base_commodities;
static commodity_base_t ** base_commodities_next;
static unsigned int	   base_commodity_index;

static commodity_t **	   commodities;
static commodity_t **	   commodities_next;
static unsigned int	   commodity_index;

extern char *		   bigints;
extern char *		   bigints_next;
extern unsigned int	   bigints_index;
extern unsigned int	   bigints_count;

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
inline void read_binary_long(std::istream& in, T& num) {
  unsigned char len;
  in.read((char *)&len, sizeof(unsigned char));

  num = 0;
  unsigned char temp;
  if (len > 3) {
    in.read((char *)&temp, sizeof(unsigned char));
    num |= ((unsigned long)temp) << 24;
  }
  if (len > 2) {
    in.read((char *)&temp, sizeof(unsigned char));
    num |= ((unsigned long)temp) << 16;
  }
  if (len > 1) {
    in.read((char *)&temp, sizeof(unsigned char));
    num |= ((unsigned long)temp) << 8;
  }

  in.read((char *)&temp, sizeof(unsigned char));
  num |= ((unsigned long)temp);
}

template <typename T>
inline T read_binary_number(std::istream& in) {
  T num;
  read_binary_number(in, num);
  return num;
}

template <typename T>
inline T read_binary_long(std::istream& in) {
  T num;
  read_binary_long(in, num);
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
inline void read_binary_long(char *& data, T& num) {
  unsigned char len = *((unsigned char *)data++);

  num = 0;
  unsigned char temp;
  if (len > 3) {
    temp =  *((unsigned char *)data++);
    num |= ((unsigned long)temp) << 24;
  }
  if (len > 2) {
    temp =  *((unsigned char *)data++);
    num |= ((unsigned long)temp) << 16;
  }
  if (len > 1) {
    temp =  *((unsigned char *)data++);
    num |= ((unsigned long)temp) << 8;
  }

  temp =  *((unsigned char *)data++);
  num |= ((unsigned long)temp);
}

template <typename T>
inline T read_binary_number(char *& data) {
  T num;
  read_binary_number(data, num);
  return num;
}

template <typename T>
inline T read_binary_long(char *& data) {
  T num;
  read_binary_long(data, num);
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
  read_binary_long(data, ident);
  if (ident == 0xffffffff)
    amt.commodity_ = NULL;
  else if (ident == 0)
    amt.commodity_ = commodity_t::null_commodity;
  else
    amt.commodity_ = commodities[ident - 1];

  amt.read_quantity(data);
}

inline void read_binary_mask(char *& data, mask_t *& mask)
{
  bool exclude;
  read_binary_number(data, exclude);
  std::string pattern;
  read_binary_string(data, pattern);

  mask = new mask_t(pattern);
  mask->exclude = exclude;
}

inline void read_binary_value_expr(char *& data, value_expr_t *& expr)
{
  if (read_binary_number<unsigned char>(data) == 0) {
    expr = NULL;
    return;
  }

  value_expr_t::kind_t kind;
  read_binary_number(data, kind);

  expr = new value_expr_t(kind);

  if (kind > value_expr_t::TERMINALS) {
    read_binary_value_expr(data, expr->left);
    if (expr->left) expr->left->acquire();
  }

  switch (expr->kind) {
  case value_expr_t::CONSTANT_T:
    read_binary_number(data, expr->constant_t);
    break;
  case value_expr_t::CONSTANT_I:
    read_binary_long(data, expr->constant_i);
    break;
  case value_expr_t::CONSTANT_A:
    expr->constant_a = new amount_t();
    read_binary_amount(data, *(expr->constant_a));
    break;
  case value_expr_t::CONSTANT_V:
    assert(0);
    break;

  case value_expr_t::F_CODE_MASK:
  case value_expr_t::F_PAYEE_MASK:
  case value_expr_t::F_NOTE_MASK:
  case value_expr_t::F_ACCOUNT_MASK:
  case value_expr_t::F_SHORT_ACCOUNT_MASK:
  case value_expr_t::F_COMMODITY_MASK:
    if (read_binary_number<unsigned char>(data) == 1)
      read_binary_mask(data, expr->mask);
    break;

  default:
    if (kind > value_expr_t::TERMINALS) {
      read_binary_value_expr(data, expr->right);
      if (expr->right) expr->right->acquire();
    }
    break;
  }
}


inline void read_binary_transaction(char *& data, transaction_t * xact)
{
  read_binary_long(data, xact->_date);
  read_binary_long(data, xact->_date_eff);
  xact->account = accounts[read_binary_long<account_t::ident_t>(data) - 1];

  if (read_binary_number<char>(data) == 1) {
    read_binary_value_expr(data, xact->amount_expr);
    if (xact->amount_expr) xact->amount_expr->acquire();
  } else {
    read_binary_amount(data, xact->amount);
  }

  if (*data++ == 1) {
    xact->cost = new amount_t;

    if (read_binary_number<char>(data) == 1) {
      read_binary_value_expr(data, xact->cost_expr);
      if (xact->cost_expr) xact->cost_expr->acquire();
    } else {
      read_binary_amount(data, *xact->cost);
    }
  } else {
    xact->cost = NULL;
  }

  read_binary_number(data, xact->state);
  read_binary_number(data, xact->flags);
  xact->flags |= TRANSACTION_BULK_ALLOC;
  read_binary_string(data, &xact->note);

  xact->beg_pos = read_binary_long<unsigned long>(data);
  read_binary_long(data, xact->beg_line);
  xact->end_pos = read_binary_long<unsigned long>(data);
  read_binary_long(data, xact->end_line);

  xact->data = NULL;

  if (xact->amount_expr)
    compute_amount(xact->amount_expr, xact->amount, xact);
  if (xact->cost_expr)
    compute_amount(xact->cost_expr, *xact->cost, xact);
}

inline void read_binary_entry_base(char *& data, entry_base_t * entry,
				   transaction_t *& xact_pool, bool& finalize)
{
  read_binary_long(data, entry->src_idx);
  entry->beg_pos = read_binary_long<unsigned long>(data);
  read_binary_long(data, entry->beg_line);
  entry->end_pos = read_binary_long<unsigned long>(data);
  read_binary_long(data, entry->end_line);

  bool ignore_calculated = read_binary_number<char>(data) == 1;

  for (unsigned long i = 0, count = read_binary_long<unsigned long>(data);
       i < count;
       i++) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor transaction_t");
    read_binary_transaction(data, xact_pool);
    if (ignore_calculated && xact_pool->flags & TRANSACTION_CALCULATED)
      finalize = true;
    entry->add_transaction(xact_pool++);
  }
}

inline void read_binary_entry(char *& data, entry_t * entry,
			      transaction_t *& xact_pool, bool& finalize)
{
  read_binary_entry_base(data, entry, xact_pool, finalize);
  read_binary_long(data, entry->_date);
  read_binary_long(data, entry->_date_eff);
  read_binary_string(data, &entry->code);
  read_binary_string(data, &entry->payee);
}

inline void read_binary_auto_entry(char *& data, auto_entry_t * entry,
				   transaction_t *& xact_pool)
{
  bool ignore;
  read_binary_entry_base(data, entry, xact_pool, ignore);
  value_expr_t * expr;
  read_binary_value_expr(data, expr);
  // the item_predicate constructor will acquire the reference
  entry->predicate = new item_predicate<transaction_t>(expr);
}

inline void read_binary_period_entry(char *& data, period_entry_t * entry,
				     transaction_t *& xact_pool, bool& finalize)
{
  read_binary_entry_base(data, entry, xact_pool, finalize);
  read_binary_string(data, &entry->period_string);
  std::istringstream stream(entry->period_string);
  entry->period.parse(stream);
}

inline commodity_base_t * read_binary_commodity_base(char *& data)
{
  commodity_base_t * commodity = new commodity_base_t;
  *base_commodities_next++ = commodity;

  read_binary_string(data, commodity->symbol);
  read_binary_string(data, commodity->name);
  read_binary_string(data, commodity->note);
  read_binary_number(data, commodity->precision);
  read_binary_number(data, commodity->flags);

  return commodity;
}

inline void read_binary_commodity_base_extra(char *& data,
					     commodity_t::ident_t ident)
{
  commodity_base_t * commodity = base_commodities[ident];

  for (unsigned long i = 0, count = read_binary_long<unsigned long>(data);
       i < count;
       i++) {
    std::time_t when;
    read_binary_long(data, when);
    amount_t amt;
    read_binary_amount(data, amt);

    // Upon insertion, amt will be copied, which will cause the amount
    // to be duplicated (and thus not lost when the journal's
    // item_pool is deleted.
    if (! commodity->history)
      commodity->history = new commodity_base_t::history_t;
    commodity->history->prices.insert(history_pair(when, amt));
  }
  if (commodity->history)
    read_binary_long(data, commodity->history->last_lookup);

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

inline commodity_t * read_binary_commodity(char *& data)
{
  commodity_t * commodity = new commodity_t;
  *commodities_next++ = commodity;

  commodity->ptr =
    base_commodities[read_binary_long<commodity_base_t::ident_t>(data) - 1];

  read_binary_string(data, commodity->qualified_symbol);
  commodity->annotated = false;

  return commodity;
}

inline commodity_t * read_binary_commodity_annotated(char *& data)
{
  annotated_commodity_t * commodity = new annotated_commodity_t;
  *commodities_next++ = commodity;

  commodity->ptr =
    base_commodities[read_binary_long<commodity_base_t::ident_t>(data) - 1];

  read_binary_string(data, commodity->qualified_symbol);
  commodity->annotated = true;

  read_binary_amount(data, commodity->price);
  read_binary_long(data, commodity->date);
  read_binary_string(data, commodity->tag);

  return commodity;
}

inline
account_t * read_binary_account(char *& data, journal_t * journal,
				account_t * master = NULL)
{
  account_t * acct = new account_t(NULL);
  *accounts_next++ = acct;

  acct->journal = journal;

  account_t::ident_t id;
  read_binary_long(data, id);	// parent id
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
	 count = read_binary_long<account_t::ident_t>(data);
       i < count;
       i++) {
    account_t * child = read_binary_account(data, journal);
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
  account_index	       = 
  base_commodity_index = 
  commodity_index      = 0;

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
      read_binary_long(in, old_mtime);
      struct stat info;
      stat(path.c_str(), &info);
      if (std::difftime(info.st_mtime, old_mtime) > 0)
	return 0;

      journal->sources.push_back(path);
    }

    // Make sure that the cache uses the same price database,
    // otherwise it means that LEDGER_PRICE_DB has been changed, and
    // we should ignore this cache file.
    if (read_binary_string(in) != journal->price_db)
      return 0;
  }

  // Read all of the data in at once, so that we're just dealing with
  // a big data buffer.

  unsigned long data_size = read_binary_number<unsigned long>(in);

  char * data_pool = new char[data_size];
  char * data = data_pool;
  in.read(data, data_size);

  // Read in the accounts

  account_t::ident_t a_count = read_binary_long<account_t::ident_t>(data);
  accounts = accounts_next = new account_t *[a_count];

  assert(journal->master); delete journal->master;
  journal->master = read_binary_account(data, journal, master);

  if (read_binary_number<bool>(data))
    journal->basket = accounts[read_binary_long<account_t::ident_t>(data) - 1];

  // Allocate the memory needed for the entries and transactions in
  // one large block, which is then chopped up and custom constructed
  // as necessary.

  unsigned long count        = read_binary_long<unsigned long>(data);
  unsigned long auto_count   = read_binary_long<unsigned long>(data);
  unsigned long period_count = read_binary_long<unsigned long>(data);
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

  // Read in the base commodities and then derived commodities

  commodity_base_t::ident_t bc_count =
    read_binary_long<commodity_base_t::ident_t>(data);
  base_commodities = base_commodities_next = new commodity_base_t *[bc_count];

  for (commodity_base_t::ident_t i = 0; i < bc_count; i++) {
    commodity_base_t * commodity = read_binary_commodity_base(data);

    if (commodity->flags & COMMODITY_STYLE_BUILTIN)
      commodity_base_t::commodities.erase(commodity->symbol);

    std::pair<base_commodities_map::iterator, bool> result =
      commodity_base_t::commodities.insert
      (base_commodities_pair(commodity->symbol, commodity));
    if (! result.second)
      throw error(std::string("Failed to read base commodity from cache: ") +
		  commodity->symbol);
  }

  for (commodity_base_t::ident_t i = 0; i < bc_count; i++)
    read_binary_commodity_base_extra(data, i);

  commodity_t::ident_t c_count  = read_binary_long<commodity_t::ident_t>(data);
  commodities = commodities_next = new commodity_t *[c_count];

  for (commodity_t::ident_t i = 0; i < c_count; i++) {
    commodity_t * commodity;
    std::string   mapping_key;

    if (read_binary_number<char>(data) == 0) {
      commodity	  = read_binary_commodity(data);
      mapping_key = commodity->ptr->symbol;
    } else {
      read_binary_string(data, mapping_key);
      commodity = read_binary_commodity_annotated(data);
    }

    if (commodity->flags() & COMMODITY_STYLE_BUILTIN) {
      commodity_t::commodities.erase(mapping_key);
      if (commodity->symbol() == "") {
	delete commodity_t::null_commodity;
	commodity_t::null_commodity = commodity;
      }
    }

    std::pair<commodities_map::iterator, bool> result =
      commodity_t::commodities.insert(commodities_pair(mapping_key,
						       commodity));
    if (! result.second)
      throw error(std::string("Failed to read commodity from cache: ") +
		  commodity->ptr->symbol);
  }

  commodity_t::ident_t ident;
  read_binary_long(data, ident);
  if (ident == 0xffffffff || ident == 0)
    commodity_t::default_commodity = NULL;
  else
    commodity_t::default_commodity = commodities[ident - 1];

  // Read in the entries and transactions

  for (unsigned long i = 0; i < count; i++) {
    new(entry_pool) entry_t;
    bool finalize = false;
    read_binary_entry(data, entry_pool, xact_pool, finalize);
    entry_pool->journal = journal;
    if (finalize && ! entry_pool->finalize())
      continue;
    journal->entries.push_back(entry_pool++);
  }

  for (unsigned long i = 0; i < auto_count; i++) {
    auto_entry_t * auto_entry = new auto_entry_t;
    read_binary_auto_entry(data, auto_entry, xact_pool);
    auto_entry->journal = journal;
    journal->auto_entries.push_back(auto_entry);
  }

  for (unsigned long i = 0; i < period_count; i++) {
    period_entry_t * period_entry = new period_entry_t;
    bool finalize = false;
    read_binary_period_entry(data, period_entry, xact_pool, finalize);
    period_entry->journal = journal;
    if (finalize && ! period_entry->finalize())
      continue;
    journal->period_entries.push_back(period_entry);
  }

  // Clean up and return the number of entries read

  journal->item_pool	 = item_pool;
  journal->item_pool_end = item_pool + pool_size;

  delete[] accounts;
  delete[] commodities;
  delete[] data_pool;

  VALIDATE(journal->valid());

  return count;
}

bool binary_parser_t::test(std::istream& in) const
{
  if (read_binary_number<unsigned long>(in) == binary_magic_number &&
      read_binary_number<unsigned long>(in) == format_version)
    return true;

  in.clear();
  in.seekg(0, std::ios::beg);
  return false;
}

unsigned int binary_parser_t::parse(std::istream&	in,
				    config_t&           config,
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

template <typename T>
inline void write_binary_long(std::ostream& out, T num) {
  unsigned char len = 4;
  if (((unsigned long)num) < 0x00000100UL)
    len = 1;
  else if (((unsigned long)num) < 0x00010000UL)
    len = 2;
  else if (((unsigned long)num) < 0x01000000UL)
    len = 3;
  out.write((char *)&len, sizeof(unsigned char));

  if (len > 3) {
    unsigned char temp = (((unsigned long)num) & 0xFF000000UL) >> 24;
    out.write((char *)&temp, sizeof(unsigned char));
  }
  if (len > 2) {
    unsigned char temp = (((unsigned long)num) & 0x00FF0000UL) >> 16;
    out.write((char *)&temp, sizeof(unsigned char));
  }
  if (len > 1) {
    unsigned char temp = (((unsigned long)num) & 0x0000FF00UL) >> 8;
    out.write((char *)&temp, sizeof(unsigned char));
  }

  unsigned char temp = (((unsigned long)num) & 0x000000FFUL);
  out.write((char *)&temp, sizeof(unsigned char));
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
    write_binary_long(out, amt.commodity_->ident);
  else
    write_binary_long<commodity_t::ident_t>(out, 0xffffffff);

  amt.write_quantity(out);
}

void write_binary_mask(std::ostream& out, mask_t * mask)
{
  write_binary_number(out, mask->exclude);
  write_binary_string(out, mask->pattern);
}

void write_binary_value_expr(std::ostream& out, const value_expr_t * expr)
{
  if (! expr) {
    write_binary_number<unsigned char>(out, 0);
    return;
  }
  write_binary_number<unsigned char>(out, 1);

  write_binary_number(out, expr->kind);

  if (expr->kind > value_expr_t::TERMINALS)
    write_binary_value_expr(out, expr->left);

  switch (expr->kind) {
  case value_expr_t::CONSTANT_T:
    write_binary_number(out, expr->constant_t);
    break;
  case value_expr_t::CONSTANT_I:
    write_binary_long(out, expr->constant_i);
    break;
  case value_expr_t::CONSTANT_A:
    write_binary_amount(out, *(expr->constant_a));
    break;
  case value_expr_t::CONSTANT_V:
    assert(0);
    break;

  case value_expr_t::F_CODE_MASK:
  case value_expr_t::F_PAYEE_MASK:
  case value_expr_t::F_NOTE_MASK:
  case value_expr_t::F_ACCOUNT_MASK:
  case value_expr_t::F_SHORT_ACCOUNT_MASK:
  case value_expr_t::F_COMMODITY_MASK:
    if (expr->mask) {
      write_binary_number<char>(out, 1);
      write_binary_mask(out, expr->mask);
    } else {
      write_binary_number<char>(out, 0);
    }
    break;

  default:
    if (expr->kind > value_expr_t::TERMINALS)
      write_binary_value_expr(out, expr->right);
    break;
  }

}

void write_binary_transaction(std::ostream& out, transaction_t * xact,
			      bool ignore_calculated)
{
  write_binary_long(out, xact->_date);
  write_binary_long(out, xact->_date_eff);
  write_binary_long(out, xact->account->ident);

  if (xact->amount_expr) {
    write_binary_number<char>(out, 1);
    write_binary_value_expr(out, xact->amount_expr);
  } else {
    write_binary_number<char>(out, 0);
    if (ignore_calculated && xact->flags & TRANSACTION_CALCULATED)
      write_binary_amount(out, amount_t());
    else
      write_binary_amount(out, xact->amount);
  }

  if (xact->cost &&
      (! (ignore_calculated && xact->flags & TRANSACTION_CALCULATED))) {
    write_binary_number<char>(out, 1);
    if (xact->cost_expr) {
      write_binary_number<char>(out, 1);
      write_binary_value_expr(out, xact->cost_expr);
    } else {
      write_binary_number<char>(out, 0);
      write_binary_amount(out, *xact->cost);
    }
  } else {
    write_binary_number<char>(out, 0);
  }

  write_binary_number(out, xact->state);
  write_binary_number(out, xact->flags);
  write_binary_string(out, xact->note);

  write_binary_long(out, xact->beg_pos);
  write_binary_long(out, xact->beg_line);
  write_binary_long(out, xact->end_pos);
  write_binary_long(out, xact->end_line);
}

void write_binary_entry_base(std::ostream& out, entry_base_t * entry)
{
  write_binary_long(out, entry->src_idx);
  write_binary_long(out, entry->beg_pos);
  write_binary_long(out, entry->beg_line);
  write_binary_long(out, entry->end_pos);
  write_binary_long(out, entry->end_line);

  bool ignore_calculated = false;
  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    if ((*i)->amount_expr || (*i)->cost_expr) {
      ignore_calculated = true;
      break;
    }

  write_binary_number<char>(out, ignore_calculated ? 1 : 0);

  write_binary_long(out, entry->transactions.size());
  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    write_binary_transaction(out, *i, ignore_calculated);
}

void write_binary_entry(std::ostream& out, entry_t * entry)
{
  write_binary_entry_base(out, entry);
  write_binary_long(out, entry->_date);
  write_binary_long(out, entry->_date_eff);
  write_binary_string(out, entry->code);
  write_binary_string(out, entry->payee);
}

void write_binary_auto_entry(std::ostream& out, auto_entry_t * entry)
{
  write_binary_entry_base(out, entry);
  write_binary_value_expr(out, entry->predicate->predicate);
}

void write_binary_period_entry(std::ostream& out, period_entry_t * entry)
{
  write_binary_entry_base(out, entry);
  write_binary_string(out, entry->period_string);
}

void write_binary_commodity_base(std::ostream& out, commodity_base_t * commodity)
{
  commodity->ident = ++base_commodity_index;

  write_binary_string(out, commodity->symbol);
  write_binary_string(out, commodity->name);
  write_binary_string(out, commodity->note);
  write_binary_number(out, commodity->precision);
  write_binary_number(out, commodity->flags);
}

void write_binary_commodity_base_extra(std::ostream& out, commodity_base_t * commodity)
{
  if (! commodity->history) {
    write_binary_long<unsigned long>(out, 0);
  } else {
    write_binary_long<unsigned long>(out, commodity->history->prices.size());
    for (history_map::const_iterator i = commodity->history->prices.begin();
	 i != commodity->history->prices.end();
	 i++) {
      write_binary_long(out, (*i).first);
      write_binary_amount(out, (*i).second);
    }
    write_binary_long(out, commodity->history->last_lookup);
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

void write_binary_commodity(std::ostream& out, commodity_t * commodity)
{
  commodity->ident = ++commodity_index;

  write_binary_long(out, commodity->ptr->ident);
  write_binary_string(out, commodity->qualified_symbol);
}

void write_binary_commodity_annotated(std::ostream& out,
				      commodity_t * commodity)
{
  commodity->ident = ++commodity_index;

  write_binary_long(out, commodity->ptr->ident);
  write_binary_string(out, commodity->qualified_symbol);

  annotated_commodity_t * ann_comm =
    static_cast<annotated_commodity_t *>(commodity);

  write_binary_amount(out, ann_comm->price);
  write_binary_long(out, ann_comm->date);
  write_binary_string(out, ann_comm->tag);
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

  if (account->parent)
    write_binary_long(out, account->parent->ident);
  else
    write_binary_long<account_t::ident_t>(out, 0xffffffff);

  write_binary_string(out, account->name);
  write_binary_string(out, account->note);
  write_binary_number(out, account->depth);

  write_binary_long<account_t::ident_t>(out, account->accounts.size());
  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    write_binary_account(out, (*i).second);
}

void write_binary_journal(std::ostream& out, journal_t * journal)
{
  account_index	       = 
  base_commodity_index = 
  commodity_index      = 0;

  write_binary_number(out, binary_magic_number);
  write_binary_number(out, format_version);

  // Write out the files that participated in this journal, so that
  // they can be checked for changes on reading.

  if (journal->sources.size() == 0) {
    write_binary_number<unsigned short>(out, 0);
  } else {
    write_binary_number<unsigned short>(out, journal->sources.size());
    for (strings_list::const_iterator i = journal->sources.begin();
	 i != journal->sources.end();
	 i++) {
      write_binary_string(out, *i);
      struct stat info;
      stat((*i).c_str(), &info);
      write_binary_long(out, std::time_t(info.st_mtime));
    }

    // Write out the price database that relates to this data file, so
    // that if it ever changes the cache can be invalidated.
    write_binary_string(out, journal->price_db);
  }

  ostream_pos_type data_val = out.tellp();
  write_binary_number<unsigned long>(out, 0);

  // Write out the accounts

  write_binary_long<account_t::ident_t>(out, count_accounts(journal->master));
  write_binary_account(out, journal->master);

  if (journal->basket) {
    write_binary_number<bool>(out, true);
    write_binary_long(out, journal->basket->ident);
  } else {
    write_binary_number<bool>(out, false);
  }

  // Write out the number of entries, transactions, and amounts

  write_binary_long<unsigned long>(out, journal->entries.size());
  write_binary_long<unsigned long>(out, journal->auto_entries.size());
  write_binary_long<unsigned long>(out, journal->period_entries.size());

  ostream_pos_type xacts_val = out.tellp();

  write_binary_number<unsigned long>(out, 0);
  ostream_pos_type bigints_val = out.tellp();
  write_binary_number<unsigned long>(out, 0);
  bigints_count = 0;

  // Write out the commodities

  write_binary_long<commodity_t::ident_t>
    (out, commodity_base_t::commodities.size());

  for (base_commodities_map::const_iterator i =
	 commodity_base_t::commodities.begin();
       i != commodity_base_t::commodities.end();
       i++)
    write_binary_commodity_base(out, (*i).second);

  for (base_commodities_map::const_iterator i =
	 commodity_base_t::commodities.begin();
       i != commodity_base_t::commodities.end();
       i++)
    write_binary_commodity_base_extra(out, (*i).second);

  write_binary_long<commodity_t::ident_t>
    (out, commodity_t::commodities.size());

  for (commodities_map::const_iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++) {
    write_binary_number<char>(out, (*i).second->annotated ? 1 : 0);
    if (! (*i).second->annotated) {
      write_binary_commodity(out, (*i).second);
    } else {
      write_binary_string(out, (*i).first); // the mapping key
      write_binary_commodity_annotated(out, (*i).second);
    }
  }

  if (commodity_t::default_commodity)
    write_binary_long(out, commodity_t::default_commodity->ident);
  else
    write_binary_long<commodity_t::ident_t>(out, 0xffffffff);

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
