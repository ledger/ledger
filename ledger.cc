#include "ledger.h"
#include "textual.h"
#include "binary.h"

#include <fstream>

namespace ledger {

const std::string version = "2.0b";

ledger_t::~ledger_t()
{
  delete master;

  // Don't bother unhooking each entry's transactions from the
  // accounts they refer to, because all accounts are about to
  // be deleted.
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    delete *i;
}

bool ledger_t::add_entry(entry_t * entry)
{
  entries.push_back(entry);

  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++) {
    (*i)->account->add_transaction(*i);

    if ((*i)->amount != (*i)->cost) {
      assert((*i)->amount.commodity);
      (*i)->amount.commodity->add_price(entry->date, (*i)->cost / (*i)->amount);
    }
  }

  return true;
}

bool ledger_t::remove_entry(entry_t * entry)
{
  entries.remove(entry);

  for (transactions_list::const_iterator i
	 = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    (*i)->account->remove_transaction(*i);

  return true;
}

int parse_ledger_file(char * p, ledger_t * journal)
{
  char * sep = std::strrchr(p, '=');
  if (sep) *sep++ = '\0';

  std::ifstream stream(p);

  account_t * master;
  if (sep)
    master = journal->find_account(sep);
  else
    master = journal->master;

  journal->sources.push_back(p);

  unsigned long magic;
  std::istream::pos_type start = stream.tellg();
  stream.read((char *)&magic, sizeof(magic));
  stream.seekg(start);

  if (magic == magic_number)
    return read_binary_ledger(stream, "", journal, master);
  else
    return parse_textual_ledger(stream, journal, master);
}

} // namespace ledger
