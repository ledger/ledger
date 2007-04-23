#ifndef _GNUCASH_H
#define _GNUCASH_H

#include "parser.h"
#include "journal.h"

namespace ledger {

struct gnucash_parser_t : public parser_t
{
  typedef std::map<const string, account_t *>  accounts_map;
  typedef std::pair<const string, account_t *> accounts_pair;

  typedef std::map<account_t *, commodity_t *>  account_comm_map;
  typedef std::pair<account_t *, commodity_t *> account_comm_pair;

  journal_t *	   curr_journal;
  account_t *	   master_account;
  account_t *	   curr_account;
  string	   curr_account_id;
  entry_t *	   curr_entry;
  commodity_t *	   entry_comm;
  commodity_t *	   curr_comm;
  amount_t	   curr_value;
  amount_t	   curr_quant;
  XML_Parser	   expat_parser;
  accounts_map	   accounts_by_id;
  account_comm_map account_comms;
  unsigned int	   count;
  string	   have_error;

  std::istream *   instreamp;
  unsigned int     offset;
  XML_Parser       parser;
  string      path;
  unsigned int     src_idx;
  istream_pos_type beg_pos;
  unsigned long    beg_line;

  transaction_t::state_t curr_state;

  enum action_t {
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

 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const string * original_file = NULL);

  amount_t convert_number(const string& number, int * precision = NULL);
};

} // namespace ledger

#endif // _GNUCASH_H
