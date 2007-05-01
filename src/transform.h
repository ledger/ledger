#ifndef _TRANSFORM_H
#define _TRANSFORM_H

#include "xpath.h"

namespace ledger {

class transform_t {
 public:
  virtual ~transform_t() {}
  virtual void execute(xml::document_t * document) = 0;
};

class check_transform : public transform_t {
  // --check checks the validity of the item list.
 public:
  virtual void execute(xml::document_t * document);
};

class accounts_transform : public transform_t {
  // --accounts transforms the report tree into an account-wise view.
 public:
  virtual void execute(xml::document_t * document);
};

class compact_transform : public transform_t {
  // --compact compacts an account tree to remove accounts with only
  // one child account.
 public:
  virtual void execute(xml::document_t * document);
};

class clean_transform : public transform_t {
  // --clean clears out entries and accounts that have no contents.
 public:
  virtual void execute(xml::document_t * document);
};

class entries_transform : public transform_t {
  // --entries transforms the report tree into an entries-wise view.
 public:
  virtual void execute(xml::document_t * document);
};

class optimize_transform : public transform_t {
  // --optimize optimizes entries for display by the print command.
  // What this means is that if an entry has two transactions of the
  // commodity (one the negative of the other), the amount of the
  // second transaction will be nulled out.
 public:
  virtual void execute(xml::document_t * document);
};

class split_transform : public transform_t {
  // --split breaks entry with two or more transactions into what
  // seems like two entries each with one transaction -- even though
  // it is the same entry being reported in both cases.  This is
  // useful before sorting, for exampel, in order to sort by
  // transaction instead of by entry.
 public:
  virtual void execute(xml::document_t * document);
};

class merge_transform : public transform_t {
  // --merge is the opposite of --split: any adjacent transactions
  // which share the same entry will be merged into a group of
  // transactions under one reported entry.
 public:
  virtual void execute(xml::document_t * document);
};

class combine_transform : public transform_t {
  // --combine EXPR combines all transactions matching EXPR so that
  // they appear within the same virtual entry (whose date will span
  // the earliest to the latest of those entries, and whose payee name
  // will show the terminating date or a label that is characteristic
  // of the set).
 public:
  virtual void execute(xml::document_t * document);
};

class group_transform : public transform_t {
  // --group groups all transactions that affect the same account
  // within an entry, so that they appear as a single transaction.
 public:
  virtual void execute(xml::document_t * document);
};

class collapse_transform : public transform_t {
  // --collapse makes all transactions within an entry appear as a
  // single transaction, even if they affect different accounts.  The
  // fictitous account "<total>" is used to represent the final sum,
  // if multiple accounts are involved.
 public:
  virtual void execute(xml::document_t * document);
};

class subtotal_transform : public transform_t {
  // --subtotal will combine the transactions from all entries into
  // one giant entry.  When used in conjunction with --group, the
  // affect is very similar to a regular balance report.
 public:
  virtual void execute(xml::document_t * document);
};

#if 0
class select_transform : public transform_t
{
 protected:
  xml::xpath_t xpath;

 public:
  select_transform(const string& selection_path) {
    xpath.parse(selection_path);
  }
  virtual ~select_transform() {}

  virtual void execute(xml::document_t * document);
};

class remove_transform : public select_transform
{
 public:
  remove_transform(const string& selection_path)
    : select_transform(selection_path) {}

  virtual void execute(xml::document_t * document);
};
#endif

} // namespace ledger

#endif // _TRANSFORM_H
