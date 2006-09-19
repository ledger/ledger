#ifndef _TRANSFORM_H
#define _TRANSFORM_H

#include "valexpr.h"
#include "repitem.h"

#include <list>
#include <deque>

namespace ledger {

class transform_t {
 public:
  virtual void execute(repitem_t * items) = 0;
};

typedef std::list<transform_t *> transform_queue_list;

extern transform_queue_list pre_transform_queue;
extern transform_queue_list transform_queue;
extern transform_queue_list post_transform_queue;

void apply_transform_queue(repitem_t * items);

class split_transform : public transform_t {
  // --split breaks entry with two or more transactions into what
  // seems like two entries each with one transaction -- even though
  // it is the same entry being reported in both cases.  This is
  // useful before sorting, for exampel, in order to sort by
  // transaction instead of by entry.
 public:
  virtual void execute(repitem_t * items);
};

class check_transform : public transform_t {
  // --check checks the validity of the item list.
 public:
  virtual void execute(repitem_t * items);
};

class merge_transform : public transform_t {
  // --merge is the opposite of --split: any adjacent transactions
  // which share the same entry will be merged into a group of
  // transactions under one reported entry.
 public:
  virtual void execute(repitem_t * items);
};

class combine_transform : public transform_t {
  // --combine EXPR combines all transactions matching EXPR so that
  // they appear within the same virtual entry (whose date will span
  // the earliest to the latest of those entries, and whose payee name
  // will show the terminating date or a label that is characteristic
  // of the set).
 public:
  virtual void execute(repitem_t * items);
};

class group_transform : public transform_t {
  // --group groups all transactions that affect the same account
  // within an entry, so that they appear as a single transaction.
 public:
  virtual void execute(repitem_t * items);
};

class collapse_transform : public transform_t {
  // --collapse makes all transactions within an entry appear as a
  // single transaction, even if they affect different accounts.  The
  // fictitous account "<total>" is used to represent the final sum,
  // if multiple accounts are involved.
 public:
  virtual void execute(repitem_t * items);
};

class subtotal_transform : public transform_t {
  // --subtotal will combine the transactions from all entries into
  // one giant entry.  When used in conjunction with --group, the
  // affect is very similar to a regular balance report.
 public:
  virtual void execute(repitem_t * items);
};

class select_transform : public transform_t
{
  const repitem_t::path_t * path;

 public:
  select_transform(const std::string& selection_path) {
    path = repitem_t::parse_selector(selection_path);
  }
  virtual ~select_transform() {
    if (path)
      delete path;
  }

  virtual void execute(repitem_t * items);
};

} // namespace ledger

#endif // _TRANSFORM_H
