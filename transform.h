#ifndef _TRANSFORM_H
#define _TRANSFORM_H

#include <list>

namespace ledger {

class repitem_t;

class transform_t {
 public:
  virtual void walk_items(repitem_t * items) = 0;
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
  virtual void walk_items(repitem_t * items);
};

class check_transform : public transform_t {
  // --check checks the validity of the item list.
 public:
  virtual void walk_items(repitem_t * items);
};

class merge_transform : public transform_t {
  // --merge is the opposite of --split: any adjacent transactions
  // which share the same entry will be merged into a group of
  // transactions under one reported entry.
 public:
  virtual void walk_items(repitem_t * items);
};

class combine_transform : public transform_t {
  // --combine EXPR combines all transactions matching EXPR so that
  // they appear within the same virtual entry (whose date will span
  // the earliest to the latest of those entries, and whose payee name
  // will show the terminating date or a label that is characteristic
  // of the set).
 public:
  virtual void walk_items(repitem_t * items);
};

class group_transform : public transform_t {
  // --group groups all transactions that affect the same account
  // within an entry, so that they appear as a single transaction.
 public:
  virtual void walk_items(repitem_t * items);
};

class collapse_transform : public transform_t {
  // --collapse makes all transactions within an entry appear as a
  // single transaction, even if they affect different accounts.  The
  // fictitous account "<total>" is used to represent the final sum,
  // if multiple accounts are involved.
 public:
  virtual void walk_items(repitem_t * items);
};

class subtotal_transform : public transform_t {
  // --subtotal will combine the transactions from all entries into
  // one giant entry.  When used in conjunction with --group, the
  // affect is very similar to a regular balance report.
 public:
  virtual void walk_items(repitem_t * items);
};

#if 0
template <typename T, typename U, typename V = int, typename W = int>
struct pystream_handler_wrap : public ledger::item_handler<U>
{
  PyFileObject * file;
  pyofstream *	 output;

  T handler;

  pystream_handler_wrap(PyObject * file_)
    : file((PyFileObject *)file_), output(new pyofstream(file)),
      handler(*output) {}
  pystream_handler_wrap(PyObject * file_, const V& arg)
    : file((PyFileObject *)file_), output(new pyofstream(file)),
      handler(*output, arg) {}
  pystream_handler_wrap(PyObject * file_, const V& arg1, const W& arg2)
    : file((PyFileObject *)file_), output(new pyofstream(file)),
      handler(*output, arg1, arg2) {}

  virtual ~pystream_handler_wrap() {
    delete output;
  }

  virtual void flush() {
    handler.flush();
  }
  virtual void operator()(U& item) {
    handler.operator()(item);
  }
};
#endif

} // namespace ledger

#endif // _TRANSFORM_H
