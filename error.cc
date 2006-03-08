#include "error.h"
#include "value.h"
#include "valexpr.h"
#include "format.h"
#include "journal.h"

value_context::value_context(const ledger::value_t& _bal,
			     const std::string& desc) throw()
  : bal(new ledger::value_t(_bal)), error_context(desc) {}

value_context::~value_context() throw()
{
  delete bal;
}

void value_context::describe(std::ostream& out) const throw()
{
  if (! desc.empty())
    out << desc << std::endl;

  ledger::balance_t * ptr = NULL;

  out << std::right;
  out.width(20);

  switch (bal->type) {
  case ledger::value_t::BOOLEAN:
    out << (*((bool *) bal->data) ? "true" : "false");
    break;
  case ledger::value_t::INTEGER:
    out << *((long *) bal->data);
    break;
  case ledger::value_t::DATETIME:
    out << *((datetime_t *) bal->data);
    break;
  case ledger::value_t::AMOUNT:
    out << *((ledger::amount_t *) bal->data);
    break;
  case ledger::value_t::BALANCE:
    ptr = (ledger::balance_t *) bal->data;
    // fall through...

  case ledger::value_t::BALANCE_PAIR:
    if (! ptr)
      ptr = &((ledger::balance_pair_t *) bal->data)->quantity;

    ptr->write(out, 20);
    break;
  default:
    assert(0);
    break;
  }
  out << std::endl;
}

valexpr_context::valexpr_context(const ledger::value_expr_t * _expr,
				 const std::string& desc) throw()
  : expr(NULL), error_node(_expr), error_context(desc)
{
  error_node->acquire();
}

valexpr_context::~valexpr_context() throw()
{
  if (expr) expr->release();
  if (error_node) error_node->release();
}

void valexpr_context::describe(std::ostream& out) const throw()
{
  if (! expr) {
    out << "Valexpr_context expr not set!" << std::endl;
    return;
  }

  if (! desc.empty())
    out << desc << std::endl;

  out << "  ";
  unsigned long start = out.tellp();
  unsigned long pos   = ledger::write_value_expr(out, expr,
						 error_node, start);
  out << std::endl << "  ";
  for (int i = 0; i < pos - start; i++)
    out << " ";
  out << "^" << std::endl;
}

void line_context::describe(std::ostream& out) const throw()
{
  if (! desc.empty())
    out << desc << std::endl;

  out << "  " << line << std::endl << "  ";
  long idx = pos < 0 ? line.length() - 1 : pos;
  for (int i = 0; i < idx; i++)
    out << " ";
  out << "^" << std::endl;
}

void entry_context::describe(std::ostream& out) const throw()
{
  if (! desc.empty())
    out << desc << std::endl;

  ledger::print_entry(out, entry, "  ");
}

xact_context::xact_context(const ledger::transaction_t& _xact,
			   const std::string& desc) throw()
  : xact(_xact), file_context("", 0, desc)
{
  const ledger::strings_list& sources(xact.entry->journal->sources);
  int x = 0;
  for (ledger::strings_list::const_iterator i = sources.begin();
       i != sources.end();
       i++, x++)
    if (x == xact.entry->src_idx) {
      file = *i;
      break;
    }
  line = xact.beg_line;
}
