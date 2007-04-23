#include "register.h"
#include "journal.h"

namespace ledger {

static void scan_for_transactions(std::ostream& out, const xml::node_t * node)
{
  if (! (node->flags & XML_NODE_IS_PARENT))
    return;
  
  const xml::parent_node_t * parent =
    static_cast<const xml::parent_node_t *>(node);

  for (const xml::node_t * child = parent->children();
       child;
       child = child->next)
    if (child->name_id == xml::document_t::TRANSACTION) {
      const xml::transaction_node_t * xact_node =
	dynamic_cast<const xml::transaction_node_t *>(child);
      assert(xact_node);

      const transaction_t * xact = xact_node->transaction;
      assert(xact);

      out << xact->entry->date() << ' '
	  << std::setw(21) << std::left
	  << abbreviate(xact->entry->payee, 21) << ' '
	  << std::setw(21) << std::left
	  << abbreviate(xact->account->fullname(), 21,
			ABBREVIATE, true) << ' '
	  << std::setw(12) << std::right
	  << xact->amount << '\n';
    } else {
      scan_for_transactions(out, child);
    }
}

void register_command::print_document(std::ostream&	out,
				      xml::document_t * doc)
{
#if DEBUG_LEVEL >= BETA
  long long old_new_size = new_size;
#endif

#if 1
  scan_for_transactions(out, doc->top);
  out.flush();
#else
  value_t nodelist;
  xml::xpath_t::eval(nodelist, "//transaction", doc);

#if DEBUG_LEVEL >= BETA
  std::cerr << "Memory requested preparing report: "
	    << (new_size - old_new_size) << std::endl;
  old_new_size = new_size;
#endif

  const value_t::sequence_t * xact_list = nodelist.to_sequence();
  assert(xact_list);

  for (value_t::sequence_t::const_iterator i = xact_list->begin();
       i != xact_list->end();
       i++) {
    const xml::node_t * node = (*i).to_xml_node();
    assert(node);

    const xml::transaction_node_t * xact_node =
      dynamic_cast<const xml::transaction_node_t *>(node);
    assert(xact_node);

    const transaction_t * xact = xact_node->transaction;
    assert(xact);

    std::cout << xact->entry->date() << ' '
	      << std::setw(21) << std::left
	      << abbreviate(xact->entry->payee, 21) << ' '
	      << std::setw(21) << std::left
	      << abbreviate(xact->account->fullname(), 21,
			    ABBREVIATE, true) << ' '
	      << std::setw(12) << std::right
	      << xact->amount
	      << std::endl;
  }

#if DEBUG_LEVEL >= BETA
  std::cerr << "Memory requested generating report: "
	    << (new_size - old_new_size) << std::endl;
  old_new_size = new_size;
#endif
#endif
}

} // namespace ledger
