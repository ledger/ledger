#include "register.h"
#include "journal.h"

namespace ledger {

void register_command::print_document(std::ostream&	out,
				      xml::document_t * doc)
{
  value_t nodelist = xml::xpath_t::eval("//transaction", doc);

  value_t::sequence_t * xact_list = nodelist.to_sequence();
  assert(xact_list);

  for (value_t::sequence_t::iterator i = xact_list->begin();
       i != xact_list->end();
       i++) {
    xml::node_t * node = (*i).to_xml_node();
    assert(node);

    xml::transaction_node_t * xact_node =
      dynamic_cast<xml::transaction_node_t *>(node);
    assert(xact_node);

    transaction_t * xact = xact_node->transaction;
    assert(xact);

    std::cout << xact->account->fullname() << std::endl;
  }
}

} // namespace ledger
