#ifdef USE_PCH
#include "pch.h"
#else
#include "dump.h"
#include "repitem.h"
#endif

namespace ledger {

void dump_command::operator()(value_t& result, valexpr_t::scope_t * locals)
{
  repitem_t * items = static_cast<repitem_t *>(locals->args[0].to_pointer());
  assert(items);

  items->print_tree(std::cout);
}

} // namespace ledger
