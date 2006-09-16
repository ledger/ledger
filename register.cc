#ifdef USE_PCH
#include "pch.h"
#else
#include "register.h"
#endif

namespace ledger {

void register_command::operator()(value_t& result, valexpr_t::scope_t * locals)
{
  repitem_t * items = static_cast<repitem_t *>(locals->args[0].get_pointer());
  assert(items);

  items->print_tree(std::cout);
}

} // namespace ledger
