#ifdef USE_PCH
#include "pch.h"
#else
#include "register.h"
#include "report.h"
#include "repitem.h"
#include "format.h"
#endif

namespace ledger {

void register_command::operator()(value_t& result, valexpr_t::scope_t * locals)
{
  std::ostream * out = static_cast<std::ostream *>(locals->args[0].to_pointer());
  assert(out);
  repitem_t * items = static_cast<repitem_t *>(locals->args[1].to_pointer());
  assert(items);

  assert(items->kind == repitem_t::SESSION);

  std::string format_string = report->session->register_format;

  format_t entry_format;
  format_t transaction_format;

  const char * f = format_string.c_str();
  if (const char * p = std::strstr(f, "%/")) {
    entry_format.parse(std::string(f, 0, p - f));
    transaction_format.parse(std::string(p + 2));
  } else {
    entry_format.parse(format_string);
    transaction_format.parse(format_string);
  }

  int column = 0;
  for (repitem_t * journal = items->children; journal; journal = journal->next) {
    for (repitem_t * entry = journal->children; entry; entry = entry->next) {
      column = entry_format.format(*out, entry, column);
      for (repitem_t * xact = entry->contents; xact; xact = xact->next) {
	column = transaction_format.format(*out, xact, column);
      }
    }
  }
}

} // namespace ledger
