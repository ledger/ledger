#ifndef _BINARY_H
#define _BINARY_H

#include "ledger.h"

namespace ledger {

extern unsigned long magic_number;

extern unsigned int read_binary_ledger(std::istream&	   in,
				       const std::string&  leader,
				       ledger_t *&         book,
				       account_t *         master = NULL);

extern void	  write_binary_ledger(std::ostream&	 out,
				      ledger_t *	 ledger,
				      const std::string& leader);

} // namespace ledger

#endif // _BINARY_H
