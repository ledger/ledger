#include "error.h"

namespace ledger {

const char* parse_error::what() const throw()
{
  std::ostringstream msg;
  msg << file << ", line " << line << ": " << error::what();
  return msg.str().c_str();
}

} // namespace ledger
