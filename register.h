#ifndef _REGISTER_H
#define _REGISTER_H

#include "xpath.h"

namespace ledger {

class register_command : public xml::xpath_t::functor_t
{
 public:
  register_command() : xml::xpath_t::functor_t("register") {}

  virtual void operator()(value_t&, xml::xpath_t::scope_t * locals) {
    std::ostream *    out = get_ptr<std::ostream>(locals, 0);
    xml::document_t * doc = get_ptr<xml::document_t>(locals, 1);

    print_document(*out, doc);
  }

  virtual void print_document(std::ostream& out, xml::document_t * doc);
};

} // namespace ledger

#endif // _REGISTER_H
