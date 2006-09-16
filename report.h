#ifndef _REPORT_H
#define _REPORT_H

#include "session.h"
#include "valexpr.h"

#include <string>
#include <list>

namespace ledger {

class transform_t;
class repitem_t;

typedef std::list<std::string> strings_list;

class report_t : public valexpr_t::scope_t
{
 public:
  std::string output_file;
  std::string format_string;
  std::string amount_expr;
  std::string total_expr;
  std::string date_output_format;

  unsigned long budget_flags;

  std::string account;
  std::string pager;

  bool show_totals;
  bool keep_price;
  bool keep_date;
  bool keep_tag;

  session_t *   session;
  transform_t * last_transform;

  std::list<transform_t *> transforms;

  report_t(session_t * _session)
    : valexpr_t::scope_t(_session),

      show_totals(false),
      keep_price(false),
      keep_date(false),
      keep_tag(false),

      session(_session),

      last_transform(NULL) {}

  virtual ~report_t();

  void apply_transforms(repitem_t * items);

  //
  // Config options
  //

  void option_foo(value_t& result) {
    std::cout << "This is foo" << std::endl;
  }
  void option_bar(value_t& result, valexpr_t::scope_t * locals) {
    std::cout << "This is bar: " << locals->args[0] << std::endl;
  }

  //
  // Scope members
  //

  virtual valexpr_t::node_t * lookup(const std::string& name);
};

} // namespace ledger

#endif // _REPORT_H
