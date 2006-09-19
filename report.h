#ifndef _REPORT_H
#define _REPORT_H

#include "session.h"
#include "valexpr.h"
#include "repitem.h"
#include "transform.h"

#include <string>
#include <list>

namespace ledger {

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

  session_t *   session;
  transform_t * last_transform;

  std::list<transform_t *> transforms;

  report_t(session_t * _session)
    : valexpr_t::scope_t(_session),
      show_totals(false),
      session(_session),
      last_transform(NULL)
  {
    eval("t=total,TOT=0,T()=(TOT=TOT+t,TOT)");
  }

  virtual ~report_t();

  void apply_transforms(repitem_t * items);

  //
  // Utility functions for value expressions
  //

  void ftime(value_t& result, valexpr_t::scope_t * locals);
  void abbrev(value_t& result, valexpr_t::scope_t * locals);

  //
  // Config options
  //

  void eval(const std::string& expr) {
    valexpr_t(expr).compile(this);
  }
  void option_eval(value_t&, valexpr_t::scope_t * locals) {
    eval(locals->args[0].to_string());
  }

  void option_amount(value_t&, valexpr_t::scope_t * locals) {
    eval(std::string("t=") + locals->args[0].to_string());
  }
  void option_total(value_t&, valexpr_t::scope_t * locals) {
    eval(std::string("T()=") + locals->args[0].to_string());
  }

  void option_format(value_t&, valexpr_t::scope_t * locals) {
    format_string = locals->args[0].to_string();
  }

  void option_foo(value_t&) {
    std::cout << "This is foo" << std::endl;
  }
  void option_bar(value_t&, valexpr_t::scope_t * locals) {
    std::cout << "This is bar: " << locals->args[0] << std::endl;
  }

  //
  // Transform options
  //

  void option_select(value_t&, valexpr_t::scope_t * locals) {
    transforms.push_back(new select_transform(locals->args[0].to_string()));
  }
  void option_limit(value_t&, valexpr_t::scope_t * locals) {
    std::string expr = (std::string("//xact[") +
			locals->args[0].to_string() + "]");
    transforms.push_back(new select_transform(expr));
  }

  void option_remove(value_t&, valexpr_t::scope_t * locals) {
    transforms.push_back(new remove_transform(locals->args[0].to_string()));
  }

  void option_split(value_t&, valexpr_t::scope_t * locals) {
    transforms.push_back(new split_transform);
  }

  //
  // Scope members
  //

  virtual bool resolve(const std::string& name, value_t& result,
		       valexpr_t::scope_t * locals);
  virtual valexpr_t::node_t * lookup(const std::string& name);
};

std::string abbrev(const std::string& str, unsigned int width,
		   const bool is_account);

} // namespace ledger

#endif // _REPORT_H
