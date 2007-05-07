#ifndef _REPORT_H
#define _REPORT_H

#include "session.h"
#include "transform.h"

namespace ledger {

typedef std::list<string> strings_list;

class report_t : public xml::xpath_t::scope_t
{
 public:
  optional<path> output_file;
  string	 format_string;
  string	 amount_expr;
  string	 total_expr;
  string	 date_output_format;

  unsigned long budget_flags;

  string account;
  optional<path> pager;

  bool show_totals;
  bool raw_mode;

  session_t *   session;
  transform_t * last_transform;

  ptr_list<transform_t> transforms;

  report_t(session_t * _session)
    : xml::xpath_t::scope_t(_session),
      show_totals(false),
      raw_mode(false),
      session(_session),
      last_transform(NULL)
  {
    TRACE_CTOR(report_t, "session_t *");
    eval("t=total,TOT=0,T()=(TOT=TOT+t,TOT)");
  }

  virtual ~report_t();

  void apply_transforms(xml::document_t * document);

  //
  // Utility functions for value expressions
  //

  void ftime(value_t& result, xml::xpath_t::scope_t * locals);
  void abbrev(value_t& result, xml::xpath_t::scope_t * locals);

  //
  // Config options
  //

  void eval(const string& expr) {
    xml::xpath_t(expr).compile((xml::document_t *)NULL, this);
  }
  void option_eval(value_t&, xml::xpath_t::scope_t * locals) {
    eval(locals->args[0].to_string());
  }

  void option_amount(value_t&, xml::xpath_t::scope_t * locals) {
    eval(string("t=") + locals->args[0].to_string());
  }
  void option_total(value_t&, xml::xpath_t::scope_t * locals) {
    eval(string("T()=") + locals->args[0].to_string());
  }

  void option_format(value_t&, xml::xpath_t::scope_t * locals) {
    format_string = locals->args[0].to_string();
  }

  void option_raw(value_t&) {
    raw_mode = true;
  }

  void option_foo(value_t&) {
    std::cout << "This is foo" << std::endl;
  }
  void option_bar(value_t&, xml::xpath_t::scope_t * locals) {
    std::cout << "This is bar: " << locals->args[0] << std::endl;
  }

  //
  // Transform options
  //

#if 0
  void option_select(value_t&, xml::xpath_t::scope_t * locals) {
    transforms.push_back(new select_transform(locals->args[0].to_string()));
  }
  void option_limit(value_t&, xml::xpath_t::scope_t * locals) {
    string expr = (string("//xact[") +
			locals->args[0].to_string() + "]");
    transforms.push_back(new select_transform(expr));
  }

  void option_remove(value_t&, xml::xpath_t::scope_t * locals) {
    transforms.push_back(new remove_transform(locals->args[0].to_string()));
  }

  void option_accounts(value_t&) {
    transforms.push_back(new accounts_transform);
  }
  void option_compact(value_t&) {
    transforms.push_back(new compact_transform);
  }
  void option_clean(value_t&) {
    transforms.push_back(new clean_transform);
  }
  void option_entries(value_t&) {
    transforms.push_back(new entries_transform);
  }

  void option_split(value_t&) {
    transforms.push_back(new split_transform);
  }
  void option_merge(value_t&) {
    transforms.push_back(new merge_transform);
  }
#endif

  //
  // Scope members
  //

  virtual bool resolve(const string& name, value_t& result,
		       xml::xpath_t::scope_t * locals);
  virtual xml::xpath_t::op_t * lookup(const string& name);
};

string abbrev(const string& str, unsigned int width,
		   const bool is_account);

} // namespace ledger

#endif // _REPORT_H
