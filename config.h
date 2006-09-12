#ifndef _CONFIG_H
#define _CONFIG_H

#include "option.h"

namespace ledger {

void help(std::ostream& out);

#ifdef USE_BOOST_PYTHON
#define OPTIONS_SIZE 100
#else
#define OPTIONS_SIZE 98
#endif
extern static_option_t static_options[OPTIONS_SIZE];


#define DEF_OPT(tag, option)					\
  struct option_ ## tag : public option_t {			\
    option_ ## tag() : option_t(option) {}

#define DEF_OPTS(tag, option, short_opt)			\
  struct option_ ## tag : public option_t {			\
    option_ ## tag() : option_t(option, short_opt) {}

#define DEF_OPT_(tag, option)					\
  struct option_ ## tag : public option_t {			\
    option_ ## tag() : option_t(option, true) {}

#define DEF_OPTS_(tag, option, short_opt)			\
  struct option_ ## tag : public option_t {			\
    option_ ## tag() : option_t(option, short_opt, true) {}

#define DEFR_OPT(tag, option)					\
  struct option_ ## tag : public option_t {			\
    option_ ## tag() : option_t(option) {}			\
								\
    virtual void select(report_t * report, const char * optarg) {

#define DEFR_OPTS(tag, option, short_opt)			\
  struct option_ ## tag : public option_t {			\
    option_ ## tag() : option_t(option, short_opt) {}		\
								\
    virtual void select(report_t * report, const char * optarg) {

#define DEFR_OPT_(tag, option)					\
  struct option_ ## tag : public option_t {			\
    option_ ## tag() : option_t(option, true) {}		\
								\
    virtual void select(report_t * report, const char * optarg) {

#define DEFR_OPTS_(tag, option, short_opt)			\
  struct option_ ## tag : public option_t {			\
    option_ ## tag() : option_t(option, short_opt, true) {}	\
								\
    virtual void select(report_t * report, const char * optarg) {

#define END_DEF() };
#define END_DEFR() } };

} // namespace ledger

#endif // _CONFIG_H
