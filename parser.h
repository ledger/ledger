#ifndef _PARSER_H
#define _PARSER_H

#include "utils.h"

namespace ledger {

class account_t;
class journal_t;
class session_t;

class parser_t
{
 public:
  virtual ~parser_t() {}

  virtual bool test(std::istream& in) const = 0;

  virtual unsigned int parse(std::istream& in,
			     session_t&    session,
			     journal_t&	   journal,
			     account_t *   master        = NULL,
			     const path *  original_file = NULL) = 0;
};

unsigned int parse_journal(std::istream& in,
			   session_t&    session,
			   journal_t&	 journal,
			   account_t *	 master        = NULL,
			   const path *	 original_file = NULL);

unsigned int parse_journal_file(const path&  path,
				session_t&   session,
				journal_t&   journal,
				account_t *  master        = NULL,
				const path * original_file = NULL);

unsigned int parse_ledger_data(session_t& session,
			       journal_t& journal,
			       parser_t * cache_parser = NULL,
			       parser_t * xml_parser   = NULL,
			       parser_t * stdin_parser = NULL);

class parse_error : public error {
 public:
  parse_error(const string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~parse_error() throw() {}
};

/************************************************************************
 *
 * General utility parsing functions
 */

inline char * skip_ws(char * ptr) {
  while (*ptr == ' ' || *ptr == '\t' || *ptr == '\n')
    ptr++;
  return ptr;
}

inline char * next_element(char * buf, bool variable = false) {
  for (char * p = buf; *p; p++) {
    if (! (*p == ' ' || *p == '\t'))
      continue;

    if (! variable) {
      *p = '\0';
      return skip_ws(p + 1);
    }
    else if (*p == '\t') {
      *p = '\0';
      return skip_ws(p + 1);
    }
    else if (*(p + 1) == ' ') {
      *p = '\0';
      return skip_ws(p + 2);
    }
  }
  return NULL;
}

inline char peek_next_nonws(std::istream& in) {
  char c = in.peek();
  while (! in.eof() && std::isspace(c)) {
    in.get(c);
    c = in.peek();
  }
  return c;
}

#define READ_INTO(str, targ, size, var, cond) {				\
  char * _p = targ;							\
  var = str.peek();							\
  while (! str.eof() && var != '\n' && (cond) && _p - targ < size) {	\
    str.get(var);							\
    if (str.eof())							\
      break;								\
    if (var == '\\') {							\
      str.get(var);							\
      if (in.eof())							\
	break;								\
    }									\
    *_p++ = var;							\
    var = str.peek();							\
  }									\
  *_p = '\0';								\
}

#define READ_INTO_(str, targ, size, var, idx, cond) {			\
  char * _p = targ;							\
  var = str.peek();							\
  while (! str.eof() && var != '\n' && (cond) && _p - targ < size) {	\
    str.get(var);							\
    if (str.eof())							\
      break;								\
    idx++;								\
    if (var == '\\') {							\
      str.get(var);							\
      if (in.eof())							\
	break;								\
      idx++;								\
    }									\
    *_p++ = var;							\
    var = str.peek();							\
  }									\
  *_p = '\0';								\
}

} // namespace ledger

#endif // _PARSER_H
