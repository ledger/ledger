#ifndef _PARSER_H
#define _PARSER_H

#include "utils.h"

namespace ledger {

class account_t;
class journal_t;

class parser_t
{
 public:
  virtual ~parser_t() {}

  virtual bool test(std::istream& in) const = 0;

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const string * original_file = NULL) = 0;
};

DECLARE_EXCEPTION(parse_error);

/************************************************************************
 *
 * General utility parsing functions
 */

inline char * skip_ws(char * ptr) {
  while (*ptr == ' ' || *ptr == '\t' || *ptr == '\n')
    ptr++;
  return ptr;
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
