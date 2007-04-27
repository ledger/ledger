#ifndef _UTIL_H
#define _UTIL_H

namespace ledger {

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

ledger::string resolve_path(const ledger::string& path);

#ifdef HAVE_REALPATH
extern "C" char *realpath(const char *, char resolved_path[]);
#endif

enum elision_style_t {
  TRUNCATE_TRAILING,
  TRUNCATE_MIDDLE,
  TRUNCATE_LEADING,
  ABBREVIATE
};

ledger::string abbreviate(const ledger::string& str, unsigned int width,
			  elision_style_t elision_style = TRUNCATE_TRAILING,
			  const bool is_account = false, int abbrev_length = 2);

static inline const
ledger::string& either_or(const ledger::string& first,
			  const ledger::string& second)
{
  if (first.empty())
    return second;
  else
    return first;
}

} // namespace ledger

#endif // _UTIL_H
