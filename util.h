#ifndef _UTIL_H
#define _UTIL_H

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

#define READ_INTO(str, targ, size, var, cond) {	\
  char * _p = targ;				\
  var = str.peek();				\
  while (! str.eof() && (cond)) {		\
    str.get(var);				\
    if (str.eof())				\
      break;					\
    if (var == '\\') {				\
      str.get(var);				\
      if (in.eof())				\
	break;					\
    }						\
    *_p++ = var;				\
    var = str.peek();				\
  }						\
  assert(_p < targ + ((size) - 1));		\
  *_p = '\0';					\
}

#endif // _UTIL_H
