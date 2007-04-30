#ifndef _CONTEXT_H
#define _CONTEXT_H

namespace ledger {

class context
{
public:
  string description;	   // ex: 'While parsing file "%R" at line %L'

  explicit context(const string& _description) throw()
    : description(_description) {}

  virtual ~context() {}
};

class file_context : public context
{
public:
  path pathname;		// ex: ledger.dat

  uint_least32_t linenum_beg;	// ex: 1010
  uint_least32_t linenum_end;	// ex: 1010
  uint_least32_t position_beg;
  uint_least32_t position_end;

  optional<uint_least32_t> colnum_beg;	// ex: 8
  optional<uint_least32_t> colnum_end;	// ex: 8

  explicit file_context(const path&          _pathname,
			const uint_least32_t _linenum_beg,
			const uint_least32_t _linenum_end,
			const uint_least32_t _position_beg,
			const uint_least32_t _position_end) throw()
    : context(""),
      pathname(_pathname),
      linenum_beg(_linenum_beg),
      linenum_end(_linenum_end),
      position_beg(_position_beg),
      position_end(_position_end) {}
};

class string_context : public context
{
public:
  string text;			// ex: (The multi-line text of an entry)

  optional<uint_least32_t> linenum_beg_off; // ex: 2
  optional<uint_least32_t> linenum_end_off; // ex: 2
  optional<uint_least32_t> colnum_beg_off; // ex: 8
  optional<uint_least32_t> colnum_end_off; // ex: 8
};

#if 0

class file_context : public error_context
{
 protected:
  string   file;
  unsigned long line;
 public:
  file_context(const string& _file, unsigned long _line,
	       const string& _desc = "") throw()
    : error_context(_desc), file(_file), line(_line) {}
  virtual ~file_context() throw() {}

  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << " ";

    out << "\"" << file << "\", line " << line << ": ";
  }
};

class line_context : public error_context {
 public:
  string line;
  long	      pos;

  line_context(const string& _line, long _pos,
	       const string& _desc = "") throw()
    : error_context(_desc), line(_line), pos(_pos) {}
  virtual ~line_context() throw() {}

  virtual void describe(std::ostream& out) const throw() {
    if (! desc.empty())
      out << desc << std::endl;

    out << "  " << line << std::endl << "  ";
    long idx = pos < 0 ? line.length() - 1 : pos;
    for (int i = 0; i < idx; i++)
      out << " ";
    out << "^" << std::endl;
  }
};

#endif

extern ptr_list<context> context_stack;

#define PUSH_CONTEXT() try {
#define POP_CONTEXT(ctxt)			\
  } catch (...) {				\
    context_stack.push_front(new ctxt);		\
    throw;					\
  }

} // namespace ledger

#endif // _CONTEXT_H
