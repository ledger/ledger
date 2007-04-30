#ifndef _CONTEXT_H
#define _CONTEXT_H

namespace ledger {

class context
{
public:
  string context;		// ex: 'While parsing file "%R" at line %L'
};

class file_context : public context
{
public:
  path pathname;		// ex: ledger.dat

  optional<long> linenum_beg;	// ex: 1010
  optional<long> linenum_end;	// ex: 1010
  optional<long> colnum_beg;	// ex: 8
  optional<long> colnum_end;	// ex: 8
  optional<long> position_beg;
  optional<long> position_end;
};

class string_context : public context
{
public:
  string text;			// ex: (The multi-line text of an entry)

  optional<long> linenum_beg_off; // ex: 2 / none means start at beginning
  optional<long> linenum_end_off; // ex: 2 / none means start at beginning
  optional<long> colnum_beg_off; // ex: 8 / none means start
  optional<long> colnum_end_off; // ex: 8 / none means start
};

} // namespace ledger

#endif // _CONTEXT_H
