#ifndef _CONTEXT_H
#define _CONTEXT_H

namespace ledger {

class context
{
public:
  string context;		// ex: 'While parsing file "%R" at line %L'

  string resource;		// ex: ledger.dat
  long	 linenum_beg;		// ex: 1010
  long	 linenum_end;		// ex: 1010
  long	 colnum_beg;		// ex: 8
  long	 colnum_end;		// ex: 8
  long	 position_beg;
  long	 position_end;

  string text;			// ex: (The multi-line text of an entry)
  long	 linenum_beg_off;	// ex: 2 / -1 means start at beginning
  long	 linenum_end_off;	// ex: 2 / -1 means start at beginning
  long	 colnum_beg_off;	// ex: 8 / -1 means start
  long	 colnum_end_off;	// ex: 8 / -1 means start
};

} // namespace ledger

#endif // _CONTEXT_H
