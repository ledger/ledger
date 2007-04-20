#ifndef _TIMING_H
#define _TIMING_H

#include <ctime>

#include "trace.h"

namespace ledger {

class timing_t
{
 public:
  std::clock_t	begin;
  std::clock_t	cumulative;
  string	file;
  unsigned long line;
  string	symbol;
  string	category;

  timing_t(const string& _symbol, const string& _category)
    : begin(0), cumulative(0), symbol(_symbol), category(_category) {}

  timing_t(const string& _symbol)
    : begin(0), cumulative(0), symbol(_symbol) {}

  ~timing_t() {
    string cls = "timing.results.";
    cls += symbol;
#if 0
    // jww (2007-04-19): This crashes things nowadays
    DEBUG_PRINT(cls.c_str(), file << ":" << line << ": "
		<< category << " = "
		<< (double(cumulative) / double(CLOCKS_PER_SEC)) << "s");
#endif
  }

  void start(const string& _file, unsigned long _line) {
    file  = _file;
    line  = _line;
    begin = std::clock();
  }
  void start() {
    begin = std::clock();
  }

  void stop() {
    cumulative += std::clock() - begin;
  }
};

#if DEBUG_LEVEL >= 4
#define TIMER_DEF(sym, cat) static timing_t sym(#sym, cat);
#define TIMER_DEF_(sym) static timing_t sym(#sym, #sym);
#define TIMER_START(sym) sym.start(__FILE__, __LINE__);
#define TIMER_STOP(sym) sym.stop();
#else
#define TIMER_DEF(sym, cat)
#define TIMER_DEF_(sym)
#define TIMER_START(sym)
#define TIMER_STOP(sym)
#endif

} // namespace ledger

#endif // _TIMING_H
