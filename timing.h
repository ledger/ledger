#ifndef _TIMING_H
#define _TIMING_H

#include "debug.h"

#include <ctime>

namespace ledger {

class timing_t
{
 public:
  std::clock_t	begin;
  std::clock_t	cumulative;
  std::string	file;
  unsigned long line;
  std::string	symbol;
  std::string	category;

  timing_t(const std::string& _symbol, const std::string& _category)
    : begin(0), cumulative(0), symbol(_symbol), category(_category) {}

  timing_t(const std::string& _symbol)
    : begin(0), cumulative(0), symbol(_symbol) {}

  ~timing_t() {
    std::string cls = "timing.results.";
    cls += symbol;
    DEBUG_PRINT(cls.c_str(), file << ":" << line << ": "
		<< category << " = "
		<< (double(cumulative) / double(CLOCKS_PER_SEC)) << "s");
  }

  void start(const std::string& _file, unsigned long _line) {
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

#ifdef DEBUG_ENABLED
#define TIMER_DEF(sym, cat) static timing_t sym(#sym, cat)
#define TIMER_DEF_(sym) static timing_t sym(#sym, #sym)
#define TIMER_START(sym) sym.start(__FILE__, __LINE__)
#define TIMER_STOP(sym) sym.stop()
#else
#define TIMER_DEF(sym, cat)
#define TIMER_DEF_(sym)
#define TIMER_START(sym)
#define TIMER_STOP(sym)
#endif

} // namespace ledger

#endif // _TIMING_H
