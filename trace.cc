#include "trace.h"
#include "acconf.h"

namespace ledger {

bool trace_mode;

void trace(const std::string& cat, const std::string& str)
{
  char buf[32];
  std::strftime(buf, 31, "%H:%M:%S", datetime_t::now.localtime());
  std::cerr << buf << " " << cat << ": " << str << std::endl;
}

void trace_push(const std::string& cat, const std::string& str,
		timing_t& timer)
{
  timer.start();
  trace(cat, str);
}

void trace_pop(const std::string& cat, const std::string& str,
	       timing_t& timer)
{
  timer.stop();
  std::ostringstream out;
  out << str << ": " << (double(timer.cumulative) / double(CLOCKS_PER_SEC)) << "s";
  trace(cat, out.str());
}

} // namespace ledger
