#include "config.h"
#include "acconf.h"
#include "option.h"
#include "datetime.h"
#include "quotes.h"
#include "valexpr.h"
#include "walk.h"

#include <fstream>
#include <cstdlib>
#ifdef WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

#ifdef HAVE_REALPATH
extern "C" char *realpath(const char *, char resolved_path[]);
#endif

#if defined(HAVE_GETPWUID) || defined(HAVE_GETPWNAM)
#include <pwd.h>
#endif

namespace ledger {

std::string expand_path(const std::string& path)
{
  if (path.length() == 0 || path[0] != '~')
    return path;

  const char * pfx = NULL;
  std::string::size_type pos = path.find_first_of('/');

  if (path.length() == 1 || pos == 1) {
    pfx = std::getenv("HOME");
#ifdef HAVE_GETPWUID
    if (! pfx) {
      // Punt. We're trying to expand ~/, but HOME isn't set
      struct passwd * pw = getpwuid(getuid());
      if (pw)
	pfx = pw->pw_dir;
    }
#endif
  }
#ifdef HAVE_GETPWNAM
  else {
    std::string user(path, 1, pos == std::string::npos ?
		     std::string::npos : pos - 1);
    struct passwd * pw = getpwnam(user.c_str());
    if (pw)
      pfx = pw->pw_dir;
  }
#endif

  // if we failed to find an expansion, return the path unchanged.

  if (! pfx)
    return path;

  std::string result(pfx);

  if (pos == std::string::npos)
    return result;

  if (result.length() == 0 || result[result.length() - 1] != '/')
    result += '/';

  result += path.substr(pos + 1);

  return result;
}

std::string resolve_path(const std::string& path)
{
  if (path[0] == '~')
    return expand_path(path);
  return path;
}

config_t::config_t()
{
  balance_format       = "%20T  %2_%-a\n";
  register_format      = ("%D %-.20P %-.22A %12.67t %!12.80T\n%/"
			  "%32|%-.22A %12.67t %!12.80T\n");
  wide_register_format = ("%D  %-.35P %-.38A %22.108t %!22.132T\n%/"
			  "%48|%-.38A %22.108t %!22.132T\n");
  plot_amount_format   = "%D %(@S(@t))\n";
  plot_total_format    = "%D %(@S(@T))\n";
  print_format	       = "\n%d %Y%C%P\n    %-34W  %12o%n\n%/    %-34W  %12o%n\n";
  write_hdr_format     = "%d %Y%C%P\n";
  write_xact_format    = "    %-34W  %12o%n\n";
  equity_format	       = "\n%D %Y%C%P\n%/    %-34W  %12t\n";
  prices_format	       = "%[%Y/%m/%d %H:%M:%S %Z]   %-10A %12t %12T\n";
  pricesdb_format      = "P %[%Y/%m/%d %H:%M:%S] %A %t\n";

  pricing_leeway       = 24 * 3600;

  download_quotes      = false;
  use_cache	       = false;
  cache_dirty	       = false;
  debug_mode	       = false;
  verbose_mode	       = false;
  trace_mode	       = false;
}

//////////////////////////////////////////////////////////////////////

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
