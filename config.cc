#include "config.h"
#include "acconf.h"
#include "option.h"
//#include "quotes.h"
#include "valexpr.h"
#include "walk.h"

namespace ledger {

string expand_path(const string& pathname)
{
  if (pathname.length() == 0 || pathname[0] != '~')
    return pathname;

  const char * pfx = NULL;
  string::size_type pos = pathname.find_first_of('/');

  if (pathname.length() == 1 || pos == 1) {
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
    string user(pathname, 1, pos == string::npos ?
		     string::npos : pos - 1);
    struct passwd * pw = getpwnam(user.c_str());
    if (pw)
      pfx = pw->pw_dir;
  }
#endif

  // if we failed to find an expansion, return the pathname unchanged.

  if (! pfx)
    return pathname;

  string result(pfx);

  if (pos == string::npos)
    return result;

  if (result.length() == 0 || result[result.length() - 1] != '/')
    result += '/';

  result += pathname.substr(pos + 1);

  return result;
}

// jww (2008-04-22): This needs to be changed to use boost::filesystem
string resolve_path(const string& pathname)
{
  if (pathname[0] == '~')
    return expand_path(pathname);
  return pathname;
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

} // namespace ledger
