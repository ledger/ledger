#include "util.h"

#include <list>
#include <string>
#include <cstring>

#include <cstdlib>
#ifdef WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

#if defined(HAVE_GETPWUID) || defined(HAVE_GETPWNAM)
#include <pwd.h>
#endif

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

std::string abbreviate(const std::string& str, unsigned int width,
		       elision_style_t elision_style, const bool is_account,
		       int abbrev_length)
{
  const unsigned int len = str.length();
  if (len <= width)
    return str;

  assert(width < 4095);

  char buf[4096];

  switch (elision_style) {
  case TRUNCATE_LEADING:
    // This method truncates at the beginning.
    std::strncpy(buf, str.c_str() + (len - width), width);
    buf[0] = '.';
    buf[1] = '.';
    break;

  case TRUNCATE_MIDDLE:
    // This method truncates in the middle.
    std::strncpy(buf, str.c_str(), width / 2);
    std::strncpy(buf + width / 2,
		 str.c_str() + (len - (width / 2 + width % 2)),
		 width / 2 + width % 2);
    buf[width / 2 - 1] = '.';
    buf[width / 2] = '.';
    break;

  case ABBREVIATE:
    if (is_account) {
      std::list<std::string> parts;
      std::string::size_type beg = 0;
      for (std::string::size_type pos = str.find(':');
	   pos != std::string::npos;
	   beg = pos + 1, pos = str.find(':', beg))
	parts.push_back(std::string(str, beg, pos - beg));
      parts.push_back(std::string(str, beg));

      std::string result;
      unsigned int newlen = len;
      for (std::list<std::string>::iterator i = parts.begin();
	   i != parts.end();
	   i++) {
	// Don't contract the last element
	std::list<std::string>::iterator x = i;
	if (++x == parts.end()) {
	  result += *i;
	  break;
	}

	if (newlen > width) {
	  result += std::string(*i, 0, abbrev_length);
	  result += ":";
	  newlen -= (*i).length() - abbrev_length;
	} else {
	  result += *i;
	  result += ":";
	}
      }

      if (newlen > width) {
	// Even abbreviated its too big to show the last account, so
	// abbreviate all but the last and truncate at the beginning.
	std::strncpy(buf, result.c_str() + (result.length() - width), width);
	buf[0] = '.';
	buf[1] = '.';
      } else {
	std::strcpy(buf, result.c_str());
      }
      break;
    }
    // fall through...

  case TRUNCATE_TRAILING:
    // This method truncates at the end (the default).
    std::strncpy(buf, str.c_str(), width - 2);
    buf[width - 2] = '.';
    buf[width - 1] = '.';
    break;
  }
  buf[width] = '\0';

  return buf;
}
