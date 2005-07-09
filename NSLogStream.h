#include <iostream>
#include <sstream>
#include <streambuf>

// NSLogStream
// - a stream that writes to NSLog

class NSLogBuffer : public std::streambuf {
 public:
  std::string buf;
  std::ostringstream stream;

  NSLogBuffer() : stream(buf) {}

  // write one character
  virtual int_type overflow (int_type c) {
    if (c != '\n')
      stream << c;
    return c;
  }

  // write multiple characters
  virtual std::streamsize xsputn (const char* s, std::streamsize num) {
    if (num > 0 && s[num - 1] == '\n')
      num--;
    stream.write(s, num);
    return num;
  }

  virtual int sync() {
    stream.flush();
    NSLog([NSString stringWithCString:stream.str().c_str()]);
    buf.clear();
    return 0;
  }
};

class NSLogStream : public std::ostream {
 protected:
  NSLogBuffer buf;

 public:
  NSLogStream () : std::ostream(0) {
    rdbuf(&buf);
  }
};
