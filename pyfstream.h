#ifndef _PYFSTREAM_H
#define _PYFSTREAM_H

#include <istream>
#include <ostream>
#include <streambuf>
#include <cstdio>
#include <cstring>

#include "Python.h"

// pyofstream
// - a stream that writes on a Python file object

class pyoutbuf : public std::streambuf {
 protected:
  PyFileObject * fo;    // Python file object
 public:
  // constructor
  pyoutbuf (PyFileObject * _fo) : fo(_fo) {}

 protected:
  // write one character
  virtual int_type overflow (int_type c) {
    if (c != EOF) {
      char z[2];
      z[0] = c;
      z[1] = '\0';
      if (PyFile_WriteString(z, (PyObject *)fo) < 0) {
	return EOF;
      }
    }
    return c;
  }

  // write multiple characters
  virtual std::streamsize xsputn (const char* s, std::streamsize num) {
    if (PyFile_WriteString(s, (PyObject *)fo) < 0)
      return 0;
    return num;
  }
};

class pyofstream : public std::ostream {
 protected:
  pyoutbuf buf;
 public:
  pyofstream (PyFileObject * fo) : std::ostream(0), buf(fo) {
    rdbuf(&buf);
  }
};

// pyifstream
// - a stream that reads on a file descriptor

class pyinbuf : public std::streambuf {
 protected:
  PyFileObject * fo;    // Python file object
 protected:
  /* data buffer:
   * - at most, pbSize characters in putback area plus
   * - at most, bufSize characters in ordinary read buffer
   */
  static const int pbSize  = 4;	   // size of putback area
  static const int bufSize = 1024; // size of the data buffer
  char buffer[bufSize + pbSize];   // data buffer

 public:
  /* constructor
   * - initialize file descriptor
   * - initialize empty data buffer
   * - no putback area
   * => force underflow()
   */
  pyinbuf (PyFileObject * _fo) : fo(_fo) {
    setg (buffer+pbSize,     // beginning of putback area
	  buffer+pbSize,     // read position
	  buffer+pbSize);    // end position
  }

 protected:
  // insert new characters into the buffer
  virtual int_type underflow () {
#ifndef _MSC_VER
    using std::memmove;
#endif

    // is read position before end of buffer?
    if (gptr() < egptr()) {
      return traits_type::to_int_type(*gptr());
    }

    /* process size of putback area
     * - use number of characters read
     * - but at most size of putback area
     */
    int numPutback;
    numPutback = gptr() - eback();
    if (numPutback > pbSize) {
      numPutback = pbSize;
    }

    /* copy up to pbSize characters previously read into
     * the putback area
     */
    memmove (buffer+(pbSize-numPutback), gptr()-numPutback,
	     numPutback);

    // read at most bufSize new characters
    int num;
    PyObject *line = PyFile_GetLine((PyObject *)fo, bufSize);
    if (! line || ! PyString_Check(line)) {
      // ERROR or EOF
      return EOF;
    }

    num = PyString_Size(line);
    if (num == 0)
      return EOF;

    memmove (buffer+pbSize, PyString_AsString(line), num);

    // reset buffer pointers
    setg (buffer+(pbSize-numPutback),   // beginning of putback area
	  buffer+pbSize,                // read position
	  buffer+pbSize+num);           // end of buffer

    // return next character
    return traits_type::to_int_type(*gptr());
  }
};

class pyifstream : public std::istream {
 protected:
  pyinbuf buf;
 public:
  pyifstream (PyFileObject * fo) : std::istream(0), buf(fo) {
    rdbuf(&buf);
  }
};

#endif // _PYFSTREAM_H
