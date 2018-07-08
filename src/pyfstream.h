/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _PYFSTREAM_H
#define _PYFSTREAM_H

// pyofstream
// - a stream that writes on a Python file object

class pyoutbuf : public boost::noncopyable, public std::streambuf
{
  pyoutbuf();

protected:
  PyFileObject * fo;    // Python file object

public:
  // constructor
  pyoutbuf(PyFileObject * _fo) : fo(_fo) {
    TRACE_CTOR(pyoutbuf, "PyFileObject *");
  }
  ~pyoutbuf() throw() {
    TRACE_DTOR(pyoutbuf);
  }

protected:
  // write one character
  virtual int_type overflow (int_type c) {
    if (c != EOF) {
      char z[2];
      z[0] = static_cast<char>(c);
      z[1] = '\0';
      if (PyFile_WriteString(z, reinterpret_cast<PyObject *>(fo)) < 0) {
        return EOF;
      }
    }
    return c;
  }

  // write multiple characters
  virtual std::streamsize xsputn (const char* s, std::streamsize num) {
    char * buf = new char[num + 1];
    std::strncpy(buf, s, static_cast<std::size_t>(num));
    buf[num] = '\0';
    if (PyFile_WriteString(buf, reinterpret_cast<PyObject *>(fo)) < 0)
      num = 0;
    boost::checked_array_delete(buf);
    return num;
  }
};

class pyofstream : public boost::noncopyable, public std::ostream
{
  pyofstream();

protected:
  pyoutbuf buf;

public:
  pyofstream (PyFileObject * fo) : std::ostream(0), buf(fo) {
    rdbuf(&buf);
    TRACE_CTOR(pyofstream, "PyFileObject *");
  }
  ~pyofstream() throw() {
    TRACE_DTOR(pyofstream);
  }
};

// pyifstream
// - a stream that reads on a file descriptor

class pyinbuf : public boost::noncopyable, public std::streambuf
{
  pyinbuf();

protected:
  PyFileObject * fo;    // Python file object

protected:
  /* data buffer:
   * - at most, pbSize characters in putback area plus
   * - at most, bufSize characters in ordinary read buffer
   */
  static const size_t pbSize  = 4;    // size of putback area
  static const size_t bufSize = 1024; // size of the data buffer
  char buffer[bufSize + pbSize];      // data buffer

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

    TRACE_CTOR(pyinbuf, "PyFileObject *");
  }
  ~pyinbuf() throw() {
    TRACE_DTOR(pyinbuf);
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
    size_t numPutback;
    numPutback = static_cast<size_t>(gptr() - eback());
    if (numPutback > pbSize) {
      numPutback = pbSize;
    }

    /* copy up to pbSize characters previously read into
     * the putback area
     */
    memmove (buffer+(pbSize-numPutback), gptr()-numPutback,
             numPutback);

    // read at most bufSize new characters
    PyObject *line = PyFile_GetLine(reinterpret_cast<PyObject *>(fo), bufSize);
    if (! line || ! PyString_Check(line)) {
      // ERROR or EOF
      return EOF;
    }

    Py_ssize_t num = PyString_Size(line);
    if (num == 0)
      return EOF;

    memmove(buffer+pbSize, PyString_AsString(line), static_cast<size_t>(num));

    // reset buffer pointers
    setg (buffer+(pbSize-numPutback),   // beginning of putback area
          buffer+pbSize,                // read position
          buffer+pbSize+num);           // end of buffer

    // return next character
    return traits_type::to_int_type(*gptr());
  }
};

class pyifstream : public boost::noncopyable, public std::istream
{
  pyifstream();

protected:
  pyinbuf buf;

public:
  pyifstream (PyFileObject * fo) : std::istream(0), buf(fo) {
    rdbuf(&buf);
    TRACE_CTOR(pyifstream, "PyFileObject *");
  }
  ~pyifstream() throw() {
    TRACE_DTOR(pyifstream);
  }
};

#endif // _PYFSTREAM_H
