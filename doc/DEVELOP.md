GLOSSARY
----

Developing the Ledger software uses a number different tools, not all of
which will be familiar to all developers.

[**Boost**](http://www.boost.org): a standard set of C++ libraries.  Most
Boost libraries consist of inline functions and templates in header files.

[**CMake**](http://www.cmake.org): A cross platform system for building
from source code.  It uses the *CMakeLists.txt* files.

[**DOxygen**](http://doxygen.org): generates programming documentation from
source code files.  Primarly used on C++ sources, but works on all.  Uses
the *doc/Doxyfile.in* file.

[**GCC**](http://gcc.gnu.org): Gnu Compiler Collection, which includes the
*gcc* compiler and *gcov* coverage/profiler tool.

[**GMP**](https://gmplib.org): Gnu Multiple Precision Arithmetic Library
provides arbitrary precision math.

[**Markdown**](https://daringfireball.net/projects/markdown/): A typesetter
format that produces *html* files from *\*.md* files.  Note that GitHub automatically renders *.md* files.

[**Texinfo**](http://www.gnu.org/software/texinfo/): Gnu documentation
typesetter that produces *html* and *pdf* files from the *doc/\*.texi*
files.

[**Travis CI**](https://travis-ci.org): a hosted continuous integration
  service that builds and runs tests each commit posted to GitHub.  Each
  build creates a [log](https://travis-ci.org/ledger/ledger), updates a
  [small graphic](https://travis-ci.org/ledger/ledger.png?branch=master) at
  the top left of the main project's
  [README.md](https://github.com/ledger/ledger/blob/master/README.md), and
  emails the author of the commit if any tests fail.


Orientation
---

The source tree can be confusing to a new developer.  Here is a selective
orientation:

**./acprep**: a custom thousand-line script to install dependencies, grab
  updates, and build.  It also creates *\*.cmake*,
  *./CmakeFiles/* and other CMake temporary files.  Use *./acprep --help*
  for more information.

**./README.md**: user readme file in markdown format, also used as the project
  discription on GitHub.

**./contrib**: contributed scripts of random quality and completion.  They usually require editing to run.

**./doc**: documentation, licenses, and
  tools for generating documents such as the *pdf* manual.
