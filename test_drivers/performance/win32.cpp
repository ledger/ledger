#include <windows.h>
#include "../../source/utf8.h"
#include "timer.h"
#include <fstream>
#include <algorithm>
#include <vector>
using namespace std;

using namespace utf8;

int main(int argc, char** argv)
{
    if (argc != 2) {
        cout << "\nUsage: win32test filename\n";
        return 0;
    }
    const char* test_file_path = argv[1];
    // Open the test file (UTF-8 encoded text)
    ifstream fs8(test_file_path, ios::binary);
    if (!fs8.is_open()) {
    cout << "Could not open " << test_file_path << endl;
    return 0;
    }
    // get length
    fs8.seekg(0, ios::end);
    int length = fs8.tellg();
    fs8.seekg(0, ios::beg);

    // allocate the buffer (no vector - we are benchmarking conversions, not STL
    char* buf = new char[length];
    // fill the data
    fs8.read(buf, length);
    fs8.close();
    cout << "UTF8 > UTF16\n";
    // the UTF-16 result will not be larger than this (I hope :) )
    vector<wchar_t> temputf16;
    utf8::utf8to16(buf, buf + length, back_inserter(temputf16));
    vector<wchar_t>::size_type wlength = temputf16.size();
    wchar_t* utf16buf = new wchar_t[wlength];

    {
        memset (utf16buf, 0 , wlength * sizeof(wchar_t));
        // utf-8 cpp:
        cout << "utf8::utf8to16: ";
        timer t(cout);
        utf8::utf8to16(buf, buf + length, utf16buf);
		t.print_time();
    }

    {
        memset (utf16buf, 0 , wlength * sizeof(wchar_t));
        // utf-8 cpp:
        cout << "unchecked::utf8to16: ";
        timer t(cout);
        utf8::unchecked::utf8to16(buf, buf + length, utf16buf);
		t.print_time();
    }
    // the UTF-16 result will not be larger than this (I hope :) )
    wchar_t* utf16iconvbuf = new wchar_t[wlength];
    {
        memset (utf16iconvbuf, 0 , wlength * sizeof(wchar_t));
        // win32
        cout << "win32: ";

        {
            timer t(cout);
            MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, buf, length, utf16iconvbuf, int(wlength));
			t.print_time();
        }

    }

    // just check the correctness while we are here:
    if (!equal(utf16buf, utf16buf + wlength, utf16iconvbuf))
        cout << "Different result!!!";

    // the other way around
    cout << "UTF16 to UTF-8\n";
    {
        //win32
        memset(buf, 0, length);
        cout<< "win32: ";

        {
            timer t(cout);
            WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, utf16buf, int(wlength), buf, length, NULL, NULL);
			t.print_time();
        }
    }

    {
        memset (buf, 0 , length);
        // utf-8 cpp:
        cout << "unchecked::utf16to8: ";
        timer t(cout);
        utf8::unchecked::utf16to8(utf16buf, utf16buf + wlength, buf);
		t.print_time();
    }

    {
        memset (buf, 0 , length);
        cout << "utf16to8: ";
        timer t(cout);
        utf8::utf16to8(utf16buf, utf16buf + wlength, buf);
		t.print_time();
    }

    delete [] buf;
    delete [] utf16buf;
}
