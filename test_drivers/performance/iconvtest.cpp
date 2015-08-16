#include <iconv.h>
#include <string.h>
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
        cout << "\nUsage: iconvtest filename\n";
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
    char* end_buf = buf + length;
    // fill the data
    fs8.read(buf, length);
    fs8.close();
    // the UTF-16 result will not be larger than this (I hope :) )
    vector<unsigned char> temputf16;
    utf8::utf8to16(buf, end_buf, back_inserter(temputf16));
    int wlength = temputf16.size();
    unsigned short* utf16buf = new unsigned short[wlength];

    cout << "UTF8 to UTF-16\n";
    {
        memset (utf16buf, 0 , wlength * sizeof(unsigned short));
        // utf-8 cpp:
        cout << "utf8::utf8to16: ";
        timer t(cout);
        utf8::utf8to16(buf, buf + length, utf16buf);
        t.print_time();
    }

    {
        memset (utf16buf, 0 , wlength * sizeof(unsigned short));
        // utf-8 cpp:
        cout << "unchecked::utf8to16: ";
        timer t(cout);
        utf8::unchecked::utf8to16(buf, buf + length, utf16buf);
        t.print_time();	
    }

    // the UTF-16 result will not be larger than this (I hope :) )
    unsigned short* utf16iconvbuf = new unsigned short[wlength];
    {
        memset (utf16iconvbuf, 0 , wlength * sizeof(unsigned short));
        // iconv
        cout << "iconv: ";

        iconv_t cd = iconv_open("UTF-16LE", "UTF-8");
        if (cd == iconv_t(-1)) {
            cout << "Error openning the iconv stream";
            return 0;
        } 
        char* inbuf = buf;
        size_t in_bytes_left = length;
        char* outbuf = (char*)utf16iconvbuf;
        size_t out_bytes_left = wlength * sizeof (unsigned short);
        {
            timer t(cout);
            iconv(cd, &inbuf, &in_bytes_left, &outbuf, &out_bytes_left);
            t.print_time();
        }
        iconv_close(cd);
    }

    // just check the correctness while we are here:
    if (!equal(utf16buf, utf16buf + wlength, utf16iconvbuf)) 
        cout << "Different result!!!\n";
    
    // the other way around
    cout << "UTF16 to UTF-8\n";
    {
        //iconv
        memset(buf, 0, length);    
        cout<< "iconv: ";

        iconv_t cd = iconv_open("UTF-8", "UTF-16LE");
        if (cd == iconv_t(-1)) {
            cout << "Error openning the iconv stream";
            return 0;
        } 
        char* inbuf = (char*)utf16buf;
        size_t in_bytes_left = wlength * sizeof(unsigned short);
        char* outbuf =buf;
        size_t out_bytes_left = length;
        {
            timer t(cout);
            iconv(cd, &inbuf, &in_bytes_left, &outbuf, &out_bytes_left);
            t.print_time();
        }
        iconv_close(cd);
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
