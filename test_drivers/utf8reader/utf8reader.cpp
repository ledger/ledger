#include "../../source/utf8.h"
using namespace utf8;

#include <string>
#include <iostream>
#include <fstream>
#include <vector>
using namespace std;

int main(int argc, char** argv)
{
    if (argc != 2) {
        cout << "\nUsage: utfreader filename\n";
        return 0;
    }
    const char* TEST_FILE_PATH = argv[1];
    // Open the test file
    ifstream fs8(TEST_FILE_PATH);
    if (!fs8.is_open()) {
    cout << "Could not open " << TEST_FILE_PATH << endl;
    return 0;
    }

    // Read it line by line
    unsigned int line_count = 0;
    char byte;
    while (!fs8.eof()) {
        string line;
        while ((byte = static_cast<char>(fs8.get())) != '\n' && !fs8.eof()) 
            line.push_back(byte);

        line_count++;
	// Play around with each line and convert it to utf16
        string::iterator line_start = line.begin();
        string::iterator line_end   = line.end();
        line_end = find_invalid(line_start, line_end);
        if (line_end != line.end()) 
            cout << "Line " << line_count << ": Invalid utf-8 at byte " << int(line.end() - line_end) << '\n';

        // Convert it to utf-16 and write to the file
        vector<unsigned short> utf16_line;
        utf8to16(line_start, line_end, back_inserter(utf16_line));

        // Back to utf-8 and compare it to the original line.
        string back_to_utf8;
        utf16to8(utf16_line.begin(), utf16_line.end(), back_inserter(back_to_utf8));
        if (back_to_utf8.compare(string(line_start, line_end)) != 0) 
            cout << "Line " << line_count << ": Conversion to UTF-16 and back failed" << '\n';

        // Now, convert it to utf-32, back to utf-8 and compare
        vector <unsigned> utf32_line;
        utf8to32(line_start, line_end, back_inserter(utf32_line));
        back_to_utf8.clear();
        utf32to8(utf32_line.begin(), utf32_line.end(), back_inserter(back_to_utf8));
        if (back_to_utf8.compare(string(line_start, line_end)) != 0) 
            cout << "Line " << line_count << ": Conversion to UTF-32 and back failed" << '\n';

        // Now, iterate and back
        unsigned char_count = 0;
        string::iterator it = line_start;
        while (it != line_end) {
            unsigned int next_cp = peek_next(it, line_end);
            if (next(it, line_end) != next_cp)
                cout << "Line " << line_count << ": Error: peek_next gave a different result than next" << '\n';
            char_count++;
        }
        if (char_count != utf32_line.size())
            cout << "Line " << line_count << ": Error in iterating with next - wrong number of characters" << '\n';

        string::iterator adv_it = line_start;
        utf8::advance(adv_it, char_count, line_end);
        if (adv_it != line_end)
            cout << "Line " << line_count << ": Error in advance function" << '\n';

        if (string::size_type(utf8::distance(line_start, line_end)) != char_count)
            cout << "Line " << line_count << ": Error in distance function" << '\n';

        while (it != line_start) {
            previous(it, line.rend().base());
            char_count--;
        }
        if (char_count != 0)
            cout << "Line " << line_count << ": Error in iterating with previous - wrong number of characters" << '\n';

        // Try utf8::iterator
        utf8::iterator<string::iterator> u8it(line_start, line_start, line_end);
        if (!utf32_line.empty() && *u8it != utf32_line.at(0))
          cout << "Line " << line_count << ": Error in utf::iterator * operator" << '\n'; 
        if (std::distance(u8it, utf8::iterator<string::iterator>(line_end, line_start, line_end)) != static_cast<int>(utf32_line.size()))
          cout << "Line " << line_count << ": Error in using utf::iterator with std::distance - wrong number of characters" << '\n';

        std::advance(u8it, utf32_line.size());
        if (u8it != utf8::iterator<string::iterator>(line_end, line_start, line_end))
          cout << "Line " << line_count << ": Error in using utf::iterator with std::advance" << '\n';


        //======================== Now, the unchecked versions ======================
        // Convert it to utf-16 and compare to the checked version
        vector<unsigned short> utf16_line_unchecked;
        unchecked::utf8to16(line_start, line_end, back_inserter(utf16_line_unchecked));

        if (utf16_line != utf16_line_unchecked)
            cout << "Line " << line_count << ": Error in unchecked::utf8to16" << '\n';

        // Back to utf-8 and compare it to the original line.
        back_to_utf8.clear();
        unchecked::utf16to8(utf16_line_unchecked.begin(), utf16_line_unchecked.end(), back_inserter(back_to_utf8));
        if (back_to_utf8.compare(string(line_start, line_end)) != 0) 
            cout << "Line " << line_count << ": Unchecked conversion to UTF-16 and back failed" << '\n';

        // Now, convert it to utf-32, back to utf-8 and compare
        vector <unsigned> utf32_line_unchecked;
        unchecked::utf8to32(line_start, line_end, back_inserter(utf32_line_unchecked));
        if (utf32_line != utf32_line_unchecked)
            cout << "Line " << line_count << ": Error in unchecked::utf8to32" << '\n';

        back_to_utf8.clear();
        unchecked::utf32to8(utf32_line.begin(), utf32_line.end(), back_inserter(back_to_utf8));
        if (back_to_utf8.compare(string(line_start, line_end)) != 0) 
            cout << "Line " << line_count << ": Unchecked conversion to UTF-32 and back failed" << '\n';

        // Now, iterate and back
        char_count = 0;
        it = line_start;
        while (it != line_end) {
            unsigned int next_cp = unchecked::peek_next(it); 
            if (unchecked::next(it) != next_cp)
              cout << "Line " << line_count << ": Error: unchecked::peek_next gave a different result than unchecked::next" << '\n';;
            char_count++;
        }
        if (char_count != utf32_line.size())
            cout << "Line " << line_count << ": Error in iterating with unchecked::next - wrong number of characters" << '\n';

        adv_it = line_start;
        utf8::unchecked::advance(adv_it, char_count);
        if (adv_it != line_end)
            cout << "Line " << line_count << ": Error in unchecked::advance function" << '\n';

        if (string::size_type(utf8::unchecked::distance(line_start, line_end)) != char_count)
            cout << "Line " << line_count << ": Error in unchecked::distance function" << '\n';

        while (it != line_start) {
            unchecked::previous(it);
            char_count--;
        }
        if (char_count != 0)
            cout << "Line " << line_count << ": Error in iterating with unchecked::previous - wrong number of characters" << '\n';

        // Try utf8::unchecked::iterator
        utf8::unchecked::iterator<string::iterator> un_u8it(line_start);
        if (!utf32_line.empty() && *un_u8it != utf32_line.at(0))
          cout << "Line " << line_count << ": Error in utf::unchecked::iterator * operator" << '\n'; 
        if (std::distance(un_u8it, utf8::unchecked::iterator<string::iterator>(line_end)) != static_cast<int>(utf32_line.size()))
          cout << "Line " << line_count << ": Error in using utf::unchecked::iterator with std::distance - wrong number of characters" << '\n';

        std::advance(un_u8it, utf32_line.size());
        if (un_u8it != utf8::unchecked::iterator<string::iterator>(line_end))
          cout << "Line " << line_count << ": Error in using utf::unchecked::iterator with std::advance" << '\n';
    }
}
