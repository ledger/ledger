#include "../../source/utf8.h"
using namespace utf8;
using namespace std;

int main()
{
    string u;
 #if __cplusplus >= 201103L // C++ 11 or later
    //append

    append(0x0448, u);
    assert (u[0] == char(0xd1) && u[1] == char(0x88) && u.length() == 2);

    u.clear();
    append(0x65e5, u);
    assert (u[0] == char(0xe6) && u[1] == char(0x97) && u[2] == char(0xa5) && u.length() == 3);

    u.clear();
    append(0x3044, u);
    assert (u[0] == char(0xe3) && u[1] == char(0x81) && u[2] == char(0x84) && u.length() == 3);

    u.clear();
    append(0x10346, u);
    assert (u[0] == char(0xf0) && u[1] == char(0x90) && u[2] == char(0x8d) && u[3] == char(0x86) && u.length() == 4);

    //utf16to8
    u16string utf16string = {0x41, 0x0448, 0x65e5, 0xd834, 0xdd1e};
    u.clear();
    u = utf16to8(utf16string);
    assert (u.size() == 10);

    //utf8to16
    string utf8_with_surrogates = "\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e";
    u16string utf16result = utf8to16(utf8_with_surrogates);
    assert (utf16result.length() == 4);
    assert (utf16result[2] == 0xd834);
    assert (utf16result[3] == 0xdd1e);

    // utf32to8
    u32string utf32string = {0x448, 0x65E5, 0x10346};
    string utf8result = utf32to8(utf32string);
    assert (utf8result.size() == 9);

    // utf8to32
    const char* twochars = "\xe6\x97\xa5\xd1\x88";
    u32string utf32result = utf8to32(twochars);
    assert (utf32result.size() == 2);

    //find_invalid
    string utf_invalid = "\xe6\x97\xa5\xd1\x88\xfa";
    auto invalid = find_invalid(utf_invalid);
    assert (invalid == 5);

    //is_valid
    bool bvalid = is_valid(utf_invalid);
    assert (bvalid == false);
    bvalid = is_valid(utf8_with_surrogates);
    assert (bvalid == true);

    //replace_invalid
    string invalid_sequence = "a\x80\xe0\xa0\xc0\xaf\xed\xa0\x80z";
    string replace_invalid_result = replace_invalid(invalid_sequence, '?');
    bvalid = is_valid(replace_invalid_result);
    assert (bvalid);
    const string fixed_invalid_sequence = "a????z";
    assert (fixed_invalid_sequence == replace_invalid_result);

    //starts_with_bom
    string byte_order_mark = {char(0xef), char(0xbb), char(0xbf)};
    bool bbom = starts_with_bom(byte_order_mark);
    assert (bbom == true);
    string threechars = "\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
	bool no_bbom = starts_with_bom(threechars);
	assert (no_bbom == false);
  

#endif  // C++ 11 or later
}
