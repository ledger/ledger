#include <cstring>
#include <cassert>
#include <vector>
#include "../../source/utf8.h"
using namespace utf8;
using namespace std;

int main()
{
    //append
    unsigned char u[5] = {0,0,0,0,0};

    append(0x0448, u);
    assert (u[0] == 0xd1 && u[1] == 0x88 && u[2] == 0 && u[3] == 0 && u[4] == 0);

    append(0x65e5, u);
    assert (u[0] == 0xe6 && u[1] == 0x97 && u[2] == 0xa5 && u[3] == 0 && u[4] == 0);

    append(0x3044, u);
    assert (u[0] == 0xe3 && u[1] == 0x81 && u[2] == 0x84 && u[3] == 0 && u[4] == 0);

    append(0x10346, u);
    assert (u[0] == 0xf0 && u[1] == 0x90 && u[2] == 0x8d && u[3] == 0x86 && u[4] == 0);


    //next
    const char* twochars = "\xe6\x97\xa5\xd1\x88";
    const char* w = twochars;
    int cp = next(w, twochars + 6);
    assert (cp == 0x65e5);
    assert (w == twochars + 3);

    const char* threechars = "\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
    w = threechars;
    cp = next(w, threechars + 9);
    assert (cp == 0x10346);
    assert (w == threechars + 4);
    cp = next(w, threechars + 9);
    assert (cp == 0x65e5);
    assert (w == threechars + 7);
    cp = next(w, threechars + 9);
    assert (cp == 0x0448);
    assert (w == threechars + 9);

    //peek_next
    const char* const cw = twochars;
    cp = peek_next(cw, cw + 6);
    assert (cp == 0x65e5);
    assert (cw == twochars);

    //prior
    w = twochars + 3;
    cp = prior (w, twochars);
    assert (cp == 0x65e5);
    assert (w == twochars);

    w = threechars + 9;
    cp = prior(w, threechars);
    assert (cp == 0x0448);
    assert (w == threechars + 7);
    cp = prior(w, threechars);
    assert (cp == 0x65e5);
    assert (w == threechars + 4);
    cp = prior(w, threechars);
    assert (cp == 0x10346);
    assert (w == threechars); 

    //previous (deprecated)
    w = twochars + 3;
    cp = previous (w, twochars - 1);
    assert (cp == 0x65e5);
    assert (w == twochars);

    w = threechars + 9;
    cp = previous(w, threechars - 1);
    assert (cp == 0x0448);
    assert (w == threechars + 7);
    cp = previous(w, threechars -1);
    assert (cp == 0x65e5);
    assert (w == threechars + 4);
    cp = previous(w, threechars - 1);
    assert (cp == 0x10346);
    assert (w == threechars); 

    // advance
    w = twochars;
    advance (w, 2, twochars + 6);
    assert (w == twochars + 5);

    // distance
    size_t dist = utf8::distance(twochars, twochars + 5);
    assert (dist == 2);

    // utf32to8
    int utf32string[] = {0x448, 0x65E5, 0x10346, 0};
    vector<char> utf8result;
    utf32to8(utf32string, utf32string + 3, back_inserter(utf8result));
    assert (utf8result.size() == 9);
    // try it with the return value;
    char* utf8_end = utf32to8(utf32string, utf32string + 3, &utf8result[0]);
    assert (utf8_end == &utf8result[0] + 9);

    //utf8to32
    vector<int> utf32result;
    utf8to32(twochars, twochars + 5, back_inserter(utf32result));
    assert (utf32result.size() == 2);
    // try it with the return value;
    int* utf32_end = utf8to32(twochars, twochars + 5, &utf32result[0]);
    assert (utf32_end == &utf32result[0] + 2);

    //utf16to8
    unsigned short utf16string[] = {0x41, 0x0448, 0x65e5, 0xd834, 0xdd1e};
    utf8result.clear();
    utf16to8(utf16string, utf16string + 5, back_inserter(utf8result));
    assert (utf8result.size() == 10);
    // try it with the return value;
    utf8_end = utf16to8 (utf16string, utf16string + 5, &utf8result[0]);
    assert (utf8_end == &utf8result[0] + 10);

    //utf8to16
    char utf8_with_surrogates[] = "\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e";
    vector <unsigned short> utf16result;
    utf8to16(utf8_with_surrogates, utf8_with_surrogates + 9, back_inserter(utf16result));
    assert (utf16result.size() == 4);
    assert (utf16result[2] == 0xd834);
    assert (utf16result[3] == 0xdd1e);
    // try it with the return value;
    unsigned short* utf16_end = utf8to16 (utf8_with_surrogates, utf8_with_surrogates + 9, &utf16result[0]);
    assert (utf16_end == &utf16result[0] + 4);

    //find_invalid
    char utf_invalid[] = "\xe6\x97\xa5\xd1\x88\xfa";
    char* invalid = find_invalid(utf_invalid, utf_invalid + 6);
    assert (invalid == utf_invalid + 5);

    //is_valid
    bool bvalid = is_valid(utf_invalid, utf_invalid + 6);
    assert (bvalid == false);
    bvalid = is_valid(utf8_with_surrogates, utf8_with_surrogates + 9);
    assert (bvalid == true);

    //starts_with_bom
    unsigned char byte_order_mark[] = {0xef, 0xbb, 0xbf};
    bool bbom = starts_with_bom(byte_order_mark, byte_order_mark + sizeof(byte_order_mark));
    assert (bbom == true);
	bool no_bbom = starts_with_bom(threechars, threechars + sizeof(threechars));
	assert (no_bbom == false);

    //is_bom
	bool unsafe_bbom = is_bom(byte_order_mark);
    assert (unsafe_bbom == true);

    
    //replace_invalid
    char invalid_sequence[] = "a\x80\xe0\xa0\xc0\xaf\xed\xa0\x80z";
    vector<char> replace_invalid_result(50);
    replace_invalid (invalid_sequence, invalid_sequence + sizeof(invalid_sequence), replace_invalid_result.begin(), '?');
    bvalid = is_valid(replace_invalid_result.begin(), replace_invalid_result.end());
    assert (bvalid);
    const char* fixed_invalid_sequence = "a????z";
    assert (std::equal(replace_invalid_result.begin(), replace_invalid_result.begin() + sizeof(fixed_invalid_sequence), fixed_invalid_sequence));

    // iterator
    utf8::iterator<const char*> it(threechars, threechars, threechars + 9);
    utf8::iterator<const char*> it2 = it;
    assert (it2 == it);
    assert (*it == 0x10346);
    assert (*(++it) == 0x65e5);
    assert ((*it++) == 0x65e5);
    assert (*it == 0x0448);
    assert (it != it2);
    utf8::iterator<const char*> endit (threechars + 9, threechars, threechars + 9);  
    assert (++it == endit);
    assert (*(--it) == 0x0448);
    assert ((*it--) == 0x0448);
    assert (*it == 0x65e5);
    assert (--it == utf8::iterator<const char*>(threechars, threechars, threechars + 9));
    assert (*it == 0x10346);

    //////////////////////////////////////////////////////////
    //// Unchecked variants
    //////////////////////////////////////////////////////////

    //append
    memset(u, 0, 5);
    append(0x0448, u);
    assert (u[0] == 0xd1 && u[1] == 0x88 && u[2] == 0 && u[3] == 0 && u[4] == 0);

    append(0x65e5, u);
    assert (u[0] == 0xe6 && u[1] == 0x97 && u[2] == 0xa5 && u[3] == 0 && u[4] == 0);

    append(0x10346, u);
    assert (u[0] == 0xf0 && u[1] == 0x90 && u[2] == 0x8d && u[3] == 0x86 && u[4] == 0);

    //next
    w = twochars;
    cp = unchecked::next(w);
    assert (cp == 0x65e5);
    assert (w == twochars + 3);

    w = threechars;
    cp = unchecked::next(w);
    assert (cp == 0x10346);
    assert (w == threechars + 4);
    cp = unchecked::next(w);
    assert (cp == 0x65e5);
    assert (w == threechars + 7);
    cp = unchecked::next(w);
    assert (cp == 0x0448);
    assert (w == threechars + 9);

    //peek_next
    cp = unchecked::peek_next(cw);
    assert (cp == 0x65e5);
    assert (cw == twochars);


    //previous (calls prior internally)

    w = twochars + 3;
    cp = unchecked::previous (w);
    assert (cp == 0x65e5);
    assert (w == twochars);

    w = threechars + 9;
    cp = unchecked::previous(w);
    assert (cp == 0x0448);
    assert (w == threechars + 7);
    cp = unchecked::previous(w);
    assert (cp == 0x65e5);
    assert (w == threechars + 4);
    cp = unchecked::previous(w);
    assert (cp == 0x10346);
    assert (w == threechars); 

    // advance
    w = twochars;
    unchecked::advance (w, 2);
    assert (w == twochars + 5);

    // distance
    dist = unchecked::distance(twochars, twochars + 5);
    assert (dist == 2);

    // utf32to8
    utf8result.clear();
    unchecked::utf32to8(utf32string, utf32string + 3, back_inserter(utf8result));
    assert (utf8result.size() == 9);
    // try it with the return value;
    utf8_end = utf32to8(utf32string, utf32string + 3, &utf8result[0]);
    assert(utf8_end == &utf8result[0] + 9);

    //utf8to32
    utf32result.clear();
    unchecked::utf8to32(twochars, twochars + 5, back_inserter(utf32result));
    assert (utf32result.size() == 2);
    // try it with the return value;
    utf32_end = utf8to32(twochars, twochars + 5, &utf32result[0]);
    assert (utf32_end == &utf32result[0] + 2);

    //utf16to8
    utf8result.clear();
    unchecked::utf16to8(utf16string, utf16string + 5, back_inserter(utf8result));
    assert (utf8result.size() == 10);
    // try it with the return value;
    utf8_end = utf16to8 (utf16string, utf16string + 5, &utf8result[0]);
    assert (utf8_end == &utf8result[0] + 10);

    //utf8to16
    utf16result.clear();
    unchecked::utf8to16(utf8_with_surrogates, utf8_with_surrogates + 9, back_inserter(utf16result));
    assert (utf16result.size() == 4);
    assert (utf16result[2] == 0xd834);
    assert (utf16result[3] == 0xdd1e);
    // try it with the return value;
    utf16_end = utf8to16 (utf8_with_surrogates, utf8_with_surrogates + 9, &utf16result[0]);
    assert (utf16_end == &utf16result[0] + 4);
    
    // iterator
    utf8::unchecked::iterator<const char*> un_it(threechars);
    utf8::unchecked::iterator<const char*> un_it2 = un_it;
    assert (un_it2 == un_it);
    assert (*un_it == 0x10346);
    assert (*(++un_it) == 0x65e5);
    assert ((*un_it++) == 0x65e5);
    assert (un_it != un_it2);
    assert (*un_it == 0x0448);
    utf8::unchecked::iterator<const char*> un_endit (threechars + 9);  
    assert (++un_it == un_endit);
    assert (*(--un_it) == 0x0448);
    assert ((*un_it--) == 0x0448);
    assert (*un_it == 0x65e5);
    assert (--un_it == utf8::unchecked::iterator<const char*>(threechars));
    assert (*un_it == 0x10346);
}


