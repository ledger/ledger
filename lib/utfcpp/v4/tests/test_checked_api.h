#ifndef UTF8_FOR_CPP_TEST_CHECKED_H_2675DCD0_9480_4c0c_B92A_CC14C027B731
#define UTF8_FOR_CPP_TEST_CHECKED_H_2675DCD0_9480_4c0c_B92A_CC14C027B731

#include "utf8.h"

#include <string>
#include <vector>
using namespace utf8;
using namespace std;


TEST(CheckedAPITests, test_append)
{
    unsigned char u[5] = {0,0,0,0,0};
    append(0x0448, u);
    EXPECT_EQ (u[0], 0xd1);
    EXPECT_EQ (u[1], 0x88);
    EXPECT_EQ (u[2], 0);
    EXPECT_EQ (u[3], 0);
    EXPECT_EQ (u[4], 0);

    append(0x65e5, u);
    EXPECT_EQ (u[0], 0xe6);
    EXPECT_EQ (u[1], 0x97);
    EXPECT_EQ (u[2], 0xa5);
    EXPECT_EQ (u[3], 0);
    EXPECT_EQ (u[4], 0);

    append(0x3044, u);
    EXPECT_EQ (u[0], 0xe3);
    EXPECT_EQ (u[1], 0x81);
    EXPECT_EQ (u[2], 0x84);
    EXPECT_EQ (u[3], 0);
    EXPECT_EQ (u[4], 0);

    append(0x10346, u);
    EXPECT_EQ (u[0], 0xf0);
    EXPECT_EQ (u[1], 0x90);
    EXPECT_EQ (u[2], 0x8d);
    EXPECT_EQ (u[3], 0x86);
    EXPECT_EQ (u[4], 0);

    // Ensure no warnings with plain char
    char c[2] = {0,0};
    append('a', c);
    EXPECT_EQ (c[0], 'a');
    EXPECT_EQ (c[1], 0);
}

TEST(CheckedAPITests, test_append16)
{
    utfchar16_t u[5] = {0,0};
    append16(0x0448, u);
    EXPECT_EQ (u[0], 0x0448);
    EXPECT_EQ (u[1], 0x0000);

    append16(0x65e5, u);
    EXPECT_EQ (u[0], 0x65e5);
    EXPECT_EQ (u[1], 0x0000);

    append16(0x10346, u);
    EXPECT_EQ (u[0], 0xd800);
    EXPECT_EQ (u[1], 0xdf46);
}

TEST(CheckedAPITests, test_next)
{
    const char* twochars = "\xe6\x97\xa5\xd1\x88";
    const char* w = twochars;
    unsigned int cp = next(w, twochars + 6);
    EXPECT_EQ (cp, 0x65e5);
    EXPECT_EQ (w, twochars + 3);

    const char* threechars = "\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
    w = threechars;

    cp = next(w, threechars + 9);
    EXPECT_EQ (cp, 0x10346);
    EXPECT_EQ (w, threechars + 4);

    cp = next(w, threechars + 9);
    EXPECT_EQ (cp, 0x65e5);
    EXPECT_EQ (w, threechars + 7);

    cp = next(w, threechars + 9);
    EXPECT_EQ (cp, 0x0448);
    EXPECT_EQ (w, threechars + 9);
}

TEST(CheckedAPITests, test_next16)
{
    const utfchar16_t u[3] = {0x65e5, 0xd800, 0xdf46};
    const utfchar16_t* w = u;
    utf8::utfchar32_t cp = next16(w, w + 3);
    EXPECT_EQ (cp, 0x65e5);
    EXPECT_EQ (w, u + 1);

    cp = next16(w, w + 2);
    EXPECT_EQ (cp, 0x10346);
    EXPECT_EQ (w, u + 3);
}

TEST(CheckedAPITests, test_peek_next)
{
    const char* const cw = "\xe6\x97\xa5\xd1\x88";
    unsigned int cp = peek_next(cw, cw + 6);
    EXPECT_EQ (cp, 0x65e5);
}

TEST(CheckedAPITests, test_prior)
{
    const char* twochars = "\xe6\x97\xa5\xd1\x88";
    const char* w = twochars + 3;
    unsigned int cp = prior (w, twochars);
    EXPECT_EQ (cp, 0x65e5);
    EXPECT_EQ (w, twochars);

    const char* threechars = "\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
    w = threechars + 9;
    cp = prior(w, threechars);
    EXPECT_EQ (cp, 0x0448);
    EXPECT_EQ (w, threechars + 7);
    cp = prior(w, threechars);
    EXPECT_EQ (cp, 0x65e5);
    EXPECT_EQ (w, threechars + 4);
    cp = prior(w, threechars);
    EXPECT_EQ (cp, 0x10346);
    EXPECT_EQ (w, threechars);
}

TEST(CheckedAPITests, test_advance)
{
    const char* threechars = "\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
    const char* w = threechars;
    advance(w, 2, threechars + 9);
    EXPECT_EQ(w, threechars + 7);
    advance(w, -2, threechars);
    EXPECT_EQ(w, threechars);
    advance(w, 3, threechars + 9);
    EXPECT_EQ(w, threechars + 9);
    advance(w, -2, threechars);
    EXPECT_EQ(w, threechars + 4);
    advance(w, -1, threechars);
    EXPECT_EQ(w, threechars);
}

TEST(CheckedAPITests, test_distance)
{
    const char* twochars = "\xe6\x97\xa5\xd1\x88";
    size_t dist = static_cast<size_t>(utf8::distance(twochars, twochars + 5));
    EXPECT_EQ (dist, 2);
}

TEST(CheckedAPITests, test_utf32to8)
{
    unsigned int utf32string[] = {0x448, 0x65E5, 0x10346, 0};
    string utf8result;
    utf32to8(utf32string, utf32string + 3, back_inserter(utf8result));
    EXPECT_EQ (utf8result.size(), 9);
}

TEST(CheckedAPITests, test_utf8to32)
{
    const char* twochars = "\xe6\x97\xa5\xd1\x88";
    vector<unsigned int> utf32result;
    utf8to32(twochars, twochars + 5, back_inserter(utf32result));
    EXPECT_EQ (utf32result.size(), 2);
}

TEST(CheckedAPITests, test_utf16to8)
{
    unsigned short utf16string[] = {0x41, 0x0448, 0x65e5, 0xd834, 0xdd1e};
    string utf8result;
    utf16to8(utf16string, utf16string + 5, back_inserter(utf8result));
    EXPECT_EQ (utf8result.size(), 10);
}

TEST(CheckedAPITests, test_utf8to16)
{
    char utf8_with_surrogates[] = "\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e";
    vector <unsigned short> utf16result;
    utf8to16(utf8_with_surrogates, utf8_with_surrogates + 9, back_inserter(utf16result));
    EXPECT_EQ (utf16result.size(), 4);
    EXPECT_EQ (utf16result[2], 0xd834);
    EXPECT_EQ (utf16result[3], 0xdd1e);
}

TEST(CheckedAPITests, test_replace_invalid)
{
    char invalid_sequence[] = "a\x80\xe0\xa0\xc0\xaf\xed\xa0\x80z";
    vector<char> replace_invalid_result;
    replace_invalid (invalid_sequence, invalid_sequence + sizeof(invalid_sequence), std::back_inserter(replace_invalid_result), '?');
    bool bvalid = is_valid(replace_invalid_result.begin(), replace_invalid_result.end());
    EXPECT_TRUE (bvalid);
    const char fixed_invalid_sequence[] = "a????z";
    EXPECT_EQ (sizeof(fixed_invalid_sequence), replace_invalid_result.size());
    EXPECT_TRUE (std::equal(replace_invalid_result.begin(), replace_invalid_result.begin() + sizeof(fixed_invalid_sequence), fixed_invalid_sequence));
}

TEST(CheckedAPITests, test_find_invalid)
{
    char utf_invalid[] = "\xe6\x97\xa5\xd1\x88\xfa";
    const char* invalid = find_invalid(utf_invalid, utf_invalid + 6);
    EXPECT_EQ (invalid, utf_invalid + 5);
    invalid = find_invalid(utf_invalid);
    EXPECT_EQ (invalid, utf_invalid + 5);
}

TEST(CheckedAPITests, test_is_valid)
{
    char utf_invalid[] = "\xe6\x97\xa5\xd1\x88\xfa";
    bool bvalid = is_valid(utf_invalid, utf_invalid + 6);
    EXPECT_FALSE (bvalid);
    bvalid = is_valid(utf_invalid);
    EXPECT_FALSE (bvalid);
    char utf8_with_surrogates[] = "\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e";
    bvalid = is_valid(utf8_with_surrogates, utf8_with_surrogates + 9);
    EXPECT_TRUE (bvalid);
    bvalid = is_valid(utf8_with_surrogates);
    EXPECT_TRUE (bvalid);
}

TEST(CheckedAPITests, test_starts_with_bom)
{
    unsigned char byte_order_mark[] = {0xef, 0xbb, 0xbf};
    bool bbom = starts_with_bom(byte_order_mark, byte_order_mark + sizeof(byte_order_mark));
    EXPECT_TRUE (bbom);
    const char* threechars = "\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
    bool no_bbom = starts_with_bom(threechars, threechars + sizeof(threechars));
    EXPECT_FALSE (no_bbom);
}

#endif
