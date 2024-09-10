#include "../extern/ftest/ftest.h"
#define UTF_CPP_CPLUSPLUS 202002L
#include "utf8.h"
#include <string>
using namespace utf8;
using namespace std;

TEST(CPP20APITests, test_utf16tou8)
{
    u16string utf16string = {0x41, 0x0448, 0x65e5, 0xd834, 0xdd1e};
    u16string_view utf16stringview{utf16string};
    u8string u = utf16tou8(utf16string);
    EXPECT_EQ (u.size(), 10);
    u = utf16tou8(utf16stringview);
    EXPECT_EQ (u.size(), 10);
}

TEST(CPP20APITests, tes20t_utf8to16)
{
    u8string utf8_with_surrogates{u8"\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e"};
    u16string utf16result = utf8to16(utf8_with_surrogates);
    EXPECT_EQ (utf16result.size(), 4);
    EXPECT_EQ (utf16result[2], 0xd834);
    EXPECT_EQ (utf16result[3], 0xdd1e);
}

TEST(CPP20APITests, test_utf32tou8)
{
    u32string utf32string = {0x448, 0x65E5, 0x10346};
    u32string_view utf32stringview{utf32string};
    u8string utf8result = utf32tou8(utf32stringview);
    EXPECT_EQ (utf8result.size(), 9);
}

TEST(CPP20APITests, test_utf8to32)
{
    u8string twochars = u8"\xe6\x97\xa5\xd1\x88";
    u32string utf32result = utf8to32(twochars);
    EXPECT_EQ (utf32result.size(), 2);
}

TEST(CPP20APITests, test_find_invalid)
{
    u8string utf_invalid = u8"\xe6\x97\xa5\xd1\x88\xfa";
    auto invalid = find_invalid(utf_invalid);
    EXPECT_EQ (invalid, 5);
}

TEST(CPP20APITests, test_is_valid)
{
    u8string utf_invalid = u8"\xe6\x97\xa5\xd1\x88\xfa";
    bool bvalid = is_valid(utf_invalid);
    EXPECT_FALSE (bvalid);
    u8string utf8_with_surrogates = u8"\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e";
    bvalid = is_valid(utf8_with_surrogates);
    EXPECT_TRUE (bvalid);
}

TEST(CPP20APITests, test_replace_invalid)
{
    u8string invalid_sequence = u8"a\x80\xe0\xa0\xc0\xaf\xed\xa0\x80z";
    u8string replace_invalid_result = replace_invalid(invalid_sequence, u8'?');
    bool bvalid = is_valid(replace_invalid_result);
    EXPECT_TRUE (bvalid);
    const u8string fixed_invalid_sequence = u8"a????z";
    EXPECT_EQ(fixed_invalid_sequence, replace_invalid_result);
}

TEST(CPP20APITests, test_starts_with_bom)
{
    u8string byte_order_mark = u8"\xef\xbb\xbf";
    bool bbom = starts_with_bom(byte_order_mark);
    EXPECT_TRUE (bbom);
    u8string threechars = u8"\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88";
    bool no_bbom = starts_with_bom(threechars);
    EXPECT_FALSE (no_bbom);
}
