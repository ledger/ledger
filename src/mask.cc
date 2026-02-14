/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "mask.h"

namespace ledger {

bool ignore_diacritics = false;

namespace {

// Lookup table mapping Unicode codepoints of common accented Latin
// characters to their ASCII base equivalents.  This covers the Latin-1
// Supplement (U+00C0-U+00FF) and Latin Extended-A (U+0100-U+017F)
// ranges, which include the most frequently used European diacritics.
boost::uint32_t fold_codepoint(boost::uint32_t cp) {
  switch (cp) {
  // Latin-1 Supplement: uppercase
  case 0x00C0: case 0x00C1: case 0x00C2: case 0x00C3:
  case 0x00C4: case 0x00C5:
    return 'A';
  case 0x00C6: return 'A'; // AE ligature -> A
  case 0x00C7: return 'C';
  case 0x00C8: case 0x00C9: case 0x00CA: case 0x00CB:
    return 'E';
  case 0x00CC: case 0x00CD: case 0x00CE: case 0x00CF:
    return 'I';
  case 0x00D0: return 'D'; // Eth
  case 0x00D1: return 'N';
  case 0x00D2: case 0x00D3: case 0x00D4: case 0x00D5:
  case 0x00D6: case 0x00D8:
    return 'O';
  case 0x00D9: case 0x00DA: case 0x00DB: case 0x00DC:
    return 'U';
  case 0x00DD: return 'Y';
  case 0x00DE: return 'T'; // Thorn

  // Latin-1 Supplement: lowercase
  case 0x00DF: return 's'; // sharp s
  case 0x00E0: case 0x00E1: case 0x00E2: case 0x00E3:
  case 0x00E4: case 0x00E5:
    return 'a';
  case 0x00E6: return 'a'; // ae ligature -> a
  case 0x00E7: return 'c';
  case 0x00E8: case 0x00E9: case 0x00EA: case 0x00EB:
    return 'e';
  case 0x00EC: case 0x00ED: case 0x00EE: case 0x00EF:
    return 'i';
  case 0x00F0: return 'd'; // eth
  case 0x00F1: return 'n';
  case 0x00F2: case 0x00F3: case 0x00F4: case 0x00F5:
  case 0x00F6: case 0x00F8:
    return 'o';
  case 0x00F9: case 0x00FA: case 0x00FB: case 0x00FC:
    return 'u';
  case 0x00FD: case 0x00FF: return 'y';
  case 0x00FE: return 't'; // thorn

  // Latin Extended-A
  case 0x0100: case 0x0102: case 0x0104: return 'A';
  case 0x0101: case 0x0103: case 0x0105: return 'a';
  case 0x0106: case 0x0108: case 0x010A: case 0x010C: return 'C';
  case 0x0107: case 0x0109: case 0x010B: case 0x010D: return 'c';
  case 0x010E: case 0x0110: return 'D';
  case 0x010F: case 0x0111: return 'd';
  case 0x0112: case 0x0114: case 0x0116: case 0x0118: case 0x011A: return 'E';
  case 0x0113: case 0x0115: case 0x0117: case 0x0119: case 0x011B: return 'e';
  case 0x011C: case 0x011E: case 0x0120: case 0x0122: return 'G';
  case 0x011D: case 0x011F: case 0x0121: case 0x0123: return 'g';
  case 0x0124: case 0x0126: return 'H';
  case 0x0125: case 0x0127: return 'h';
  case 0x0128: case 0x012A: case 0x012C: case 0x012E: case 0x0130: return 'I';
  case 0x0129: case 0x012B: case 0x012D: case 0x012F: case 0x0131: return 'i';
  case 0x0134: return 'J';
  case 0x0135: return 'j';
  case 0x0136: return 'K';
  case 0x0137: case 0x0138: return 'k';
  case 0x0139: case 0x013B: case 0x013D: case 0x013F: case 0x0141: return 'L';
  case 0x013A: case 0x013C: case 0x013E: case 0x0140: case 0x0142: return 'l';
  case 0x0143: case 0x0145: case 0x0147: case 0x014A: return 'N';
  case 0x0144: case 0x0146: case 0x0148: case 0x0149: case 0x014B: return 'n';
  case 0x014C: case 0x014E: case 0x0150: return 'O';
  case 0x014D: case 0x014F: case 0x0151: return 'o';
  case 0x0152: return 'O'; // OE ligature
  case 0x0153: return 'o'; // oe ligature
  case 0x0154: case 0x0156: case 0x0158: return 'R';
  case 0x0155: case 0x0157: case 0x0159: return 'r';
  case 0x015A: case 0x015C: case 0x015E: case 0x0160: return 'S';
  case 0x015B: case 0x015D: case 0x015F: case 0x0161: return 's';
  case 0x0162: case 0x0164: case 0x0166: return 'T';
  case 0x0163: case 0x0165: case 0x0167: return 't';
  case 0x0168: case 0x016A: case 0x016C: case 0x016E:
  case 0x0170: case 0x0172: return 'U';
  case 0x0169: case 0x016B: case 0x016D: case 0x016F:
  case 0x0171: case 0x0173: return 'u';
  case 0x0174: return 'W';
  case 0x0175: return 'w';
  case 0x0176: case 0x0178: return 'Y';
  case 0x0177: return 'y';
  case 0x0179: case 0x017B: case 0x017D: return 'Z';
  case 0x017A: case 0x017C: case 0x017E: return 'z';

  default:
    return cp;
  }
}

} // namespace

string fold_diacritics(const string& text) {
  const char* p = text.c_str();
  std::size_t len = text.length();

  if (!utf8::is_valid(p, p + len))
    return text;

  std::vector<boost::uint32_t> utf32chars;
  utf8::unchecked::utf8to32(p, p + len, std::back_inserter(utf32chars));

  bool changed = false;
  for (auto& ch : utf32chars) {
    boost::uint32_t folded = fold_codepoint(ch);
    if (folded != ch) {
      ch = folded;
      changed = true;
    }
  }

  if (!changed)
    return text;

  string result;
  utf8::unchecked::utf32to8(utf32chars.begin(), utf32chars.end(),
                            std::back_inserter(result));
  return result;
}

mask_t::mask_t(const string& pat) : expr() {
  *this = pat;
  TRACE_CTOR(mask_t, "const string&");
}

mask_t& mask_t::operator=(const string& pat) {
  string folded_pat = ignore_diacritics ? fold_diacritics(pat) : pat;
#if HAVE_BOOST_REGEX_UNICODE
  expr = boost::make_u32regex(folded_pat.c_str(), boost::regex::perl | boost::regex::icase);
#else
  expr.assign(folded_pat.c_str(), boost::regex::perl | boost::regex::icase);
#endif
  VERIFY(valid());
  return *this;
}

mask_t& mask_t::assign_glob(const string& pat) {
  string re_pat = "";
  string::size_type len = pat.length();
  for (string::size_type i = 0; i < len; i++) {
    switch (pat[i]) {
    case '?':
      re_pat += '.';
      break;
    case '*':
      re_pat += ".*";
      break;
    case '[':
      while (i < len && pat[i] != ']')
        re_pat += pat[i++];
      if (i < len)
        re_pat += pat[i];
      break;

    case '\\':
      if (i + 1 < len) {
        re_pat += pat[++i];
        break;
      }
      // fallthrough...
    default:
      re_pat += pat[i];
      break;
    }
  }
  return (*this = re_pat);
}

} // namespace ledger
