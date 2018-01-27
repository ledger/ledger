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

#include <system.hh>

#include "lookup.h"
#include "unistring.h"

namespace ledger {

namespace {
  typedef std::pair<xact_t *, int> score_entry_t;
  typedef std::deque<score_entry_t> scorecard_t;
  typedef std::map<uint32_t, std::size_t> char_positions_map;

  struct score_sorter {
    bool operator()(const score_entry_t& left,
                    const score_entry_t& right) const {
      return left.second > right.second;
    }
  };

  typedef std::map<account_t *, int>  account_use_map;
  typedef std::pair<account_t *, int> account_use_pair;

  struct usage_sorter {
    bool operator()(const account_use_pair& left,
                    const account_use_pair& right) const {
      return left.second < right.second;
    }
  };
}

std::pair<xact_t *, account_t *>
lookup_probable_account(const string& ident,
                        xacts_list::reverse_iterator iter,
                        xacts_list::reverse_iterator end,
                        account_t * ref_account)
{
  scorecard_t scores;

#if !HAVE_BOOST_REGEX_UNICODE
    string lident = ident;
    to_lower(lident);
    unistring lowered_ident(lident);
#else
    // jww (2010-03-07): Not yet implemented
    unistring lowered_ident(ident);
#endif

  DEBUG("lookup.account",
        "Looking up identifier '" << lowered_ident.extract() << "'");
#if DEBUG_ON
  if (ref_account != NULL)
    DEBUG("lookup.account",
          "  with reference account: " << ref_account->fullname());
#endif

  xact_t * xact;
  while (iter != end && (xact = *iter++) != NULL) {
#if 0
    // Only consider transactions from the last two years (jww (2010-03-07):
    // make this an option)
    if ((CURRENT_DATE() - xact->date()).days() > 700)
      continue;
#endif

    // An exact match is worth a score of 100 and terminates the search
    if (ident == xact->payee) {
      DEBUG("lookup", "  we have an exact match, score = 100");
      scores.push_back(score_entry_t(xact, 100));
      break;
    }

#if !HAVE_BOOST_REGEX_UNICODE
    string payee = xact->payee;
    to_lower(payee);
    unistring value_key(payee);
#else
    // jww (2010-03-07): Not yet implemented
    unistring value_key(xact->payee);
#endif

    DEBUG("lookup", "Considering payee: " << value_key.extract());

    std::size_t        index          = 0;
    std::size_t        last_match_pos = unistring::npos;
    int                bonus          = 0;
    int                score          = 0;
    std::size_t        pos;
    char_positions_map positions;

    // Walk each letter in the source identifier
    foreach (const uint32_t& ch, lowered_ident.utf32chars) {
      int         addend      = 0;
      bool        added_bonus = false;
      std::size_t value_len   = value_key.length();

      pos = value_key.find(ch);

      // Ensure that a letter which has been matched is not matched twice, so
      // that the two x's of Exxon don't both match to the single x in Oxford.
      // This part of the loop is very expensive, but avoids a lot of bogus
      // matches.

      char_positions_map::iterator pi = positions.find(ch);
      while (pi != positions.end() &&
             pos != unistring::npos && pos <= (*pi).second &&
             (*pi).second + 1 < value_len)
        pos = value_key.find(ch, (*pi).second + 1);

      if (pos != unistring::npos) {
        if (pi != positions.end())
          (*pi).second = pos;
        else
          positions.insert(char_positions_map::value_type(ch, pos));

        // If it occurs in the same order as the source identifier -- that is,
        // without intervening letters to break the pattern -- it's worth 10
        // points.  Plus, an extra point is added for every letter in chains
        // of 3 or more.

        if (last_match_pos == unistring::npos ?
            index == 0 && pos == 0 : pos == last_match_pos + 1) {
          DEBUG("lookup",
                "  char " << index << " in-sequence match with bonus " << bonus);
          addend += 10;
          if (bonus > 2)
            addend += bonus - 2;
          bonus++;
          added_bonus = true;

          last_match_pos = pos;
        }

        // If it occurs in the same general sequence as the source identifier,
        // it's worth 5 points, plus an extra point if it's within the next 3
        // characters, and an extra point if it's preceded by a non-alphabetic
        // character.
        //
        // If the letter occurs at all in the target identifier, it's worth 1
        // point, plus an extra point if it's within 3 characters, and an
        // extra point if it's preceded by a non-alphabetic character.

        else {
          bool in_order_match = (last_match_pos != unistring::npos &&
                                 pos > last_match_pos);
          DEBUG("lookup", "  char " << index << " " <<
                (in_order_match ? "in-order" : "out-of-order")
                << " match" << (in_order_match && pos - index < 3 ?
                                " with proximity bonus of 1" : ""));

          if (pos < index)
            addend += 1;
          else
            addend += 5;

          if (in_order_match && pos - index < 3)
            addend++;

#if 0
#if !HAVE_BOOST_REGEX_UNICODE
          if (pos == 0 || (pos > 0 && !std::isalnum(value_key[pos - 1])))
            addend++;
#else
          // jww (2010-03-07): Not yet implemented
#endif
#endif

          last_match_pos = pos;
        }

      // If the letter does not appear at all, decrease the score by 1

      } else {
        last_match_pos = unistring::npos;

        DEBUG("lookup", "  char " << index << " does not match");
        addend--;
      }

      // Finally, decay what is to be added to the score based on its position
      // in the word.  Since credit card payees in particular often share
      // information at the end (such as the location where the purchase was
      // made), we want to give much more credence to what occurs at the
      // beginning.  Every 5 character positions from the beginning becomes a
      // divisor for the addend.

      if ((int(index / 5) + 1) > 1) {
        DEBUG("lookup",
              "  discounting the addend by / " << (int(index / 5) + 1));
        addend = int(double(addend) / (int(index / 5) + 1));
      }

      DEBUG("lookup", "  final addend is " << addend);
      score += addend;
      DEBUG("lookup", "  score is " << score);

      if (! added_bonus)
        bonus = 0;

      index++;
    }

    // Only consider payees with a score of 30 or greater
    if (score >= 30)
      scores.push_back(score_entry_t(xact, score));
  }

  // Sort the results by descending score, then look at every account ever
  // used among the top five.  Rank these by number of times used.  Lastly,
  // "decay" any latter accounts, so that we give recently used accounts a
  // slightly higher rating in case of a tie.

  std::stable_sort(scores.begin(), scores.end(), score_sorter());

  scorecard_t::iterator si        = scores.begin();
  int                   decay     = 0;
  xact_t *              best_xact = si != scores.end() ? (*si).first : NULL;
  account_use_map       account_usage;

  for (int i = 0; i < 5 && si != scores.end(); i++, si++) {
    DEBUG("lookup.account",
          "Payee: " << std::setw(5) << std::right << (*si).second <<
          " - " << (*si).first->payee);

    foreach (post_t * post, (*si).first->posts) {
      if (! post->has_flags(ITEM_TEMP | ITEM_GENERATED) &&
          post->account != ref_account &&
          ! post->account->has_flags(ACCOUNT_TEMP | ACCOUNT_GENERATED)) {
        account_use_map::iterator x = account_usage.find(post->account);
        if (x == account_usage.end())
          account_usage.insert(account_use_pair(post->account,
                                                ((*si).second - decay)));
        else
          (*x).second += ((*si).second - decay);
      }
      decay++;
    }
  }

  if (account_usage.size() > 0) {
#if DEBUG_ON
    if (SHOW_DEBUG("lookup.account")) {
      foreach (const account_use_pair& value, account_usage) {
        DEBUG("lookup.account",
              "Account: " << value.second << " - " << value.first->fullname());
      }
    }
#endif
    return std::pair<xact_t *, account_t *>
      (best_xact, (*std::max_element(account_usage.begin(), account_usage.end(),
                                     usage_sorter())).first);
  } else {
    return std::pair<xact_t *, account_t *>(best_xact, NULL);
  }
}

} // namespace ledger
