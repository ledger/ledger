#include "times.h"

namespace ledger {

ptime now = boost::posix_time::second_clock::universal_time();
bool  day_before_month = false;

ptime parse_datetime(std::istream& in)
{
#if 1
  return parse_abs_datetime(in);
#else
  std::string word;

  if (! in.good() || in.eof())
    return ptime();

  in >> word;

  // Grammar
  //
  // datetime:      absdate [time]
  //              | reldate
  //              | datetime preposition
  //
  // reldate:       NOW | TODAY | YESTERDAY | TOMORROW
  //              | skip_or_quantity specifier
  //
  // skip_or_quantity: skip | quantity
  //
  // skip:          LAST | NEXT
  //
  // quantity:      INTEGER | CARDINAL
  //
  // specifier:     DAY | WEEK | MONTH | QUARTER | YEAR | DECADE
  //
  // preposition:   AGO | BACK
  //              | BEFORE reldate
  //              | SINCE/FROM reldate
  //              | UNTIL reldate
  //              | AFTER reldate

  if (std::isdigit(word[0])) {
    // This could be any of a variety of formats:
    //
    //   20070702 [TIME]
    //   22072007T171940
    //   22072007T171940-0700
    //   2007-07-02 [TIME]
    //   2007/07/02 [TIME]
    //   2007.07.02 [TIME]
    //   2007-Jul-22 [TIME]
    //   07-22-2007 [TIME]
    //   07-22-07 [TIME]
    //   07/22/2007 [TIME]
    //   07/22/2007 [TIME]
    //   07.22.2007 [TIME]
    //   07.22.07 [TIME]
    //   22-07-2007 [TIME]
    //   22-07-07 [TIME]
    //   22/07/2007 [TIME]
    //   22/07/07 [TIME]
    //   22.07.2007 [TIME]
    //   22.07.07 [TIME]
    //   22 Jul 2007 [TIME]
    //   22 July 2007 [TIME]
    //
    //   (NUMBER) (SPECIFIER)

  } else {
    // If there is no starting digit, then it could be any of these:
    //
    //   now
    //   today
    //   yesterday
    //   tomorrow
    //   (last|next) (week|month|quarter|year|decade)
    //   (one|two|three|four|five|six|seven|eight|nine|ten) SPECIFIER
    //   PREPOSITION DATE
    //
    // PREPOSITION = (from|after|before|since|until)
    // SPECIFIER   = (weeks?|months?|quarters?|years?|decades?) (ago|back)
    //
    // 
  }
#endif
}

ptime datetime_range_from_stream(std::istream& in)
{
}

}
