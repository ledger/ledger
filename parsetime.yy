%{
#define YYSTYPE struct ledger::intorchar

#include "times.h"
#include "FlexLexer.h"

static struct std::tm * timeval;

namespace {
  boost::posix_time::ptime moment;

  yyFlexLexer * lexer;

  inline void yyerror(const char *str) {
    throw new ledger::datetime_error(str);
  }

  inline int yylex(void) {
    return lexer->yylex();
  }

  int month_to_int(char * name)
  {
    switch (std::toupper(name[0])) {
    case 'J':
      if (name[1] == std::tolower('a'))
	return 1;
      else if (name[2] == std::tolower('n'))
	return 6;
      else
	return 7;
    case 'F':
      return 2;
    case 'M':
      if (name[2] == std::tolower('r'))
	return 3;
      else
	return 5;
    case 'A':
      if (name[1] == std::tolower('p'))
	return 4;
      else
	return 8;
    case 'S':
      return 9;
    case 'O':
      return 10;
    case 'N':
      return 11;
    case 'D':
      return 12;
    default:
      std::cerr << "What?? (" << name << ")" << std::endl;
      assert(0);
      return -1;
    }
  }

  void set_mdy(const ledger::intorchar& month,
	       const ledger::intorchar& day,
	       const ledger::intorchar& year = ledger::intorchar(),
	       bool shortyear = false)
  {
    if (ledger::day_before_month) {
      timeval->tm_mon  = (day.ival == -1 ?
			  month_to_int(day.sval) : day.ival) - 1;
      timeval->tm_mday = month.ival;
    } else {
      timeval->tm_mon  = (month.ival == -1 ?
			  month_to_int(month.sval) : month.ival) - 1;
      timeval->tm_mday = day.ival;
    }

    if (year.ival != -1)
      timeval->tm_year = (shortyear ?
			  (year.ival < 70 ? year.ival + 100 : year.ival) :
			  year.ival - 1900);
  }
}

%}

%token TOK_FOURNUM
%token TOK_TWONUM
%token TOK_ONENUM
%token TOK_MONTH

%token TOK_SPACE

%left '/' '-' '.' 'T'

%%

input: date optspace ;

optspace: /* epsilon */ | TOK_SPACE ;

date:
  absdate opttime
{
  if (timeval->tm_gmtoff != -1) {
    boost::posix_time::ptime::time_duration_type offset;
    offset = boost::posix_time::seconds(timeval->tm_gmtoff);
    moment = boost::posix_time::from_time_t(timegm(timeval)) - offset;
  } else {
    moment = boost::posix_time::ptime_from_tm(*timeval);
  }
};

absdate:
  year '/' morday '/' morday {
    set_mdy($3, $5, $1);
  }
|
  year '-' morday '-' morday {
    set_mdy($3, $5, $1);
  }
|
  year '.' morday '.' morday {
    set_mdy($3, $5, $1);
  }
|
  morday '/' morday '/' year {
    set_mdy($1, $3, $5);
  }
|
  morday '-' morday '-' year {
    set_mdy($1, $3, $5);
  }
|
  morday '.' morday '.' year {
    set_mdy($1, $3, $5);
  }
|
  morday '.' morday {
    set_mdy($1, $3);
  }
|
  morday '/' morday {
    set_mdy($1, $3);
  }
|
  morday '-' morday {
    set_mdy($1, $3);
  }
|
  morday '/' morday '/' TOK_TWONUM {
    set_mdy($1, $3, $5, true);
  }
|
  morday '-' morday '-' TOK_TWONUM {
    set_mdy($1, $3, $5, true);
  }
|
  morday '.' morday '.' TOK_TWONUM {
    set_mdy($1, $3, $5, true);
  }
|
  isodate
|
  year TOK_SPACE TOK_MONTH TOK_SPACE morday {
    set_mdy($3, $5, $1);
  }
|
  morday TOK_SPACE TOK_MONTH TOK_SPACE year {
    set_mdy($3, $1, $5);
  }
|
  TOK_MONTH TOK_SPACE morday {
    set_mdy($1, $3);
  }
|
  morday TOK_SPACE TOK_MONTH {
    set_mdy($3, $1);
  }
|
  year '-' TOK_MONTH '-' morday {
    set_mdy($3, $5, $1);
  }
|
  morday '-' TOK_MONTH '-' year {
    set_mdy($3, $1, $5);
  }
|
  TOK_MONTH '-' morday {
    set_mdy($1, $3);
  }
|
  morday '-' TOK_MONTH {
    set_mdy($3, $1);
  }
|
  TOK_MONTH TOK_SPACE morday ',' TOK_SPACE year {
    set_mdy($1, $3, $6);
  }
;

opttime: /* epsilon */ |
  TOK_SPACE TOK_TWONUM ':' TOK_TWONUM ':' TOK_TWONUM
{
  timeval->tm_hour = $2.ival;
  timeval->tm_min  = $4.ival;
  timeval->tm_sec  = $6.ival;
};

isodate:
  year TOK_FOURNUM optisotime
{
  timeval->tm_year = $1.ival - 1900;
  timeval->tm_mon  = $2.ival / 100 - 1;
  timeval->tm_mday = $3.ival % 100;
};

optisotime: /* epsilon */ |
  'T' TOK_FOURNUM TOK_TWONUM optisozone
{
  timeval->tm_hour = $2.ival / 100;
  timeval->tm_min  = $2.ival % 100;
  timeval->tm_sec  = $3.ival;
};

optisozone: /* epsilon */ |
  '-' TOK_FOURNUM {
    timeval->tm_gmtoff = - (($2.ival / 100) * 3600 + ($2.ival % 100) * 60);
  }
| '+' TOK_FOURNUM {
    timeval->tm_gmtoff = (($2.ival / 100) * 3600 + ($2.ival % 100) * 60);
  };

year:  TOK_FOURNUM { $$ = $1; };

morday:
  TOK_TWONUM { $$ = $1; }
| TOK_ONENUM { $$ = $1; };

%%

int yywrap()
{
  return 1;
}

boost::posix_time::ptime parse_abs_datetime(std::istream& input)
{
  lexer = new yyFlexLexer(&input);

  struct std::tm temp;
  std::memset(&temp, 0, sizeof(struct std::tm));
  temp.tm_year = 2002 - 1900;
  temp.tm_gmtoff = -1;

  timeval = &temp;
  //yydebug = 1;

  // jww (2007-04-19): Catch any boost errors thrown from here and
  // push them onto the new error stack scheme.
  try {
    if (yyparse() == 0)
      return moment;
  }
  catch (ledger::datetime_error *) {
    throw;
  }
  catch (...) {
    throw new ledger::datetime_error("Failed to parse date/time");
  }
  throw new ledger::datetime_error("Failed to parse date/time");
} 

#ifdef MAIN

namespace ledger {
  bool day_before_month = false;
}

int main()
{
  std::cout << parse_abs_datetime(std::cin) << std::endl;
  return 0;
}

#endif // MAIN
