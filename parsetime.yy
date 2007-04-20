%{
#define YYSTYPE struct ledger::intorchar

#include "times.h"
#include "FlexLexer.h"

static struct std::tm * timeval;

namespace {
  ledger::moment_t moment;

  struct time_to_leave : std::exception {};

  yyFlexLexer * lexer;

  inline void yyerror(const char *str) {
    throw new ledger::datetime_error(str);
  }

  inline int yylex(void) {
    return lexer->yylex();
  }

  int month_to_int(const std::string& name)
  {
    switch (std::toupper(name[0])) {
    case 'J':
      if (std::tolower(name[1]) == 'a')
	return 1;
      else if (std::tolower(name[2]) == 'n')
	return 6;
      else
	return 7;
    case 'F':
      return 2;
    case 'M':
      if (std::tolower(name[2]) == 'r')
	return 3;
      else
	return 5;
    case 'A':
      if (std::tolower(name[1]) == 'p')
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

  void set_hms(const ledger::intorchar& ampm,
	       const ledger::intorchar& hour,
	       const ledger::intorchar& min = ledger::intorchar(),
	       const ledger::intorchar& sec = ledger::intorchar())
  {
    if (! ampm.sval.empty() &&
	std::tolower(ampm.sval[0]) == 'a' && hour.ival == 12)
      timeval->tm_hour = 0;
    else if (! ampm.sval.empty() &&
	     std::tolower(ampm.sval[0]) == 'p' && hour.ival == 12)
      timeval->tm_hour = 12;
    else if (hour.ival < 0 || (ampm.sval.empty() && hour.ival > 23) ||
	     (! ampm.sval.empty() && hour.ival > 12))
      throw ledger::datetime_error("Hour out of range");
    else
      timeval->tm_hour += hour.ival;

    if (min.ival < -1 || min.ival > 59)
      throw ledger::datetime_error("Minute out of range");
    if (sec.ival < -1 || sec.ival > 59)
      throw ledger::datetime_error("Seconds out of range");

    timeval->tm_min  = min.ival == -1 ? 0 : min.ival;
    timeval->tm_sec  = sec.ival == -1 ? 0 : sec.ival;
  }
}

%}

%token TOK_FOURNUM
%token TOK_TWONUM
%token TOK_ONENUM
%token TOK_MONTH
%token TOK_AMPM

%token TOK_SPACE

%left '/' '-' '.' 'T'

%%

input: date
{
  throw time_to_leave();
};

date: absdate opttime
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
  isodate
| year '/' morday '/' morday			{ set_mdy($3, $5, $1); }
| year '-' morday '-' morday			{ set_mdy($3, $5, $1); }
| year '.' morday '.' morday			{ set_mdy($3, $5, $1); }
| morday '/' morday '/' year			{ set_mdy($1, $3, $5); }
| morday '-' morday '-' year			{ set_mdy($1, $3, $5); }
| morday '.' morday '.' year			{ set_mdy($1, $3, $5); }
| morday '.' morday				{ set_mdy($1, $3); }
| morday '/' morday				{ set_mdy($1, $3); }
| morday '-' morday				{ set_mdy($1, $3); }
| morday '/' morday '/' TOK_TWONUM		{ set_mdy($1, $3, $5, true); }
| morday '-' morday '-' TOK_TWONUM		{ set_mdy($1, $3, $5, true); }
| morday '.' morday '.' TOK_TWONUM		{ set_mdy($1, $3, $5, true); }
| year TOK_SPACE TOK_MONTH TOK_SPACE morday	{ set_mdy($3, $5, $1); }
| morday TOK_SPACE TOK_MONTH TOK_SPACE year	{ set_mdy($3, $1, $5); }
| TOK_MONTH TOK_SPACE morday			{ set_mdy($1, $3); }
| morday TOK_SPACE TOK_MONTH			{ set_mdy($3, $1); }
| year '-' TOK_MONTH '-' morday			{ set_mdy($3, $5, $1); }
| morday '-' TOK_MONTH '-' year			{ set_mdy($3, $1, $5); }
| TOK_MONTH '-' morday				{ set_mdy($1, $3); }
| morday '-' TOK_MONTH				{ set_mdy($3, $1); }
| TOK_MONTH TOK_SPACE morday ',' TOK_SPACE year { set_mdy($1, $3, $6); }
;

opttime:  /* epsilon */ | TOK_SPACE time ;

time:
  onetwo optspace TOK_AMPM {
    if (std::tolower($3.sval[0]) == 'p')
      timeval->tm_hour = 12;
    else
      timeval->tm_hour = 0;

    set_hms($3, $1);
  }
|
  onetwo ':' TOK_TWONUM optampm {
    set_hms($4, $1, $3);
  }
|
  onetwo ':' TOK_TWONUM ':' TOK_TWONUM optampm {
    set_hms($6, $1, $3, $5);
  }
;

onetwo: TOK_ONENUM { $$ = $1; } | TOK_TWONUM { $$ = $1; } ;

optspace: /* epsilon */ | TOK_SPACE ;

optampm: /* epsilon */ |
  optspace TOK_AMPM {
    if (std::tolower($2.sval[0]) == 'p')
      timeval->tm_hour = 12;
    else
      timeval->tm_hour = 0;
    $$ = $2;
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

ledger::moment_t parse_abs_datetime(std::istream& input)
{
  lexer = new yyFlexLexer(&input);

  struct std::tm temp;
  std::memset(&temp, 0, sizeof(struct std::tm));
  temp.tm_year = 2002 - 1900;
  temp.tm_gmtoff = -1;

  timeval = &temp;

  // jww (2007-04-19): Catch any boost errors thrown from here and
  // push them onto the new error stack scheme.
  try {
    if (yyparse() == 0) {
      delete lexer;
      return moment;
    }
  }
  catch (const time_to_leave&) {
    delete lexer;
    return moment;
  }
  catch (ledger::datetime_error *) {
    delete lexer;
    throw;
  }
  catch (...) {
    delete lexer;
    throw new ledger::datetime_error("Failed to parse date/time");
  }
  delete lexer;
  throw new ledger::datetime_error("Failed to parse date/time");
} 

#ifdef MAIN

namespace ledger {
  bool day_before_month = false;
}

int main()
{
  yydebug = 1;
  std::cout << parse_abs_datetime(std::cin) << std::endl;
  return 0;
}

#endif // MAIN
