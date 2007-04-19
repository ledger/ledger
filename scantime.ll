%option c++ 8bit

%{
#define YYSTYPE struct ledger::intorchar

extern int yywrap();

#include "times.h"
#include "parsetime.h"

extern YYSTYPE yylval;
%}

shortmon (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)
longmon  (January|February|March|April|May|June|July|August|September|October|November|December)
ampm     (AM|PM|am|pm|A.M.|P.M.|a.m.|p.m.|[AP]|[ap])

%%

[ \t]  return TOK_SPACE;
[\r\n] ;

[0-9]{4}   yylval = ledger::intorchar(std::atoi(yytext)); return TOK_FOURNUM;
[0-9]{2}   yylval = ledger::intorchar(std::atoi(yytext)); return TOK_TWONUM;
[0-9]{1}   yylval = ledger::intorchar(std::atoi(yytext)); return TOK_ONENUM;

{shortmon} yylval = ledger::intorchar(yytext); return TOK_MONTH;
{longmon}  yylval = ledger::intorchar(yytext); return TOK_MONTH;

{ampm}	   yylval = ledger::intorchar(yytext); return TOK_AMPM;

.  return (int) yytext[0];
