" Vim Compiler File
" Compiler:	ledger
" by Johann Klähn; Use according to the terms of the GPL>=2.
" vim:ts=2:sw=2:sts=2:foldmethod=marker

if exists("current_compiler")
  finish
endif
let current_compiler = "ledger"

if exists(":CompilerSet") != 2
  command -nargs=* CompilerSet setlocal <args>
endif

" %-G throws away blank lines, everything else is assumed to be part of a
" multi-line error message.
CompilerSet errorformat=%-G,%EWhile\ parsing\ file\ \"%f\"\\,\ line\ %l:%.%#,%ZError:\ %m,%C%.%#

" unfortunately there is no 'check file' command, 
" so we will just use a query that returns no results. ever.
CompilerSet makeprg=ledger\ -f\ %\ reg\ not\ '.*'\ \>\ /dev/null

