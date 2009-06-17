" Vim syntax file
" filetype: ledger
" Version: 0.1.0
" by Johann Klähn; Use according to the terms of the GPL>=2.
" by Stefan Karrmann; Use according to the terms of the GPL>=2.
" by Wolfgang Oertl; Use according to the terms of the GPL>=2.
" Revision history
"  2009-06-12  J. Klähn: Use all available columns for foldtext
"  2009-03-25  J. Klähn: Allow Metadata
"              in transactions and postings (Ledger 3.0)
"              Also fixed alignment for multi-byte-characters
"  2009-01-28  S.Karrmann: minor fixes
"  2009-01-27  third version by S.Karrmann.
"              better extraction of the amount of the posting
"              decimal separator can be one of '.' and ','.
"  2005-02-05  first version (partly copied from ledger.vim 0.0.1)
" vim:ts=2:sw=2:sts=2:foldmethod=marker

if version < 600
  syntax clear
elseif exists("b:current_sytax")
  finish
endif

" for debugging
syntax clear
 
" region: a transaction containing postings
syn region transNorm start=/^[[:digit:]~]/ skip=/^\s/ end=/^/
    \ fold keepend transparent contains=transDate, Metadata, Posting
syn match transDate /^\d\S\+/ contained
syn match Metadata /^\s\+;.*/ contained
syn match Comment /^;.*$/
" every space in an account name shall be surrounded by two non-spaces
" every account name ends with a tab, two spaces or the end of the line
syn match Account /^\s\+\zs\%(\S\|\S \S\)\+\ze\%([ ]\{2,}\|\t\s*\|\s*$\)/ contained
syn match Posting /^\s\+[^[:blank:];].*$/ contained transparent contains=Account


highlight default link transDate Question
highlight default link Metadata PreProc
highlight default link Comment Comment
highlight default link Account Identifier
 
" syncinc is easy: search for the first transaction.
syn sync clear
syn sync match ledgerSync grouphere transNorm "^\d"
 
let b:current_syntax = "ledger"
