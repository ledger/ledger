" Vim syntax file
" filetype: ledger
" Version: 0.0.4
" by Stefan Karrmann; Use according to the terms of the GPL>=2.
" by Wolfgang Oertl; Use according to the terms of the GPL>=2.
" Revision history
"  2009-01-28  S.Karrmann: minor fixes
"  2009-01-27  third version by S.Karrmann.
"              better extraction of the amount of the transaction
"              decimal separator can be one of '.' and ','.
"  2005-02-05  first version (partly copied from ledger.vim 0.0.1)
 
if version < 600
  syntax clear
elseif exists("b:current_sytax")
  finish
endif
 
" for debugging
syntax clear
 
" region: a normal transaction
syn region transNorm start=/^\d/ skip=/^\s/ end=/^/ fold keepend transparent contains=transDate
syn match transDate /^\d\S\+/ contained
syn match Comment /^;.*$/
" highlight default link transNorm Question
highlight default link Comment SpecialKey
highlight default link transDate Question
 
" folding: how to represent a transaction in one line.
function! LedgerFoldText()
   let line = strpart(getline(v:foldstart), 0, 99)
   " get the amount at the end of the second line
   let line2 = getline(v:foldstart+1)
   let lst = matchlist(line2,'\([0-9]\+\%([,.][0-9]\+\)\=\|[,.][0-9]\+\)\s*\%([A-Za-z$€¢]\+\s*\)\=\%(\s*;.*\)\=$')
   if (len(lst) == 0)
      let line2 = ""
   else
      let line2 = lst[1]
   endif
   let pad_len = 80 - strlen(line) - strlen(line2)
   if (pad_len < 0)
      pad_len = 0
   endif
   let pad = strpart("                                                                                                                      ", 0, pad_len)
   return line . pad . line2
endfunction
set foldtext=LedgerFoldText()
set foldmethod=syntax
 
" syncinc is easy: search for the first transaction.
syn sync clear
syn sync match ledgerSync grouphere transNorm "^\d"
 
let b:current_syntax = "ledger"
