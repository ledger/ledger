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
 
" region: a normal posting
syn region postNorm start=/^\d/ skip=/^\s/ end=/^/ fold keepend transparent contains=postDate,Metadata
syn match postDate /^\d\S\+/ contained
syn match Metadata /^\s\+;.*$/ contained
syn match Comment /^;.*$/

highlight default link Comment SpecialKey
highlight default link Metadata SpecialKey
highlight default link postDate Question
 
" folding: how to represent a posting in one line.
function! LedgerFoldText()
   let line = strpart(getline(v:foldstart), 0, 99)
   let amount = ""

   let delta = 1
   while (delta >= 1 && delta < (v:foldend-v:foldstart+1))
      let other_line = getline(v:foldstart+delta)
      
      if len(matchstr(other_line, '^\s\+;')) != 0
         " Skip metadata/leading comment
         let delta = delta + 1
      else
         " No comment, look for amount...
         let delta = -1
         let lst = matchlist(other_line,'\(\%([0-9]\+[,.]\)\=[0-9]\+\%([,.][0-9]\+\)\=\|[,.][0-9]\+\)\s*\%([A-Za-z$€¢]\+\s*\)\=\%(\s*;.*\)\=$')
         if (len(lst) != 0)
            let amount = lst[1]
         endif
      endif
   endwhile

   " Replace each char with x prior to counting
   " this fixes problems with multi byte characters
   let pad_len = 80 - strlen(substitute(line, ".", "x", "g")) - strlen(substitute(amount, ".", "x", "g"))
                    
   if (pad_len < 0)
      pad_len = 0
   endif
   let pad = strpart("                                                                                                                      ", 0, pad_len)
   return line . pad . amount
endfunction
set foldtext=LedgerFoldText()
set foldmethod=syntax
 
" syncinc is easy: search for the first posting.
syn sync clear
syn sync match ledgerSync grouphere postNorm "^\d"
 
let b:current_syntax = "ledger"
