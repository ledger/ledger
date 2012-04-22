" Vim filetype indent file
" filetype: ledger
" by Johann Klähn; Use according to the terms of the GPL>=2.
" vim:ts=2:sw=2:sts=2:foldmethod=marker

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setl autoindent
setl indentexpr=GetLedgerIndent()

if exists("*GetLedgerIndent")
  finish
endif

function GetLedgerIndent(...)
  " You can pass in a line number when calling this function manually.
  let lnum = a:0 > 0 ? a:1 : v:lnum
  " If this line is empty look at (the indentation of) the last line.
  " Note that inside of a transaction no blank lines are allowed.
  let line = getline(lnum)
  let prev = getline(lnum - 1)

  if line =~ '^\s\+\S'
    " Lines that already are indented (→postings, sub-directives) keep their indentation.
    return &sw
  elseif line =~ '^\s*$'
    " Current line is empty, try to guess its type based on the previous line.
    if prev =~ '^\([[:digit:]~=]\|\s\+\S\)'
      " This is very likely a posting or a sub-directive.
      " While lines following the start of a transaction are automatically
      " indented you will have to indent the first line following a
      " pre-declaration manually. This makes it easier to type long lists of
      " 'account' pre-declarations without sub-directives, for example.
      return &sw
    else
      return 0
    endif
  else
    " Everything else is not indented:
    " start of transactions, pre-declarations, apply/end-lines
    return 0
  endif
endf
