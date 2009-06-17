" Vimwiki filetype plugin file
" filetype: ledger
" Version: 0.1.0
" by Johann Klähn; Use according to the terms of the GPL>=2.
" vim:ts=2:sw=2:sts=2:foldmethod=marker

if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1

let b:undo_ftplugin = "setlocal ".
                    \ "foldmethod< foldtext< ".
                    \ "include< comments< "

" don't fill fold lines --> cleaner look
setl fillchars="fold: "
setl foldtext=LedgerFoldText()
setl foldmethod=syntax
setl include=^!include
setl comments=b:;

" You can set a maximal number of columns the fold text (excluding amount)
" will use by overriding g:ledger_maxwidth in your .vimrc.
" When maxwidth is zero, the amount will be displayed at the far right side
" of the screen.
if !exists('g:ledger_maxwidth')
  let g:ledger_maxwidth = 0
endif

let s:rx_amount = '\('.
                \   '\%([0-9]\+\)'.
                \   '\%([,.][0-9]\+\)*'.
                \ '\|'.
                \   '[,.][0-9]\+'.
                \ '\)'.
                \ '\s*\%([[:alpha:]¢$€£]\+\s*\)\?'.
                \ '\%(\s*;.*\)\?$'

function! LedgerFoldText() "{{{1
  " find amount
  let amount = ""
  let lnum = v:foldstart
  while lnum <= v:foldend
    let line = getline(lnum)

    " Skip metadata/leading comment
    if line !~ '^\s\+;'
      " No comment, look for amount...
      let groups = matchlist(line, s:rx_amount)
      echomsg string(groups)
      if ! empty(groups)
        echomsg amount
        let amount = groups[1]
        break
      endif
    endif
    let lnum += 1
  endwhile

  let fmt = '%s %s '
  " strip whitespace at beginning and end of line
  let foldtext = substitute(getline(v:foldstart),
                          \ '\(^\s\+\|\s\+$\)', '', 'g')

  " number of columns foldtext can use
  let columns = s:get_columns(0)
  if g:ledger_maxwidth
    let columns = min([columns, g:ledger_maxwidth])
  endif
  let columns -= s:multibyte_strlen(printf(fmt, '', amount))

  " add spaces so the text is always long enough when we strip it
  " to a certain width (fake table)
  let foldtext .= repeat(' ', s:get_columns(0))
  " we don't use slices[:5], because that messes up multibyte characters
  let foldtext = substitute(foldtext, '.\{'.columns.'}\zs.*$', '', '')

  return printf(fmt, foldtext, amount)
endfunction "}}}

" Helper functions {{{1
function! s:multibyte_strlen(text) "{{{2
   return strlen(substitute(a:text, ".", "x", "g"))
endfunction "}}}

function! s:get_columns(win) "{{{2
  " As long as vim doesn't provide a command natively,
  " we have to compute the available columns.
  " see :help todo.txt -> /Add argument to winwidth()/
  " FIXME: Although this will propably never be used with debug mode enabled
  "        this should take the signs column into account (:help sign.txt)
  let columns = (winwidth(a:win) == 0 ? 80 : winwidth(a:win)) - &foldcolumn
  if &number
    " line('w$') is the line number of the last line
    let columns -= max([len(line('w$'))+1, &numberwidth])
  endif
  return columns
endfunction "}}}

