" Vim filetype plugin file
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
                    \ "include< comments< omnifunc< formatprg<"

" don't fill fold lines --> cleaner look
setl fillchars="fold: "
setl foldtext=LedgerFoldText()
setl foldmethod=syntax
setl include=^!include
setl comments=b:;
setl omnifunc=LedgerComplete

" set location of ledger binary for checking and auto-formatting
if ! exists("g:ledger_bin") || empty(g:ledger_bin) || ! executable(split(g:ledger_bin, '\s')[0])
  if executable('ledger')
    let g:ledger_bin = 'ledger'
  else
    unlet g:ledger_bin
    echoerr "ledger command not found. Set g:ledger_bin or extend $PATH ".
          \ "to enable error checking and auto-formatting."
  endif
endif

if exists("g:ledger_bin")
  exe 'setl formatprg='.substitute(g:ledger_bin, ' ', '\\ ', 'g').'\ -f\ -\ print'
endif

" You can set a maximal number of columns the fold text (excluding amount)
" will use by overriding g:ledger_maxwidth in your .vimrc.
" When maxwidth is zero, the amount will be displayed at the far right side
" of the screen.
if !exists('g:ledger_maxwidth')
  let g:ledger_maxwidth = 0
endif

if !exists('g:ledger_fillstring')
  let g:ledger_fillstring = ' '
endif

" If enabled this will list the most detailed matches at the top {{{
" of the completion list.
" For example when you have some accounts like this:
"   A:Ba:Bu
"   A:Bu:Bu
" and you complete on A:B:B normal behaviour may be the following
"   A:B:B
"   A:Bu:Bu
"   A:Bu
"   A:Ba:Bu
"   A:Ba
"   A
" with this option turned on it will be
"   A:B:B
"   A:Bu:Bu
"   A:Ba:Bu
"   A:Bu
"   A:Ba
"   A
" }}}
if !exists('g:ledger_detailed_first')
  let g:ledger_detailed_first = 0
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
    if line !~ '^\%(\s\+;\|\d\)'
      " No comment, look for amount...
      let groups = matchlist(line, s:rx_amount)
      if ! empty(groups)
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
  if strlen(g:ledger_fillstring)
    " add extra spaces so fillstring aligns
    let filen = s:multibyte_strlen(g:ledger_fillstring)
    let folen = s:multibyte_strlen(foldtext)
    let foldtext .= repeat(' ', filen - (folen%filen))

    let foldtext .= repeat(g:ledger_fillstring,
                  \ s:get_columns(0)/filen)
  else
    let foldtext .= repeat(' ', s:get_columns(0))
  endif

  " we don't use slices[:5], because that messes up multibyte characters
  let foldtext = substitute(foldtext, '.\{'.columns.'}\zs.*$', '', '')

  return printf(fmt, foldtext, amount)
endfunction "}}}

function! LedgerComplete(findstart, base) "{{{1
  if a:findstart
    let lnum = line('.')
    let line = getline('.')
    let lastcol = col('.') - 2
    if line =~ '^\d' "{{{2 (date / payee / description)
      let b:compl_context = 'payee'
      return -1
    elseif line =~ '^\s\+;' "{{{2 (metadata / tags)
      let b:compl_context = 'meta-tag'
      let first_possible = matchend(line, '^\s\+;')

      " find first column of text to be replaced
      let firstcol = lastcol
      while firstcol >= 0
        if firstcol <= first_possible
          " Stop before the ';' don't ever include it
          let firstcol = first_possible
          break
        elseif line[firstcol] =~ ':'
          " Stop before first ':'
          let firstcol += 1
          break
        endif

        let firstcol -= 1
      endwhile

      " strip whitespace starting from firstcol
      let end_of_whitespace = matchend(line, '^\s\+', firstcol)
      if end_of_whitespace != -1
        let firstcol = end_of_whitespace
      endif

      return firstcol
    elseif line =~ '^\s\+' "{{{2 (account)
      let b:compl_context = 'account'
      if matchend(line, '^\s\+\%(\S \S\|\S\)\+') <= lastcol
        " only allow completion when in or at end of account name
        return -1
      endif
      " the start of the first non-blank character
      " (excluding virtual-transaction-marks)
      " is the beginning of the account name
      return matchend(line, '^\s\+[\[(]\?')
    else "}}}
      return -1
    endif
  else
    if b:compl_context == 'account' "{{{2 (account)
      unlet! b:compl_context
      let hierarchy = split(a:base, ':')
      if a:base =~ ':$'
        call add(hierarchy, '')
      endif

      let results = LedgerFindInTree(LedgerGetAccountHierarchy(), hierarchy)
      " sort by alphabet and reverse because it will get reversed one more time
      let results = reverse(sort(results))
      if g:ledger_detailed_first
        let results = sort(results, 's:sort_accounts_by_depth')
      endif
      call add(results, a:base)
      return reverse(results)
    elseif b:compl_context == 'meta-tag' "{{{2
      unlet! b:compl_context
      let results = [a:base]
      call extend(results, sort(s:filter_items(keys(LedgerGetTags()), a:base)))
      return results
    else "}}}
      unlet! b:compl_context
      return []
    endif
  endif
endf "}}}

function! LedgerFindInTree(tree, levels) "{{{1
  if empty(a:levels)
    return []
  endif
  let results = []
  let currentlvl = a:levels[0]
  let nextlvls = a:levels[1:]
  let branches = s:filter_items(keys(a:tree), currentlvl)
  for branch in branches
    call add(results, branch)
    if !empty(nextlvls)
      for result in LedgerFindInTree(a:tree[branch], nextlvls)
        call add(results, branch.':'.result)
      endfor
    endif
  endfor
  return results
endf "}}}

function! LedgerGetAccountHierarchy() "{{{1
  let hierarchy = {}
  let accounts = s:grep_buffer('^\s\+\zs[^[:blank:];]\%(\S \S\|\S\)\+\ze')
  for name in accounts
    " remove virtual-transaction-marks
    let name = substitute(name, '\%(^\s*[\[(]\?\|[\])]\?\s*$\)', '', 'g')
    let last = hierarchy
    for part in split(name, ':')
      let last[part] = get(last, part, {})
      let last = last[part]
    endfor
  endfor
  return hierarchy
endf "}}}

function! LedgerGetTags() "{{{1
  let alltags = {}
  let metalines = s:grep_buffer('^\s\+;\s*\zs.*$')
  for line in metalines
    " (spaces at beginning are stripped by matchstr!)
    if line[0] == ':'
      " multiple tags
      for val in split(line, ':')
        if val !~ '^\s*$'
          let name = s:strip_spaces(val)
          let alltags[name] = get(alltags, name, [])
        endif
      endfor
    elseif line =~ '^.*:.*$'
      " line with tag=value
      let name = s:strip_spaces(split(line, ':')[0])
      let val = s:strip_spaces(join(split(line, ':')[1:], ':'))
      let values = get(alltags, name, [])
      call add(values, val)
      let alltags[name] = values
    endif
  endfor
  return alltags
endf "}}}

" Helper functions {{{1

" return length of string with fix for multibyte characters
function! s:multibyte_strlen(text) "{{{2
   return strlen(substitute(a:text, ".", "x", "g"))
endfunction "}}}

" get # of visible/usable columns in current window
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

" remove spaces at start and end of string
function! s:strip_spaces(text) "{{{2
  return matchstr(a:text, '^\s*\zs\S\%(.*\S\)\?\ze\s*$')
endf "}}}

" return only those items that start with a specified keyword
function! s:filter_items(list, keyword) "{{{2
  return filter(a:list, 'v:val =~ ''^\V'.substitute(a:keyword, '\\', '\\\\', 'g').'''')
endf "}}}

" return all lines matching an expression, returning only the matched part
function! s:grep_buffer(expression) "{{{2
  let lines = map(getline(1, '$'), 'matchstr(v:val, '''.a:expression.''')')
  return filter(lines, 'v:val != ""')
endf "}}}

function! s:sort_accounts_by_depth(name1, name2) "{{{2
  let depth1 = s:count_expression(a:name1, ':')
  let depth2 = s:count_expression(a:name2, ':')
  return depth1 == depth2 ? 0 : depth1 > depth2 ? 1 : -1
endf "}}}

function! s:count_expression(text, expression) "{{{2
  return len(split(a:text, a:expression, 1))-1
endf "}}}
