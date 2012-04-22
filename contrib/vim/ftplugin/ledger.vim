" Vim filetype plugin file
" filetype: ledger
" by Johann Klähn; Use according to the terms of the GPL>=2.
" vim:ts=2:sw=2:sts=2:foldmethod=marker

if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1

let b:undo_ftplugin = "setlocal ".
                    \ "foldmethod< foldtext< ".
                    \ "include< comments< commentstring< omnifunc< formatprg<"

" don't fill fold lines --> cleaner look
setl fillchars="fold: "
setl foldtext=LedgerFoldText()
setl foldmethod=syntax
setl include=^!include
setl comments=b:;
setl commentstring=;%s
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
  let g:ledger_detailed_first = 1
endif

" only display exact matches (no parent accounts etc.)
if !exists('g:ledger_exact_only')
  let g:ledger_exact_only = 0
endif

" display original text / account name as completion
if !exists('g:ledger_include_original')
  let g:ledger_include_original = 0
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
  let columns = s:get_columns()
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
                  \ s:get_columns()/filen)
  else
    let foldtext .= repeat(' ', s:get_columns())
  endif

  " we don't use slices[:5], because that messes up multibyte characters
  let foldtext = substitute(foldtext, '.\{'.columns.'}\zs.*$', '', '')

  return printf(fmt, foldtext, amount)
endfunction "}}}

function! LedgerComplete(findstart, base) "{{{1
  if a:findstart
    let lnum = line('.')
    let line = getline('.')
    let b:compl_context = ''
    if line =~ '^\s\+[^[:blank:];]' "{{{2 (account)
      " only allow completion when in or at end of account name
      if matchend(line, '^\s\+\%(\S \S\|\S\)\+') >= col('.') - 1
        " the start of the first non-blank character
        " (excluding virtual-transaction and 'cleared' marks)
        " is the beginning of the account name
        let b:compl_context = 'account'
        return matchend(line, '^\s\+[*!]\?\s*[\[(]\?')
      endif
    elseif line =~ '^\d' "{{{2 (description)
      let pre = matchend(line, '^\d\S\+\%(([^)]*)\|[*?!]\|\s\)\+')
      if pre < col('.') - 1
        let b:compl_context = 'description'
        return pre
      endif
    elseif line =~ '^$' "{{{2 (new line)
      let b:compl_context = 'new'
    endif "}}}
    return -1
  else
    if ! exists('b:compl_cache')
      let b:compl_cache = s:collect_completion_data()
      let b:compl_cache['#'] = changenr()
    endif
    let update_cache = 0

    let results = []
    if b:compl_context == 'account' "{{{2 (account)
      let hierarchy = split(a:base, ':')
      if a:base =~ ':$'
        call add(hierarchy, '')
      endif

      let results = LedgerFindInTree(b:compl_cache.accounts, hierarchy)
      let exacts = filter(copy(results), 'v:val[1]')

      if len(exacts) < 1
        " update cache if we have no exact matches
        let update_cache = 1
      endif

      if g:ledger_exact_only
        let results = exacts
      endif

      call map(results, 'v:val[0]')

      if g:ledger_detailed_first
        let results = reverse(sort(results, 's:sort_accounts_by_depth'))
      else
        let results = sort(results)
      endif
    elseif b:compl_context == 'description' "{{{2 (description)
      let results = s:filter_items(b:compl_cache.descriptions, a:base)

      if len(results) < 1
        let update_cache = 1
      endif
    elseif b:compl_context == 'new' "{{{2 (new line)
      return [strftime('%Y/%m/%d')]
    endif "}}}


    if g:ledger_include_original
      call insert(results, a:base)
    endif

    " no completion (apart from a:base) found. update cache if file has changed
    if update_cache && b:compl_cache['#'] != changenr()
      unlet b:compl_cache
      return LedgerComplete(a:findstart, a:base)
    else
      unlet! b:compl_context
      return results
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
  let exact = empty(nextlvls)
  for branch in branches
    call add(results, [branch, exact])
    if ! empty(nextlvls)
      for [result, exact] in LedgerFindInTree(a:tree[branch], nextlvls)
        call add(results, [branch.':'.result, exact])
      endfor
    endif
  endfor
  return results
endf "}}}

function! LedgerToggleTransactionState(lnum, ...)
  if a:0 == 1
    let chars = a:1
  else
    let chars = ' *'
  endif
  let trans = s:transaction.from_lnum(a:lnum)
  if empty(trans) || has_key(trans, 'expr')
    return
  endif

  let old = has_key(trans, 'state') ? trans['state'] : ' '
  let i = stridx(chars, old) + 1
  let new = chars[i >= len(chars) ? 0 : i]

  call trans.set_state(new)

  call setline(trans['head'], trans.format_head())
endf

function! LedgerSetTransactionState(lnum, char) "{{{1
  " modifies or sets the state of the transaction at the cursor,
  " removing the state alltogether if a:char is empty
  let trans = s:transaction.from_lnum(a:lnum)
  if empty(trans) || has_key(trans, 'expr')
    return
  endif

  call trans.set_state(a:char)

  call setline(trans['head'], trans.format_head())
endf "}}}

function! LedgerSetDate(lnum, type, ...) "{{{1
  let time = a:0 == 1 ? a:1 : localtime()
  let trans = s:transaction.from_lnum(a:lnum)
  if empty(trans) || has_key(trans, 'expr')
    return
  endif

  let formatted = strftime('%Y/%m/%d', time)
  if has_key(trans, 'date') && ! empty(trans['date'])
    let date = split(trans['date'], '=')
  else
    let date = [formatted]
  endif

  if a:type ==? 'actual'
    let date[0] = formatted
  elseif a:type ==? 'effective'
    if time < 0
      " remove effective date
      let date = [date[0]]
    else
      " set effective date
      if len(date) >= 2
        let date[1] = formatted
      else
        call add(date, formatted)
      endif
    endif
  endif

  let trans['date'] = join(date, '=')

  call setline(trans['head'], trans.format_head())
endf "}}}

function! s:collect_completion_data() "{{{1
  let transactions = s:get_transactions()
  let cache = {'descriptions': [], 'tags': {}, 'accounts': {}}
  let accounts = []
  for xact in transactions
    " collect descriptions
    if has_key(xact, 'description') && index(cache.descriptions, xact['description']) < 0
      call add(cache.descriptions, xact['description'])
    endif
    let [t, postings] = xact.parse_body()
    let tagdicts = [t]

    " collect account names
    for posting in postings
      if has_key(posting, 'tags')
        call add(tagdicts, posting.tags)
      endif
      " remove virtual-transaction-marks
      let name = substitute(posting.account, '\%(^\s*[\[(]\?\|[\])]\?\s*$\)', '', 'g')
      if index(accounts, name) < 0
        call add(accounts, name)
      endif
    endfor

    " collect tags
    for tags in tagdicts | for [tag, val] in items(tags)
      let values = get(cache.tags, tag, [])
      if index(values, val) < 0
        call add(values, val)
      endif
      let cache.tags[tag] = values
    endfor | endfor
  endfor

  for account in accounts
    let last = cache.accounts
    for part in split(account, ':')
      let last[part] = get(last, part, {})
      let last = last[part]
    endfor
  endfor

  return cache
endf "}}}

let s:transaction = {} "{{{1
function! s:transaction.new() dict
  return copy(s:transaction)
endf

function! s:transaction.from_lnum(lnum) dict "{{{2
  let [head, tail] = s:get_transaction_extents(a:lnum)
  if ! head
    return {}
  endif

  let trans = copy(s:transaction)
  let trans['head'] = head
  let trans['tail'] = tail

  " split off eventual comments at the end of line
  let line = split(getline(head), '\ze\s*\%(\t\|  \);', 1)
  if len(line) > 1
    let trans['appendix'] = join(line[1:], '')
  endif

  " parse rest of line
  " FIXME (minor): will not preserve spacing (see 'join(parts)')
  let parts = split(line[0], '\s\+')
  if parts[0] ==# '~'
    let trans['expr'] = join(parts[1:])
    return trans
  elseif parts[0] ==# '='
    let trans['auto'] = join(parts[1:])
    return trans
  elseif parts[0] !~ '^\d'
    " this case is avoided in s:get_transaction_extents(),
    " but we'll check anyway.
    return {}
  endif

  for part in parts
    if     ! has_key(trans, 'date')  && part =~ '^\d'
      let trans['date'] = part
    elseif ! has_key(trans, 'code')  && part =~ '^([^)]*)$'
      let trans['code'] = part[1:-2]
    elseif ! has_key(trans, 'state') && part =~ '^[[:punct:]]$'
      " the first character by itself is assumed to be the state of the transaction.
      let trans['state'] = part
    else
      " everything after date/code or state belongs to the description
      break
    endif
    call remove(parts, 0)
  endfor

  let trans['description'] = join(parts)
  return trans
endf "}}}

function! s:transaction.set_state(char) dict "{{{2
  if has_key(self, 'state') && a:char =~ '^\s*$'
    call remove(self, 'state')
  else
    let self['state'] = a:char
  endif
endf "}}}

function! s:transaction.parse_body(...) dict "{{{2
  if a:0 == 2
    let head = a:1
    let tail = a:2
  elseif a:0 == 0
    let head = self['head']
    let tail = self['tail']
  else
    throw "wrong number of arguments for parse_body()"
    return []
  endif

  if ! head || tail <= head
    return []
  endif

  let lnum = head
  let tags = {}
  let postings = []
  while lnum <= tail
    let line = split(getline(lnum), '\s*\%(\t\|  \);', 1)

    if line[0] =~ '^\s\+[^[:blank:];]'
      " posting
      let [state, rest] = matchlist(line[0], '^\s\+\([*!]\?\)\s*\(.*\)$')[1:2]
      if rest =~ '\t\|  '
        let [account, amount] = matchlist(rest, '^\(.\{-}\)\%(\t\|  \)\s*\(.\{-}\)\s*$')[1:2]
      else
        let amount = ''
        let account = matchstr(rest, '^\s*\zs.\{-}\ze\s*$')
      endif
      call add(postings, {'account': account, 'amount': amount, 'state': state})
    end

    " where are tags to be stored?
    if empty(postings)
      " they belong to the transaction
      let tag_container = tags
    else
      " they belong to last posting
      if ! has_key(postings[-1], 'tags')
        let postings[-1]['tags'] = {}
      endif
      let tag_container = postings[-1]['tags']
    endif

    let comment = join(line[1:], '  ;')
    if comment =~ '^\s*:'
      " tags without values
      for t in s:findall(comment, ':\zs[^:[:blank:]]\([^:]*[^:[:blank:]]\)\?\ze:')
        let tag_container[t] = ''
      endfor
    elseif comment =~ '^\s*[^:[:blank:]][^:]\+:'
      " tag with value
      let key = matchstr(comment, '^\s*\zs[^:]\+\ze:')
      if ! empty(key)
        let val = matchstr(comment, ':\s*\zs.*\ze\s*$')
        let tag_container[key] = val
      endif
    endif
    let lnum += 1
  endw
  return [tags, postings]
endf "}}}

function! s:transaction.format_head() dict "{{{2
  if has_key(self, 'expr')
    return '~ '.self['expr']
  elseif has_key(self, 'auto')
    return '= '.self['auto']
  endif

  let parts = []
  if has_key(self, 'date') | call add(parts, self['date']) | endif
  if has_key(self, 'code') | call add(parts, '('.self['code'].')') | endif
  if has_key(self, 'state') | call add(parts, self['state']) | endif
  if has_key(self, 'description') | call add(parts, self['description']) | endif

  let line = join(parts)
  if has_key(self, 'appendix') | let line .= self['appendix'] | endif

  return line
endf "}}}
"}}}

" Helper functions {{{1

function! s:get_transactions(...) "{{{2
  if a:0 == 2
    let lnum = a:1
    let end = a:2
  elseif a:0 == 0
    let lnum = 1
    let end = line('$')
  else
    throw "wrong number of arguments for get_transactions()"
    return []
  endif

  " safe view / position
  let view = winsaveview()
  let fe = &foldenable
  set nofoldenable

  let transactions = []
  call cursor(lnum, 0)
  while lnum && lnum <= end
    let trans = s:transaction.from_lnum(lnum)
    if ! empty(trans)
      call add(transactions, trans)
      call cursor(trans['tail'], 0)
    endif
    let lnum = search('^[~=[:digit:]]', 'cW')
  endw

  " restore view / position
  let &foldenable = fe
  call winrestview(view)

  return transactions
endf "}}}

function! s:get_transaction_extents(lnum) "{{{2
  if ! (indent(a:lnum) || getline(a:lnum) =~ '^[~=[:digit:]]')
    " only do something if lnum is in a transaction
    return [0, 0]
  endif

  " safe view / position
  let view = winsaveview()
  let fe = &foldenable
  set nofoldenable

  call cursor(a:lnum, 0)
  let head = search('^[~=[:digit:]]', 'bcnW')
  let tail = search('^[^;[:blank:]]\S\+', 'nW')
  let tail = tail > head ? tail - 1 : line('$')

  " restore view / position
  let &foldenable = fe
  call winrestview(view)

  return head ? [head, tail] : [0, 0]
endf "}}}

function! s:findall(text, rx) " {{{2
  " returns all the matches in a string,
  " there will be overlapping matches according to :help match()
  let matches = []

  while 1
    let m = matchstr(a:text, a:rx, 0, len(matches)+1)
    if empty(m)
      break
    endif

    call add(matches, m)
  endw

  return matches
endf "}}}

" return length of string with fix for multibyte characters
function! s:multibyte_strlen(text) "{{{2
   return strlen(substitute(a:text, ".", "x", "g"))
endfunction "}}}

" get # of visible/usable columns in current window
function! s:get_columns() " {{{2
  " As long as vim doesn't provide a command natively,
  " we have to compute the available columns.
  " see :help todo.txt -> /Add argument to winwidth()/

  let columns = (winwidth(0) == 0 ? 80 : winwidth(0)) - &foldcolumn
  if &number
    " line('w$') is the line number of the last line
    let columns -= max([len(line('w$'))+1, &numberwidth])
  endif

  " are there any signs/is the sign column displayed?
  redir => signs
  silent execute 'sign place buffer='.string(bufnr("%"))
  redir END
  if signs =~# 'id='
    let columns -= 2
  endif

  return columns
endf "}}}

" remove spaces at start and end of string
function! s:strip_spaces(text) "{{{2
  return matchstr(a:text, '^\s*\zs\S\%(.*\S\)\?\ze\s*$')
endf "}}}

" return only those items that start with a specified keyword
function! s:filter_items(list, keyword) "{{{2
  return filter(copy(a:list), 'v:val =~ ''^\V'.substitute(a:keyword, '\\', '\\\\', 'g').'''')
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
