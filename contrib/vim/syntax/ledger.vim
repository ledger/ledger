" Vim syntax file
" filetype: ledger
" by Johann Klähn; Use according to the terms of the GPL>=2.
" by Stefan Karrmann; Use according to the terms of the GPL>=2.
" by Wolfgang Oertl; Use according to the terms of the GPL>=2.
" vim:ts=2:sw=2:sts=2:foldmethod=marker

if version < 600
  syntax clear
elseif exists("b:current_sytax")
  finish
endif

" for debugging
syntax clear

" DATE[=EDATE] [*|!] [(CODE)] DESC <-- first line of transaction
"   ACCOUNT AMOUNT [; NOTE]  <-- posting

syn region ledgerTransaction start=/^[[:digit:]~]/ skip=/^\s/ end=/^/
    \ fold keepend transparent contains=ledgerTransactionDate,ledgerMetadata,ledgerPosting
syn match ledgerTransactionDate /^\d\S\+/ contained
syn match ledgerPosting /^\s\+[^[:blank:];][^;]*\ze\%($\|;\)/
    \ contained transparent contains=ledgerAccount,ledgerMetadata
" every space in an account name shall be surrounded by two non-spaces
" every account name ends with a tab, two spaces or the end of the line
syn match ledgerAccount /^\s\+\zs\%(\S\@<= \S\|\S\)\+\ze\%(  \|\t\|\s*$\)/ contained

syn match ledgerComment /^;.*$/
" comments at eol must be preceeded by at least 2 spaces / 1 tab
syn region ledgerMetadata start=/\%(  \|\t\|^\s\+\);/ skip=/^\s\+;/ end=/^/
    \ keepend contained contains=ledgerTag,ledgerTypedTag
syn match ledgerTag /:[^:]\+:/hs=s+1,he=e-1 contained
syn match ledgerTag /\%(\%(;\|^tag\)[^:]\+\)\@<=[^:]\+:\ze[^:]\+$/ contained
syn match ledgerTypedTag /\%(\%(;\|^tag\)[^:]\+\)\@<=[^:]\+::\ze[^:]\+$/ contained

syn region ledgerTagStack
    \ matchgroup=ledgerTagPush start=/^tag\>/
    \ matchgroup=ledgerTagPop end=/^pop\>/
    \ contains=ledgerTagHead,ledgerTagStack,ledgerTransaction,ledgerComment
syn match ledgerTagHead /\%(^tag\s\+\)\@<=\S.*$/ contained contains=ledgerTag transparent

highlight default link ledgerTransactionDate Constant
highlight default link ledgerMetadata Tag
highlight default link ledgerTypedTag Keyword
highlight default link ledgerTag Type
highlight default link ledgerTagPop Tag
highlight default link ledgerTagPush Tag
highlight default link ledgerAccount Identifier
 
" syncinc is easy: search for the first transaction.
syn sync clear
syn sync match ledgerSync grouphere ledgerTransaction "^[[:digit:]~]"
 
let b:current_syntax = "ledger"
