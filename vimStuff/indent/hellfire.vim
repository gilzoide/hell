" Vim indent file
" Language:     hellfire
" Maintainer:	gilzoide <gilzoide@gmail.com>
" Last Change:  2015 May 9

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

" Just use Lua's indentation, taken from 'indent/lua.vim' =]
setlocal indentexpr=GetLuaIndent()
setlocal indentkeys+=0=end,0=until
setlocal autoindent

" vim: ts=8
