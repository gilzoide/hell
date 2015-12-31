" Vim syntax file
" Language:     hellfire
" Maintainer:	gilzoide <gilzoide@gmail.com>
" Last Change:  2015 May 9
"
" This vim syntax file was inspired by the SConscript syntax file (as it uses
" another programming language as a base, just like hell)
"

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Read the Lua syntax to start with
if version < 600
  so <sfile>:p:h/lua.vim
else
  runtime! syntax/lua.vim
  unlet b:current_syntax
endif

" hellfire extensions
syn keyword hellFireHandler	feedHellFire addHellBuild
syn keyword hellBuilder		gcc cpp copy hs java
syn keyword hellBuild		build install target exclusiveTarget
syn keyword hellMetatables	Builder
syn keyword hell		hell utils


" Default highlighting
if version >= 508 || !exists("did_hell_syntax_inits")
  if version < 508
    let did_hell_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink hellFireHandler	Special
  HiLink hellBuild		Function
  HiLink hellBuilder		Function
  HiLink hellMetatables		Type
  HiLink hell			Special
  delcommand HiLink
endif

let b:current_syntax = "hellfire"
" vim: ts=8
