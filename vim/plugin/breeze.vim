" breeze.vim - Vim integration for Breeze (Common Lisp tooling)
" Maintainer: Francis St-Amour

" <sfile> = path of this script file
" :p     = expand to full absolute path
" :h     = head (strip last component); applied 7 times to go from
"          .../vim/pack/breeze/start/breeze/plugin/breeze.vim -> repo root
let g:breeze_root_dir = expand('<sfile>:p:h:h:h:h:h:h:h')

if exists('g:loaded_breeze')
  finish
endif
let g:loaded_breeze = 1

" Initialize Breeze: load the Breeze system into the running Lisp listener.
command! BreezeInit call breeze#init()

