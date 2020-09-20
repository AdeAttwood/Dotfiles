" Maintainer:   Ade Attwood <code@adeattwood.co.uk>
" Last Change:  2018-07-15

" This is a copy of the default colour theme with some edits to
" to the diff view and spelling

" Set 'background' back to the default.
hi clear Normal
set bg&

" Remove all existing highlighting and set the defaults.
hi clear

" Load the syntax highlighting defaults, if it's enabled.
if exists("syntax_on")
  syntax reset
endif

" Add in colours for vim diff
highlight DiffAdd    cterm=bold ctermfg=7 ctermbg=28 gui=none guifg=bg guibg=Red
highlight DiffDelete cterm=bold ctermfg=7 ctermbg=9  gui=none guifg=bg guibg=Red
highlight DiffChange cterm=bold ctermfg=7 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffText   cterm=bold ctermfg=7 ctermbg=20 gui=none guifg=bg guibg=Red

" Change background colour of highlighted spell words
highlight SpellBad   cterm=bold ctermfg=15 ctermbg=9  gui=none guifg=bg guibg=Red

let colors_name = "my_default"
