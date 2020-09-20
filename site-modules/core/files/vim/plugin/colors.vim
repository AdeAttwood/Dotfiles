"
" Set color theme
"
let base16colorspace=256
colorscheme base16-eighties

"
" Set tailing white space to errors
"
match ErrorMsg '\s\+$'

"
" Fix jsx end tag highlighting
"
hi Tag        ctermfg=04
hi xmlTag     ctermfg=04
hi xmlTagName ctermfg=04
hi xmlEndTag  ctermfg=04

"
" Highlight comments to be italic this also targets the language client
" diagnostics.
"
highlight Comment cterm=italic gui=italic

"
" Make coc diagnostics italic
"
highlight CocErrorSign cterm=italic gui=italic ctermfg=9 guifg=#ff0000
highlight CocWarningSign cterm=italic gui=italic ctermfg=130 guifg=#ff922b
highlight CocInfoSign cterm=italic gui=italic ctermfg=11 guifg=#fab005
highlight CocHintSign cterm=italic gui=italic ctermfg=12 guifg=#15aabf
