" Set color theme
let base16colorspace=256
let g:github_dark_sidebar = 0
colorscheme github_light

" Set tailing white space to errors
match ErrorMsg '\s\+$'

" Fix jsx end tag highlighting
" NOTE: This is only with vim regex highlighting, this dose not apply if you
" are using treesitter to highlighting
hi Tag        ctermfg=04
hi xmlTag     ctermfg=04
hi xmlTagName ctermfg=04
hi xmlEndTag  ctermfg=04

" Highlight comments to be italic this also targets the language client
" diagnostics.
highlight Comment cterm=italic gui=italic

" Remove underline from coursor line number
highlight CursorLineNr cterm=none

" Set the LSP diagnostics feedback to be italic
highlight DiagnosticError cterm=italic gui=italic
highlight DiagnosticWarn cterm=italic gui=italic
highlight DiagnosticInfo cterm=italic gui=italic
highlight DiagnosticHint cterm=italic gui=italic

" hi ActiveWindow ctermbg=00 | hi InactiveWindow ctermbg=22
" set winhighlight=Normal:ActiveWindow,NormalNC:InactiveWindow
" 
" au VimEnter,WinEnter,BufEnter,BufWinEnter,FocusGained,CompleteDone * hi ActiveWindow ctermbg=00 | hi InactiveWindow ctermbg=22
" au VimLeave,WinLeave,BufLeave,BufWinLeave,FocusLost * hi ActiveWindow ctermbg=22 | hi InactiveWindow ctermbg=22
