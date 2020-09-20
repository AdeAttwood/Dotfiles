"
" Config for UltiSnips config
"

let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips"
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
let g:snips_author = "Ade Attwood"
let g:snips_email = "code@adeattwood.co.uk"

"
" Run super tab script on UltiSnipsExpandTrigger to enable ultisnips and emmet
" on tab press
"
au InsertEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=aa#supertab#expand()<cr>"
