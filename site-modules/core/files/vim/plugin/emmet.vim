"
" Init emmet on specified files
"
autocmd FileType html,css,scss,less,javascript.jsx,typescript.jsx call aa#emmet#init()

"let g:user_emmet_leader_key='<C-Z>'

"
" Make <c-e> expand emment expreson like sparkup did
"

"inoremap <silent> <c-e> <esc>:call emmet#expandAbbr(3,"")<cr>
