"
" Add deoplete plugin
"
function s:deoplete_init()
    if has('nvim')
        packadd deoplete
        call deoplete#enable()

        call deoplete#custom#source('ultisnips', 'rank', 9999)
    endif
endfunction

"au User WincentHardDefer call s:deoplete_init()
