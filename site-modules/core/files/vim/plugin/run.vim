"
" Runs a shell command in the neovim terminal. This will open up a buffered
" terminal in a split at the bottom
"
" a:command
"   The command you want the run in the terminal
"
function! s:aa_run(command)
    if strlen($TMUX) > 0
        if (&columns > 180)
            let l:split = '-h'
        else
            let l:split = '-v'
        endif

        execute 'silent !tmux split-window -p 50 ' . l:split . ' && tmux send-keys "' . a:command . ' && exit" C-m'
        return
    endif

    if has('nvim')
        if (&columns > 180)
            execute 'VTerm ' . a:command . ' && exit'
        else
            execute 'HTerm ' . a:command . ' && exit'
        endif
    endif
endfunction

command! -nargs=* -complete=shellcmd Run call s:aa_run(<q-args>)
