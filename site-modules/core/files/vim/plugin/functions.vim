"
" Trims all traling white space from every line in a file
"
function! s:aa_trim_white_space()
    %s/\s\+$//e
endfunction

command! -nargs=* -complete=file TrimWhiteSpace call s:aa_trim_white_space(<f-args>)

"
" Creates a new file or directory updateing the UI with and clearing the
" command t cache. This function uses the nerd tree functions to create files
" or directory. For more info on this you can open nerdtree and press `m` then
" `a` to add a node. If it is a file the file will be opened in a new buffer
"
" a:path
"   Path to file or directory. Dirs end in /
"
function! s:aa_new(path)
    call g:NERDTreePath.Create(a:path)

    call commandt#Flush()
    NERDTreeRefreshRoot

    if filereadable(expand(a:path))
       execute "e " . expand(a:path)
    endif
endfunction

command! -nargs=1 -complete=file New call s:aa_new(<f-args>)

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


"
" Git gutter alias commad commands to give them the same api as fugitive
"
command! GaddHunk :GitGutterStageHunk
command! GresetHunk :GitGutterUndoHunk

"
" Browse command to open urls in a browser. This is to help fugitive `Gbrowse`
" open urls in a browser rather that other programmes
"
function! s:browse(uri)
    silent exec "!brave-browser '".a:uri."'"
endfunction

command! -nargs=1 Browse call s:browse(<f-args>)
