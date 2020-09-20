"
" Sets all the settings for different fold types
"
" a:type
"   The type of fold you want to set.
"   Valid options are 'marker', 'indent', 'diff', hunk and 'off'
"
function! aa#fold#set_fold(type)
    if a:type == "marker"
        setlocal foldenable
        setlocal foldmethod=marker
        setlocal foldlevel=0
        setlocal foldnestmax=99
    elseif a:type == "indent"
        setlocal foldenable
        setlocal foldmethod=indent
        setlocal foldlevel=1
        setlocal foldnestmax=2
    elseif a:type == "diff"
        setlocal foldenable
        setlocal nomodeline formatoptions-=croq formatoptions+=tl
        setlocal foldmethod=expr
        setlocal foldexpr=aa#fold#diff_fold()
        setlocal foldcolumn=3
    elseif a:type == "hunk"
        setlocal foldenable
        setlocal nomodeline formatoptions-=croq formatoptions+=tl
        setlocal foldmethod=expr
        setlocal foldexpr=aa#fold#diff_hunk_fold()
        setlocal foldcolumn=3
    elseif a:type == 'off'
        set nofoldenable
    else
        echoerr a:type . " is not a valid fold use 'off', 'marker' or 'indent'"
    endif
endfunction

"
" Get the competition list for `set_fold()`
"
function! aa#fold#command_complete(a, cm, cu)
    return ['off', 'marker', 'indent', 'diff', 'hunk']
endfunction

"
" Get the fold text for displaying neat folds
" See: https://dhruvasagar.com/2013/03/28/vim-better-foldtext
"
function! aa#fold#fold_text()
    let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
    let lines_count = v:foldend - v:foldstart + 1
    let lines_count_text = '| ' . printf("%10s", lines_count . ' linesss') . ' |'
    let foldchar = '-'
    let foldtextstart = strpart('+' . repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
    let foldtextend = lines_count_text . repeat(foldchar, 8)
    let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
    return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction

"
" function for the `foldexpr` to fold git diffs
"
" Inspired from https://github.com/sgeb/vim-diff-fold
"
function! aa#fold#diff_fold()
    let l:line=getline(v:lnum)

    if l:line =~# '^\(diff\|Index\)'
        return '>1'
    else
        return '='
    endif
endfunction

"
" function for the `foldexpr` to fold git diffs and hunks
"
" Inspired from https://github.com/sgeb/vim-diff-fold
"
function! aa#fold#diff_hunk_fold()
    let l:line=getline(v:lnum)

  if l:line =~# '^\(diff\|Index\)'
        return '>1'
    elseif l:line =~# '^\(@@\|\d\)'
        return '>2'
    elseif l:line =~# '^\*\*\* \d\+,\d\+ \*\*\*\*$'
        return '>2'
    elseif l:line =~# '^--- \d\+,\d\+ ----$'
        return '>2'
    else
        return '='
    endif
endfunction
