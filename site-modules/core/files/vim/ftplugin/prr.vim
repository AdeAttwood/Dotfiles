"
" function for the `foldexpr` to fold git diffs and hunks
"
" Inspired from https://github.com/sgeb/vim-diff-fold
"
function! s:fold_diff_hunk()
    let l:line=getline(v:lnum)

  if l:line =~# '^> \(diff\|Index\)'
        return '>1'
    elseif l:line =~# '^> \(@@\|\d\)'
        return '>2'
    elseif l:line =~# '^> \*\*\* \d\+,\d\+ \*\*\*\*$'
        return '>2'
    elseif l:line =~# '^> --- \d\+,\d\+ ----$'
        return '>2'
    else
        return '='
    endif
endfunction

setlocal foldenable
setlocal nomodeline formatoptions-=croq formatoptions+=tl
setlocal foldmethod=expr
setlocal foldexpr=s:fold_diff_hunk()
setlocal foldlevel=99

" Add "go to definition" mapping for prr lines. This will allow you to go to
" the line of code when reviewing a pull request. Seeing the code in full
" context is really helpful when reviewing.
noremap <silent> gd :PrrGoToLine<cr>
