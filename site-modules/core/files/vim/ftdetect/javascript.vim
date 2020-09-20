"
" Change filetype to javascript.jsx if a js file imports `React`
"
function! s:ScanFile()
    let n = 1
    let nmax = line('$')
    if line('$') > 500
        let nmax = 500
    endif
    while n < nmax
        if getline(n) =~# "\\v<React>"
            return 1
            break
        endif
        let n = n + 1
    endwhile
    return 0
endfunction

function! s:DetectJSX()
    if match(&filetype, '\v<jsx>') != -1
        return
    endif
    if s:ScanFile()
        set filetype=javascript.jsx
    endif
endfunction

autocmd BufNewFile,BufRead *.js call s:DetectJSX()
