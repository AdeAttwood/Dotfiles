function! s:ScanFile()
    let n = 1
    let nmax = line('$')
    if line('$') > 500
        let nmax = 500
    endif
    while n < nmax
        if getline(n) =~ "^class [A-Z]"
            return 1
            break
        endif
        let n = n + 1
    endwhile
    return 0
endfunction

function! s:DetectPhpClass()
    if match(&filetype, '\v<php.class>') != -1
        return
    endif

    if s:ScanFile()
        set ft=php.class
        return
    endif
endfunction

"autocmd BufNewFile,BufRead *.php call s:DetectPhpClass()
