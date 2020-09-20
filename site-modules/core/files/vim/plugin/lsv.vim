function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm() :
            \"\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
nmap <silent> gd <Plug>(coc-definition)
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction

function! s:open_as_preview(callstr)
    " e.g. the string should look like: +call cursor(<line>,<col>) <filename>
    let m = matchlist(a:callstr, '^+call cursor(\(\d\+\),\s*\(\d\+\))\s\+\(.*\)')
    if len(m) < 4   " TODO: more robust error handling
        echohl WarningMsg | echom "ERROR: Invalid callstr format" | echohl None
        return
    endif
    let linenr = m[1]
    let filename = expand(m[3])
    call quickui#preview#open(filename, {
                \ 'cursor': linenr,
                \ 'number' : 1,
                \ 'persist': 0,
                \ })
endfunction

command! -nargs=0 PreviewDefinition :call CocActionAsync('jumpDefinition', ':OpenAsPreview')
command! -nargs=* OpenAsPreview :call s:open_as_preview("<args>")

autocmd CursorHold * silent call CocActionAsync('highlight')
