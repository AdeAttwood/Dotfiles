" Check if NERDTree is open or active
function! s:isNERDTreeOpen()
  return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
endfunction

" Call NERDTreeFind iff NERDTree is active, current window contains a modifiable
" file, and we're not in vimdiff
function! s:syncTree()
  if &modifiable && s:isNERDTreeOpen() && strlen(expand('%')) > 0 && !&diff
    NERDTreeFind
    wincmd p
  endif
endfunction

function! ToggleNerdTree()
    if s:isNERDTreeOpen()
        NERDTreeClose
        return
    endif

  set eventignore=BufEnter
  NERDTreeFind
  set eventignore=
endfunction


function s:nerdtree_init()
    nmap <silent> <Leader>o :call ToggleNerdTree()<CR>
    autocmd BufEnter * call s:syncTree()
    packadd nerdtree
endfunction


au User WincentSoftDefer call s:nerdtree_init()
