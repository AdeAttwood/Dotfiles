"
" Orignaly from google maktaba
" https://github.com/google/vim-maktaba/blob/master/autoload/maktaba/buffer.vim#L15
"
" Gets the text of the current or last visual selection.
" Useful for visual mode mappings.
function! aa#buffer#GetVisualSelection() abort
    let [l:lnum1, l:col1] = getpos("'<")[1:2]
    let [l:lnum2, l:col2] = getpos("'>")[1:2]
    " 'selection' is a rarely-used option for overriding whether the last
    " character is included in the selection. Bizarrely, it always affects the
    " last character even when selecting from the end backwards.
    if &selection !=# 'inclusive'
        let l:col2 -= 1
    endif
    let l:lines = getline(l:lnum1, l:lnum2)
    if !empty(l:lines)
        " If there is only 1 line, the part after the selection must be removed
        " first because `col2` is relative to the start of the line.
        let l:lines[-1] = l:lines[-1][: l:col2 - 1]
        let l:lines[0] = l:lines[0][l:col1 - 1 : ]
    endif
    return join(l:lines, "\n")
endfunction

"
" Mainly taken form maktab. This just uses the pure vim implemetaion to
" replace lines
"
function! aa#buffer#Overwrite(startline, endline, lines) abort
    " If lines already match, don't modify buffer.
    if getline(a:startline, a:endline) == a:lines
        return
    endif

    " Lines being replaced minus lines being inserted.
    let l:line_delta = len(a:lines) - (a:endline + 1 - a:startline)
    " If there's a surplus (more to replace than insert), delete the last n lines.
    if l:line_delta < 0
        let l:winview = winsaveview()
        let l:keep_end = a:endline - (-l:line_delta)
        execute string(l:keep_end + 1) . ',' . string(a:endline) . 'delete'
        " Special case: Move the cursor up to track buffer changes if necessary.
        " If we delete lines above the cursor, the cursor should NOT remain on the
        " same line number.
        if l:winview.lnum > a:endline
            let l:winview.lnum += l:line_delta
        endif
        call winrestview(l:winview)
    endif
    " If there's a deficit (more to insert than replace), append the last n lines.
    let l:lines = a:lines
    if l:line_delta > 0
        call append(a:endline, a:lines[-l:line_delta : ])
        let l:lines = l:lines[ : -l:line_delta - 1]
    endif
    call setline(a:startline, l:lines)
endfunction
