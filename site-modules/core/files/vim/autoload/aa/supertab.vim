function aa#supertab#expand()
    if exists('g:UltiSnipsEnableSnipMate')
        "
        " Try to expand snippet or go to the next tab stop
        "
        call UltiSnips#ExpandSnippetOrJump()
        if g:ulti_expand_or_jump_res != 0
            let g:ulti_expand_or_jump_res = 0
            return ""
        endif
    endif

    "
    " If popup is open cycle through the items this will most likely be
    " deoplete completion
    "
    if pumvisible()
        return "\<c-n>"
    endif

    if exists('g:loaded_emmet_vim')

        "
        " If there is an emmet expression expand it
        "
        if emmet#isExpandable()
            call emmet#expandAbbr(0,"")
            return "\<Right>"
        endif
    endif

    "
    " If all else fails just tab along
    "
    return "\<tab>"
endfunction
