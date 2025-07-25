"
" Functions to toggle format on save with Neoformat. This is off by default so
" I don't accidentally start formatting when rebasing (bad memories).

function s:format_on_save()
  if &filetype == 'fsharp' || &filetype == 'cs'
    return execute('lua vim.lsp.buf.format()')
  endif

  undojoin | Neoformat
endfunction

function s:fmt_on_save_enable()
  augroup aa_fmt
    autocmd!
    autocmd BufWritePre * call s:format_on_save()
  augroup END
endfunction

function s:fmt_on_save_disable()
  autocmd! aa_fmt
endfunction

command! FMTOnSaveEnable call s:fmt_on_save_enable()
command! FMTOnSaveDisable call s:fmt_on_save_disable()
