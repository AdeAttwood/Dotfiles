"
" Highlights the active window in neo Vim. `ActiveWindow` is only available
" in neovim so
"
if has('nvim')
    hi ActiveWindow ctermbg=00 | hi InactiveWindow ctermbg=22
    set winhighlight=Normal:ActiveWindow,NormalNC:InactiveWindow

    au VimEnter,WinEnter,BufEnter,BufWinEnter,FocusGained,CompleteDone * hi ActiveWindow ctermbg=00 | hi InactiveWindow ctermbg=22
    au VimLeave,WinLeave,BufLeave,BufWinLeave,FocusLost * hi ActiveWindow ctermbg=22 | hi InactiveWindow ctermbg=22
else
    hi Normal ctermbg=None
endif
