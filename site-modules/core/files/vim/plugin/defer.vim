"
" Defer command for starting work after vim has loaded
"
" This is biased from Greg Hurrell's Profiling and optimization screen cast in
" his implementation he is using the `CursorHold,CursorHoldI` auto command
" that didn't really work for me. This uses `timer_start` to set a timer when
" to start the work. This is then started on `VimEnter` when everything has
" loaded. There are two levels of defer one at `250ms` and `500ms` This is for
" calling functions that depend on something that has already been deferred.
"
" You can use this by adding a function to the user auto command `WincentSoftDefer`
"
"   autocmd User WincentSoftDefer call s:do_work()
"

function HardDefer(timer)
    augroup WincentIdleboot
        autocmd!
    augroup END

    doautocmd User WincentHardDefer
    autocmd! User WincentHardDefer
endfunction

function SoftDefer(timer)
    augroup WincentIdleboot
        autocmd!
    augroup END

    doautocmd User WincentSoftDefer
    autocmd! User WincentSoftDefer
endfunction

augroup WincentIdleboot
    autocmd!
    if has('vim_starting')
        autocmd VimEnter * call timer_start(250, 'SoftDefer')
        autocmd VimEnter * call timer_start(500, 'HardDefer')
    endif
augroup END
