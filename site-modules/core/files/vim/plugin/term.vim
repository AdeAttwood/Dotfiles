"
" All of the good bits from `split_term`
" https://github.com/vimlab/split-term.vim/blob/master/plugin/split-term.vim
"

let s:force_vertical = exists('g:split_term_vertical') ? 1 : 0
let s:default_shell = exists('g:split_term_default_shell') ? g:split_term_default_shell : 0

au BufEnter * if &buftype == 'terminal' | :startinsert | endif

"
" Set split directions
"
set splitbelow
set splitright

" Opens up a new buffer, either vertical or horizontal. Count can be used to
" specify the number of visible columns or rows.
fun! s:openBuffer(count, vertical)
    let cmd = a:vertical ? 'vnew' : 'new'
    let cmd = a:count ? a:count . cmd : cmd
    exe cmd
endf

" Opens a new terminal buffer, but instead of doing so using 'enew' (same
" window), it uses :vnew and :new instead. Usually, I want to open a new
" terminal and not replace my current buffer.
fun! s:openSplitTerm(args, count, vertical)
    let direction = s:force_vertical ? 1 : a:vertical

    call s:openBuffer(a:count, direction)
    call s:openTerm(a:args)
endf

" Opens a new terminal buffer, but instead of doing so using split buffer, it
" uses :tabnew instead.
fun! s:openTabTerm(args)
    exe 'tabnew'
    call s:openTerm(a:args)
endf

" Open a new terminal in the active buffer, while defining default mappings
" for this plugin.
fun! s:openTerm(args)
    let prevShell = &shell
    if exists('g:split_term_default_shell')
        exe 'set shell =' . s:default_shell
    endif

    exe 'terminal' a:args


	"
	" Turn off line numbers in terminal mode
    "
    call aa#functions#number_off(1)

    exe 'startinsert'

    "
    " Add teminal mappings
    "
    tnoremap <buffer> <Esc> <C-\><C-n>
    tnoremap <buffer> <expr> <C-v> '<C-\><C-N>pi'
    tnoremap <buffer> <c-h> <C-\><C-n><C-w>h
    tnoremap <buffer> <c-j> <C-\><C-n><C-w>j
    tnoremap <buffer> <c-k> <C-\><C-n><C-w>k
    tnoremap <buffer> <c-l> <C-\><C-n><C-w>l

    if exists('g:split_term_default_shell')
        exe 'set shell =' . prevShell
    endif
endf

command! -count -nargs=* Term call  s:openTerm(<q-args>)
command! -count -nargs=* VTerm call  s:openSplitTerm(<q-args>, <count>, 1)
command! -count -nargs=* HTerm call s:openSplitTerm(<q-args>, <count>, 0)
command! -nargs=* TTerm call s:openTabTerm(<q-args>)
