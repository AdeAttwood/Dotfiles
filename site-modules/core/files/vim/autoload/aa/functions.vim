"
" Turn line numbers on
"
" This set all settings for relative number and enables git gutter
"
function! aa#functions#number_on(local)
    if a:local == 1
        setlocal relativenumber
        setlocal number
        setlocal signcolumn=yes
    else
        set relativenumber
        set number
        set signcolumn=yes
    endif

    if exists(':GitGutterEnable')
        GitGutterEnable
    endif

    let g:linenumber=1
endfunction

"
" Turn line numbers off
"
" Disables line numbers and git gutter
"
function! aa#functions#number_off(local)
    if a:local == 1
        setlocal norelativenumber
        setlocal nonumber
        setlocal signcolumn=no
    else
        set norelativenumber
        set nonumber
        set signcolumn=no
    endif


    if exists(':GitGutterDisable')
        GitGutterDisable
    endif

    let g:linenumber=0
endfunction

"
" Toggles line numbers and all of the relevant settings
"
function! aa#functions#toggle_numbers()
    if  g:linenumber
        call aa#functions#number_off(0)
    else
        call aa#functions#number_on(0)
    end
endfunction

"
" Display ruler if text is over a specified char length
"
" a:lengthLimit
"   The char length when to show the column ruler
"
function! aa#functions#show_column_if_line_too_long(lengthLimit)
    " See https://stackoverflow.com/questions/2075276/longest-line-in-vim#2982789
    let maxLineLength = max(map(getline(1,'$'), 'len(v:val)'))

    if maxLineLength > a:lengthLimit
        highlight ColorColumn ctermbg=18 guibg=18
        execute "set colorcolumn=" . join(range((a:lengthLimit + 1),999), ",")
    else
        set colorcolumn=""
    endif
endfunction
