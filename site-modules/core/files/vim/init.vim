" Ade Attwood <adeattwood.co.uk>

" Vimrc is ordered in the options style
" to view all the options run :options
" and get the docs on all the settings

"
" Important
"
"call pathogen#infect()
set nocompatible
filetype plugin on
set clipboard=unnamedplus
"set t_Co=256

"
" Set leader as space bar
"
let mapleader="\<Space>"


"
" Not sorted and new
"
set listchars=tab:▸\ ,extends:❯,precedes:❮
set pastetoggle=<F8>
set wildignore+=*/vendor/*,*/node_modules/*,*/runtime/*,*/public_html/*
set scrolloff=20
set signcolumn=yes

"
" Moving around, searching and pattern
"
set incsearch

"
" Tags
"

"
" Displaying text
"
set linebreak
set nowrap

call aa#functions#number_on(0)
cabb tognum call aa#functions#toggle_numbers()

"
" Highlighting and spelling
"
syntax on
set spelllang=en_gb
set cursorline
set nohlsearch


"
" Multiple windows
"
set laststatus=2


"
" Multiple tab pages
"

"
" Terminal
"

"
" Using the mouse
"
set mouse=a


"
" Printing
"

"
" Messages and info
"
set ruler
set showcmd

"
" Selecting text
"

"
" Editing text
"
set matchpairs+=<:>
set showmatch

"
" Tabs and indenting
"
set tabstop=4
set shiftwidth=4
set expandtab
set autoindent
filetype plugin indent on

"
" Folding
"
hi Folded ctermbg=0
set foldtext=aa#fold#fold_text()

command! -nargs=1 -complete=customlist,aa#fold#command_complete SetFold call aa#fold#set_fold(<f-args>)
call aa#fold#set_fold('marker')

"
" Diff mode
"

"
" Mapping
"
cabb Q q
cabb W w
cabb WQ wq
cabb Wq wq
cabb __html set ft=html
cabb __php set ft=php
cabb aa <esc>vi[:EasyAlign =<CR>
cabb ap <esc>vip:EasyAlign =<CR>
cabb evim e ~/.vimrc
cabb sortp <esc>vip:sort<CR>
imap <c-h> <esc>I
imap <c-l> <esc>A
inoremap <C-Del> X<Esc>lbce
inoremap ,, <esc>A,
inoremap ;; <esc>A;
inoremap jj <esc>:w<cr>
nmap <silent> <Leader>s ]sz=
nnoremap <leader><leader> <c-^>
nnoremap <leader>d :call pdv#DocumentWithSnip()<cr>
nnoremap <silent> <Down> :resize -5<cr>
nnoremap <silent> <Right> :vertical resize +5<cr>
nnoremap <silent> <Up> :resize +5<cr>
nnoremap <silent> <leader>f za<cr>
nnoremap <silent> <leader>q :q<cr>
noremap <leader>g :tab Gstatus<cr>
noremap <leader>p :CommandT<cr>
noremap <silent> <Left> :vertical resize -5<cr>

noremap <silent> [b :bp<cr>
noremap <silent> ]b :bn<cr>

noremap <silent> [q :cprevious<cr>
noremap <silent> ]q :cnext<cr>

noremap <silent> ]h :GitGutterNextHunk<cr>
noremap <silent> [h :GitGutterPrevHunk<cr>

noremap <F2> :Run make build<cr>
noremap <F3> :Run make test<cr>
noremap <F4> :Run make test FILE="%"<cr>

nnoremap <silent> <Leader>h :set nohlsearch<cr>:nohlsearch<cr>
nnoremap / :set hlsearch<cr>:nohlsearch<cr>/\v\c
nnoremap ? :set hlsearch<cr>:nohlsearch<cr>?
nnoremap * :set hlsearch<cr>:nohlsearch<cr>*

"
" Populate the command pallet with the `Ack` command followed by the visually
" selected text
"
vnoremap <leader>a <Esc>:Ack <c-r>=aa#buffer#GetVisualSelection()<cr>

"
" Reading and writing files
"
set autoread

"
" The swap file
"
set noswapfile


"
" Command line editing
" Executing external commands
" Running make and jumping to errors
" Language specific
"

"
" Multi-byte characters
"
set fileencoding=utf-8
set encoding=utf-8

"
" Various
"
autocmd BufNewFile,BufRead *.phtml   set ft=html


"
"  Plugins
"

" vim-airline
let g:airline#extensions#branch#enabled=1
let g:airline_theme='murmur'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#show_buffers = 1
let g:airline#extensions#tabline#show_splits = 0
let g:airline#extensions#tabline#show_tabs = 1

" pdv
let g:pdv_template_dir = $HOME . "/.config/nvim/UltiSnips/pdv_templates"

" indent-guides
let g:indent_guides_auto_colors = 1

"
" Text length markers
"
autocmd BufRead,TextChanged,TextChangedI *.pp,*.sh,*.bash,*.go,*.js,*.jsx,.ts,.tsx call aa#functions#show_column_if_line_too_long(80)
autocmd BufRead,TextChanged,TextChangedI *.php call aa#functions#show_column_if_line_too_long(120)

"
" Custom gitlab
"
let g:fugitive_gitlab_domains = ['https://git.zportal.co.uk']
