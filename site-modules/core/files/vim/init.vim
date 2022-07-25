" Set leader as space bar
let mapleader="\<Space>"

" Use the system clipboard with yank and paste
set clipboard=unnamedplus

" Setup better searching
set ignorecase
set incsearch
set smartcase

" Stay 20 chars from the top and bottom of the buffer so I am always editing
" in context
set scrolloff=20

" Using the mouse
set mouse=a

" Tabs and indenting
set tabstop=4
set shiftwidth=4
set expandtab
set autoindent
filetype plugin indent on

" Highlight the current line where the cursor is on
set cursorline
let g:vim_json_conceal=0

" Relative line numbers, this set line numbers a swell so the current line is
" displayed correctly. If this is not set then the current line is always 0
set relativenumber
set number

set conceallevel=0

" Ensure the signcolumn is on to stop jumping for LSP diagnostics
set signcolumn=yes

" Disable swap files
set noswapfile

" Disable line wrapping
set nowrap

packadd! command-t

noremap <leader>p :CommandT<cr>
noremap <silent> <leader>q :bdelete<cr>
noremap <silent> <leader>fs :w<cr>

noremap <silent> <leader>o :Open    

noremap <silent> [b :bp<cr>
noremap <silent> ]b :bn<cr>

noremap <silent> [q :cprevious<cr>
noremap <silent> ]q :cnext<cr>

inoremap <M-;> <esc>A;

noremap <leader>; gcc
"vnoremap <leader>; gc

" Lint code with nvim-lint on save. This will lint all filetypes with cspell
" and then any other filetypes will be linted per the config.
au BufWritePost,BufReadPost <buffer> lua require('lint').try_lint('cspell')
au BufWritePost,BufReadPost <buffer> lua require('lint').try_lint()


inoremap jj <esc>:w<cr>
nnoremap <leader><tab> <c-^>

" Use the CommandTWildIgnore insted of the wildignore so I can still get
" completion for :e in folders like vendor or node_modules but, they will be
" ignored from command-t file searches.
let g:CommandTWildIgnore="*/node_modules/*,*/vendor/*,*/runtime/*,*/public_html/*,*/pack/*"

" Required for complietion with nvim-cmp
set completeopt=menu,menuone,noselect

" Format code on save
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup END
