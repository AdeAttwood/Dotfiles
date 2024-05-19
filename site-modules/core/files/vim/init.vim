" Set leader as space bar
let mapleader="\<Space>"
let maplocalleader="\<Space>"

" Use the system clipboard with yank and paste
set clipboard=unnamedplus

" Setup better searching
set ignorecase
set incsearch
set nohlsearch
set smartcase

" Stay 20 chars from the top and bottom of the buffer so I am always editing
" in context
set scrolloff=20

" Using the mouse
set mouse=a

" Ensure `split` is made below and `vsplit` are make to the right of the
" current window
set splitright
set splitbelow

" Tabs and indenting
set tabstop=2
set shiftwidth=2
set expandtab
set autoindent
filetype plugin indent on

" Highlight the current line where the cursor is on
set cursorline
let g:vim_json_conceal=0

set list listchars=tab:--▷,trail:•,precedes:«,extends:»

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

noremap <silent> <leader>q :bdelete<cr>
noremap <silent> <leader>fs :w ++p<cr>

noremap <silent> <leader>o :Open<cr>

noremap <silent> <leader><leader> za

noremap <silent> [b :bp<cr>
noremap <silent> ]b :bn<cr>

noremap <silent> [q :cprevious<cr>
noremap <silent> ]q :cnext<cr>

noremap <silent> [f zk
noremap <silent> ]f zj

" Add binding to "git open" to open the current file in the browser at the
" current line. For some reason `:GBrowse` will only open the file and not go
" to the line unless I use visual mode to select the line and then run
" `:GBrowse`
noremap <silent> go <c-v>:GBrowse<cr>

inoremap <M-;> <esc>A;

noremap <leader>; gcc
"vnoremap <leader>; gc

inoremap jj <esc>:w<cr>
nnoremap <leader><tab> <c-^>

let g:fsharp#lsp_auto_setup = 0


" Add mapping to "Run" tasks from the taskfile
"
" This needs to be added here so its defined before the Ferret plugin is
" loaded. Ferret adds a mapping to <leader>r only if one is not defined. If we
" do this in the taksfile plugin it will be too late and the Ferret mapping
" will only be added.
nmap <silent> <Leader>r :IvyTaskfile<cr>

" Use the CommandTWildIgnore insted of the wildignore so I can still get
" completion for :e in folders like vendor or node_modules but, they will be
" ignored from command-t file searches.
let g:CommandTWildIgnore="*/node_modules/*,*/vendor/*,*/runtime/*,*/public_html/*,*/pack/*"

let g:FerretExecutableArguments = {
  \   'rg': '--no-require-git --vimgrep --no-heading --no-config --max-columns 4096'
  \ }


let g:neoformat_enabled_cs = ['csharpier']
