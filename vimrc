" -----------------------------------------------------------------------------
"      Filename: vimrc
"      Author: Alex Carney
"      
"      Created: 12/01/14
"      Last Modified: 07/02/14
"
" -----------------------------------------------------------------------------

" Automatically reload a file if it has been modified externally
set autoread
set nocompatible

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set the leader character to ,
let mapleader = ','
set tm=500

" Make backspace behave
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

let g:mapleader = ','

" Don't redraw while executing macros (should boost performance)
set lazyredraw

" ----------------------------------- Colours ---------------------------------

syntax enable                          " Turn on syntax highlighting

colorscheme slate

" ----------------------------------- Tab Settings ----------------------------

set expandtab                          " Insert spaces instead of tab characters
set tabstop=4                          " Tabs are equal to 4 spaces
set softtabstop=4                      " Allows navigation as if the tab characters are there
set autoindent                         " Automatically match indentation of previous line
set smartindent                        " Automatically indent new code blocks
set wrap                               " Wrap long lines

" This let's you indent/unindent blocks of code using the </> keys repectively
" while in visual mode
set shiftwidth=4

" ----------------------------------- Searching -------------------------------

set ignorecase                         " Ignore case when searching
set smartcase                          " Apparently vim tries to be smart here
set hlsearch                           " Highlight matches
set incsearch                          " Cycle through matches in a loop

" ----------------------------------- Interface -------------------------------

set number                             " Enable line numbers
set mouse=a                            " Enable mouse support
set ruler                              " Show cursor position
set showmatch                          " Highlight matching brackets
set noerrorbells                       " Disable annoying beeps
set novisualbell


" Show trailing whitespace chars
set listchars=nbsp:_,trail:.
set list

" Highlight a column when I go over a certain width
highlight ColorColumn ctermbg=cyan
call matchadd('ColorColumn', '\%126v', 100)

" Make the command key ; instead of :
nnoremap ; :
nnoremap : ;

" Make backspace behave
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Statusline
set laststatus=2
set statusline=\ %t%m%r%h\ %w\ \ \ Line:\ %l\ Col:\ %c

" ----------------------------------- Leader shortcut commands  ---------------
nmap <leader>w ;w<CR>                  " Quicksave
nmap <leader>r ;so /home/alex/.vimrc<CR> " Reload vimrc 
map <silent> <leader><CR> :noh<CR>     " Clear search match highlights

" --------------------------------- Vundle ----------------------------
"filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Auto update yourself
Bundle 'gmarik/vundle'

" Plugins

" Filetree browser
Bundle 'scrooloose/nerdtree'

" Git Integration
Bundle 'tpope/vim-fugitive'

" C++ Completion
Bundle 'omnicppcomplete'

" Project explorer
Bundle 'project'

" Surround
Bundle 'tpope/vim-surround'

" Taglist - Source Code Browser
Bundle 'taglist'

" Snippets
Bundle 'SerVer/ultisnips'

" ------------------------------- Plugin Config -----------------------

" ------------------------------ NERDTree File Browser ----------------
map <C-n> :NERDTreeToggle<CR>                       " Ctrl-n Toggle file browser

" Automatically start NERDTree if no file is specified
"autocmd vimenter * if !argc() | NERDTree | endif

" Close vim if the only window left open is NERDTRee
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
