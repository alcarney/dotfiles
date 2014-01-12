" -----------------------------------------------------------------------------
"      Filename: vimrc
"      Author: Alex Carney
"      
"      Created: 12/01/14
"      Last Modified: 12/01/14
"
" -----------------------------------------------------------------------------

" Automatically reload a file if it has been modified externally
set autoread 

" Enable filetype plugins 
filetype plugin on 
filetype indent on 

" Set the leader character to ,
let mapleader = ','
let g:mapleader = ','

" Don't redraw while executing macros (should boost performance)
set lazyredraw

" ----------------------------------- Colours ---------------------------------

syntax enable                          " Turn on syntax highlighting

colorscheme slate                     

" ----------------------------------- Searching -------------------------------

set ignorecase                         " Ignore case when searching 
set smartcase                          " Apparently vim tries to be smart here
set hlsearch                           " Highlight matches
set incsearch                          " Cycle through matches in a loop

" ----------------------------------- Interface -------------------------------

set ruler                              " Show cursor position

" Make backspace behave
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Highlight matching brackets
set showmatch

" ----------------------------------- Leader shortcut commands  ---------------
nmap <leader>w :w<CR>                  " Quicksave

