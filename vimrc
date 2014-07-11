" -----------------------------------------------------------------------------
"      Filename: vimrc
"      Author: Alex Carney
"
"      Created: 12/01/14
"      Last Modified: 10/07/14
"
" -----------------------------------------------------------------------------

" Automatically reload a file if it has been modified externally
set nocompatible
set autoread

" --------------------------------- Vundle ----------------------------
"filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Auto update yourself
Plugin 'gmarik/vundle'

" Plugins

" Syntax checker
Plugin 'scrooloose/syntastic'

" Git Integration
Plugin 'tpope/vim-fugitive'

" Better Snippets
Plugin 'SirVer/ultisnips'

" Surround
Plugin 'tpope/vim-surround'

" Tagbar
Plugin 'majutsushi/tagbar'

" Vim AutoClose
Plugin 'Townk/vim-autoclose'

" Easier commenting of lines
Plugin 'tpope/vim-commentary'

" Easier file browsing
Plugin 'tpope/vim-vinegar'

" Display VCS info
Plugin 'mhinz/vim-signify'

" Airline
Plugin 'bling/vim-airline'
" Plugins to install /investigate
" vim-signify => highlight changes to a file based on VCS info
" vim-scripts/a.vim => quickly and easily switch between/open header + source
" files


call vundle#end()

" Enable filetype plugins"
filetype plugin indent on

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

colorscheme darkblue

" ----------------------------------- Tab Settings ----------------------------

set expandtab                          " Insert spaces instead of tab characters
set tabstop=4                          " Tabs are equal to 4 spaces
set softtabstop=4                      " Allows navigation as if the tab characters are there
set autoindent                         " Automatically match indentation of previous line
set smartindent                        " Automatically indent new code blocks
set nowrap                             " Don't wrap long lines

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
set visualbell
set scrolloff=5                        " Keep more of the file visible while scrolling

" Markdown syntax
au BufRead,BufNewFile *.md set filetype=markdown

" Show trailing whitespace chars
set listchars=tab:>-,trail:.,eol:$
nmap <silent> <leader>t ;set list! <CR>

" Highlight a column when I go over a certain width
highlight ColorColumn ctermbg=cyan
call matchadd('ColorColumn', '\%126v', 100)

" Make the command key ; instead of :
nnoremap ; :
nnoremap : ;

" Be mean and force myself to use hjkl
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" Easier split navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Easier tab navigation
noremap <S-l> gt
noremap <S-h> gT

" Make backspace behave
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Statusline
set laststatus=2
set statusline=\ %t%m%y%r%h\ %w\ \ \ Line:\ %l\ Col:\ %c
hi statusline ctermfg=16 ctermbg=231

" Highlight current line
hi CursorLine ctermbg=16 cterm=NONE
set cursorline

" ----------------------------------- Leader shortcut commands  ---------------
nmap <leader>w ;w<CR>                       " Quicksave
nmap <leader>r ;so /home/alex/.vimrc<CR>    " Reload vimrc 
map <silent> <leader><CR> ;noh<CR>          " Clear search match highlights
nnoremap <leader>c :set cursorline! <CR>    " Toggle highlighting of the current line

" Spell Checking
map <leader>ss ;setlocal spell!<CR>  " Toggle spell checking
map <leader>s? z=                    " Give spelling suggestions
map <leader>sn ]s                    " Search for the next wrong word
map <leader>sp [s                    " Search for the previous bad word

" Automagically compile and show errors
map <leader>m ;silent make\|redraw!\|cw<CR>

"----------------------------------- Functions -------------------------------

" Change colour of the statusline based on what mode we are in
function! StatusLineColour(mode)
    if a:mode == 'i'
        hi statusline ctermfg=16 ctermbg=46
    else
        hi statusline ctermfg=16 ctermbg=223
    endif
endfunction

au InsertEnter * call StatusLineColour(v:insertmode)
au InsertLeave * hi statusline ctermfg=16 ctermbg=231
" ------------------------------- Plugin Config -----------------------

" Toggle the tagbar
nmap <F8> ;TagbarToggle<CR>
