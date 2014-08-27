" -----------------------------------------------------------------------------
"      Filename: vimrc
"      Author: Alex Carney - with a lot of help from the internet
"
"      Created: 12/01/14
"      Last Modified: 27/08/14
"
" -----------------------------------------------------------------------------

" Automatically reload a file if it has been modified externally
set nocompatible

" --------------------------------- Vundle ----------------------- {{{
"filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Auto update yourself
Plugin 'gmarik/vundle'

" Plugins
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-fugitive'
Plugin 'SirVer/ultisnips'
Plugin 'tpope/vim-surround'
Plugin 'majutsushi/tagbar'
Plugin 'Townk/vim-autoclose'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-vinegar'
Plugin 'mhinz/vim-signify'
Plugin 'vim-scripts/a.vim' 
Plugin 'tpope/vim-abolish'
Plugin 'Valloric/YouCompleteMe'
Plugin 'bling/vim-airline'
Plugin 'ervandew/supertab'
Plugin 'altercation/vim-colors-solarized'
Plugin 'kien/ctrlp.vim'

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
" }}}

" ----------------------------------- Colours -------------------------- {{{

syntax enable                          " Turn on syntax highlighting

let g:solarized_termtrans = 1
set background=dark
colorscheme solarized

" }}}

" ----------------------------------- Tab Settings --------------------- {{{

set expandtab                          " Insert spaces instead of tab characters
set tabstop=4                          " Tabs are equal to 4 spaces
set softtabstop=4                      " Allows navigation as if the tab characters are there
set autoindent                         " Automatically match indentation of previous line
set smartindent                        " Automatically indent new code blocks
set nowrap                             " Don't wrap long lines
set shiftround                         " I think this should help making indents more consistent

" This let's you indent/unindent blocks of code using the </> keys repectively
" while in visual mode
set shiftwidth=4
" }}}

" ----------------------------------- Editing -------------------------- {{{

" Convert a word to uppercase
nnoremap <C-u> gUiw
inoremap <C-u> <esc>gUiwea

" Reselect previous visual selection
nnoremap <leader>V V`]

" The opposite of J Splits the line
nnoremap S i<CR><esc><right>
" }}}

" ----------------------------------- Searching ------------------------ {{{

set ignorecase                         " Ignore case when searching
set smartcase                          " Apparently vim tries to be smart here
set hlsearch                           " Highlight matches
set incsearch                          " Cycle through matches in a loop

" When cycling through search results keep the matches in the centre of the
" screen
nnoremap n nzzzv
nnoremap N Nzzzv

" Open a quickfix window for the last search
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>
" }}}

" ----------------------------------- Interface ------------------------ {{{

set number                             " Enable line numbers
set mouse=a                            " Enable mouse support
set ruler                              " Show cursor position
set showmatch                          " Highlight matching brackets
set noerrorbells                       " Disable annoying beeps
set visualbell
set scrolloff=5                        " Keep more of the file visible while scrolling
set splitbelow                         " Makes using splits more intuitive since the splits happen in the right direction
set splitright
set autowriteall                       " Basically autosave whenever we try to quit vim, open new files, swap buffers around etc.
set autoread
set title                              " Set the title of the window to the current file being edited
set noswapfile                         " Disable those pescky *.swp files

" }}}

" ------------------------------------ Folding --------------------------- {{{

" Use markers for folding
set foldmethod=marker

" Open a file with all folds closed
set foldlevelstart=0

" Use spacebar to toggle a fold
nnoremap <Space> za
vnoremap <Space> za

" Close all other folds except for the current one
nnoremap <leader>z zMzvzz

" }}}

" ------------------------------------ Auto Commands --------------------- {{{

" Automatically save when vim loses focus
au FocusLost * :wa 

" Resize splits when vim is resiszed
au VimResized * exe "normal! \<c-w>="

" A few filetypes should be recognised
au BufRead,BufNewFile *.md set filetype=markdown
au BufRead,BufNewFile *.h set filetype=c
au BufRead,BufNewFile *.tex set filetype=tex

" }}}

" ------------------------------------ Misc ------------------------------- {{{

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
" }}}

" ----------------------------------- Leader shortcut commands  ----------- {{{
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
" }}}

"----------------------------------- Functions -------------------------- {{{

" Lifted from Steve Losh's vimrc shows indent guides like in some of the IDEs
let g:indentguides_state = 0
function! IndentGuides() 
    if g:indentguides_state
        let g:indentguides_state = 0
        2match None
    else
        let g:indentguides_state = 1
        execute '2match IndentGuides /\%(\_^\s*\)\@<=\%(\%'.(0*&sw+1).'v\|\%'.(1*&sw+1).'v\|\%'.(2*&sw+1).'v\|\%'.(3*&sw+1).'v\|\%'.(4*&sw+1).'v\|\%'.(5*&sw+1).'v\|\%'.(6*&sw+1).'v\|\%'.(7*&sw+1).'v\)\s/'
    endif
endfunction 
hi def IndentGuides guibg=#303030 ctermbg=234
nnoremap <leader>I :call IndentGuides()<cr>

" }}}

" ------------------------------- Plugin Config -------------------- {{{

" Toggle the tagbar
nmap <F8> ;TagbarToggle<CR>

" Fugitive commands (Git integration)
nmap <F2> ;Gstatus<CR>
nmap <F3> ;Gcommit<CR>

" Make syntastic automatically open the error box when errors are detected
let g:syntastic_auto_loc_list = 1

" Ultisnips settings
let g:snips_author = "Alex Carney"

let g:ycm_add_preview_to_completeopt = 1 " Open the preview window when giving semantic completions
let g:ycm_autoclose_preview_window_after_insertion = 1 " Close the preview after leaving insert mode


let g:ycm_key_list_select_completion=['<C-n>']
let g:ycm_key_list_previous_completion=['<C-p>']
let g:SuperTabDefaultCompletionType='<C-n>'

" Since UltiSnips and YouCompleteMe fight over tab lets change that
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsListSnippets="<c-tab>"

" Airline
set noshowmode " Since airline shows the mode for us we no longer need vim to show it as well


let g:airline#extensions#tabline#enabled = 1 " Display buffers for us when we are only using one tab, or all the tabs in a nice pretty format
let g:airline_theme='badwolf'
"let g:airline_powerline_fonts = 1

" }}}
