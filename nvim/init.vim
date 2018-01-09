let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'
let s:status = s:path . 'statusline'

" -------------------------- Plugins! ---------------------------------------
call plug#begin(s:path . 'plugged')

" Interface
Plug 'romainl/flattened'

set noshowmode
set ttimeoutlen=10

" Generic editing plugins
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'machakann/vim-sandwich'
Plug 'tommcdo/vim-lion'
" -------------------------- Language Specific Plugins -----------------------

" Fountain
Plug 'vim-scripts/fountain.vim', {'for': ['fountain']}

" Idris
Plug 'idris-hackers/idris-vim', {'for': ['idris']}
let g:idris_conceal = 1

" Latex
Plug 'lervag/vimtex', {'for': ['tex', 'plaintex']}
let g:tex_flavour = 'latex'

" Python
Plug 'davidhalter/jedi-vim', {'for': ['python']}
let g:jedi#auto_vim_configuration = 0
let g:jedi#popup_on_dot           = 0
let g:jedi#popup_select_first     = 0

" Git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

let g:gitgutter_sign_added    = '•'
let g:gitgutter_sign_modified = '•'
let g:gitgutter_sign_removed  = '•'

" Rust
Plug 'rust-lang/rust.vim' , {'for': ['rust']}

call plug#end()

" -------------------------- General Config -----------------------------------

" Tabs and Spaces
set expandtab
set smarttab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set autoindent

" Colors
syntax enable
set background=dark
colorscheme flattened_dark
highlight Comment cterm=italic

" Non printable characters
set list
set listchars=tab:».,trail:·,extends:→,precedes:←
set hidden

" Tags
set tags=.tags

" Be able to switch away from modified buffers without saving
set hidden

" Make find recursive
set path=**

" Wildmenu
set wildmenu
set wildmode=longest:full,full
set wildignore=*.pyc,.git/,*.o,*.png,*.jpg
set wildignore+=*.jpeg

" Don't highlight search matches
set nohlsearch

" No *.swp files please
set noswapfile

" Live search and replace in a new split
set inccommand=split

" Statusline
exec 'source ' . s:status

"----------------------------- Keys ------------------------------------------

nnoremap -  :e .<CR>

" Run the command on the current line and dump the results in the buffer
nnoremap Q !!$SHELL<CR>
nnoremap QQ yyp!!$SHELL<CR>

" Automatically center lines after a few common motions
nnoremap n nzz
nnoremap N Nzz
nnoremap <c-o> <c-o>zz
nnoremap <c-i> <c-i>zz
nnoremap <s-g> <s-g>zz

" Search through command history based on current command line
cnoremap <c-n> <down>
cnoremap <c-p> <up>

" Quickly cycle through buffers
nnoremap > :bn<CR>
nnoremap < :bp<CR>

let mapleader = ' '
nnoremap <leader><tab>         :b#<CR>
nnoremap <leader>b             :filter! /\[/ ls<CR>:b<Space>
nnoremap <leader>f             :find<Space>
nnoremap <leader>F             :tabnew<CR>:find<Space>
nnoremap <leader>i             :ilist /
nnoremap <leader>z             zMzvzz

" Open all matches of the previous search in the current buffer in a loclist
nnoremap <leader>// :silent! lgrep <c-r>/ %<CR>:lwindow<CR>

" Run :g/pattern/# on the previous search
nnoremap <leader>/# :g/<c-r>//#<CR>

" Open all matches of the previous search in all files in a loclist
nnoremap <leader>/f :silent! lgrep <c-r>/ *<CR>:lwindow<CR>

" Open all matches of the previous search in all files in the current 'project'
" in a loclist
nnoremap <leader>/p :silent! lgrep <c-r>/ `git ls-files`<CR>:lwindow<CR>

" Quick and dirty expansions
inoremap (<Space> ()<Esc>i
inoremap [<Space> []<Esc>i
inoremap {<Space> {}<Esc>i
inoremap {<CR>    {<CR><CR>}<Esc>ki<Tab>
inoremap '<Space> ''<Esc>i
inoremap "<Space> ""<Esc>i

"----------------------------- Auto Commands ---------------------

augroup general
    autocmd!
    autocmd BufRead,BufNewFile *.spmd set filetype=fountain
    autocmd BufWritePre * %s/\s\+$//e            " Trim trailing whitespace on save.
    autocmd VimResized * <c-w>=                  " Resize splits if vim is resized
augroup END

augroup lint
    autocmd!
    autocmd BufWritePost *.py silent make! | silent redraw!
    autocmd QuickFixCmdPost [^l]* cwindow
augroup END

" Check the ftplugin folder for filetype specific settings!
