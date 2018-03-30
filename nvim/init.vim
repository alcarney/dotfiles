let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'

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

" Vue.js
Plug 'posva/vim-vue', {'for': ['vue']}

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
highlight StatusLine cterm=italic
highlight StatusLineNC cterm=italic
highlight Visual cterm=bold

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
set wildignore=.git/,*.o,*.png,*.jpg
set wildignore+=*.jpeg

" Don't highlight search matches
set nohlsearch

" No *.swp files please
set noswapfile

" Live search and replace in a new split
set inccommand=split

" Statusline
set laststatus=0
set showtabline=0
set noruler

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
let maplocalleader = '\'
nnoremap <leader><tab>         :b#<CR>
nnoremap <leader>b             :filter! /\[/ ls<CR>:b<Space>
nnoremap <leader>f             :find<Space>
nnoremap <leader>z             zMzvzz

nnoremap <localleader><tab>    <c-w>p
nnoremap <localleader>f        :echo expand("%")<CR>
nnoremap <localleader>t        :tabs<CR>

" Open all matches of the previous search in the current buffer in a loclist
nnoremap <leader>// :silent! lvimgrep /<c-r>//j %<CR>:lwindow<CR>

" Run :g/pattern/# on the previous search
nnoremap <leader>/# :g/<c-r>//#<CR>

" Open all matches of the previous search in all files in a loclist
nnoremap <leader>/f :silent! lvimgrep /<c-r>//j *<CR>:lwindow<CR>

" Open all matches of the previous search in all files in dir/*.ext
nnoremap <leader>/d :silent! lvimgrep /<c-r>//j <c-r>=expand("%:p:h")<CR>/*.<c-r>=expand("%:e")<CR><CR>:lwindow<CR>

" Open all matches of the previous search in all files in the current 'project'
" in a loclist
nnoremap <leader>/p :silent! lvimgrep /<c-r>//j `git ls-files`<CR>:lwindow<CR>

" Do a search for the previous pattern, but leave the scope of the search for
" the user to fill in
nnoremap <leader>/s :silent! lvimgrep /<c-r>//j

" Create a command to diff the current buffer against the file on disk
" tweaked version of :help :DiffOrig
command! DiffOrig tabedit % | vert new | set buftype=nofile | read ++edit # |
            \ 0d_ | windo diffthis
nnoremap <leader>d :DiffOrig<CR>

" Functions

" A poor man's tagbar, call it with a filename and a regexp to filter by
" it opens the result in a new vertical split - see lang.vim files for
" invocations
function! Defs(filename, pattern)
    vert new
    set buftype=nofile
    exec '0read ' . a:filename
    exec 'g!/' . a:pattern . '/d'
endfunction

"----------------------------- Auto Commands ---------------------

augroup general
    autocmd!
    autocmd BufRead,BufNewFile *.pm setlocal filetype=racket
    autocmd BufWritePre * %s/\s\+$//e                           " Trim trailing whitespace on save.
    autocmd BufWritePre * silent $g/^$/d                        " Delete the last line if blank
    autocmd VimResized * <c-w>=                                 " Resize splits if vim is resized
augroup END

augroup lint
    autocmd!
    autocmd BufWritePost *.py silent make! | silent redraw!
    autocmd QuickFixCmdPost [^l]* cwindow
augroup END

" Check the ftplugin folder for filetype specific settings!
