" Set the path the plugins will live in
let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'

" -------------------------- Plugins! ---------------------------------------
call plug#begin(s:path . 'plugged')

" Interface
Plug 'romainl/flattened'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/rainbow_parentheses.vim'
let g:rainbow#pairs = [['(', ')'], ['[', ']']]

let g:airline_theme='solarized'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

set noshowmode
set ttimeoutlen=10

" Generic editing plugins
Plug 'godlygeek/tabular'
Plug 'junegunn/vim-peekaboo'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'wellle/targets.vim'
Plug 'junegunn/goyo.vim'

" Interactive Scratchpad
Plug 'metakirby5/codi.vim'

" Git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
nnoremap <leader>gs       :Gstatus<CR>

" -------------------------- Language Specific Plugins -----------------------

" C++
let g:load_doxygen_syntax=1

" Fountain
Plug 'vim-scripts/fountain.vim', {'for': ['fountain']}

" Idris
Plug 'idris-hackers/idris-vim', { 'for': ['idris'] }
let g:idris_conceal = 1

" Latex
Plug 'lervag/vimtex', { 'for': ['tex', 'plaintex'] }
let g:tex_flavour = 'latex'

call plug#end()

" -------------------------- General Config -----------------------------------

" Tabs and Spaces
set expandtab
set smarttab
set tabstop=4
set shiftwidth=4
set softtabstop=4

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

" Don't highlight search matches
set nohlsearch

" No *.swp files please
set noswapfile

" Live search and replace in a new split
set inccommand=split

"----------------------------- Functions -------------------------------------
function! Docs(thing)
    :execute ':silent !' . &keywordprg . ' ' . a:thing . ' > /tmp/docs'
    pedit /tmp/docs
endfunction


"----------------------------- Keys ------------------------------------------

nnoremap -  :e .<CR>

" Location lists and quickfixes
nnoremap ]l @=execute('lnext')<CR>
nnoremap [l @=execute('lprev')<CR>
nnoremap ]c @=execute('cnext')<CR>
nnoremap [c @=execute('cprev')<CR>

" Run the command on the current line and dump the results in the buffer
nnoremap Q !!$SHELL<CR>

" A 'better K', run my Docs command on the word under the cursor
nnoremap K :call Docs(expand("<cword>"))<CR>

" Search through command history based on current command line
cnoremap <c-n> <down>
cnoremap <c-p> <up>

let mapleader = ' '
nnoremap <leader><tab>         :b#<CR>
nnoremap <leader>bb            :buf
nnoremap <leader>z             zMzvzz
nnoremap <leader>l             :lclose<CR>

" Open all matches of the previous search in the current buffer in a loclist
nnoremap <leader>// :silent! lgrep <c-r>/ %<CR>:lwindow<CR>

" Open all matches of the previous search in all files in a loclist
nnoremap <leader>/f :silent! lgrep <c-r>/ *<CR>:lwindow<CR>

" Open all matches of the previous search in all files in the current 'project'
" in a loclist
nnoremap <leader>/p :silent! lgrep <c-r>/ `git ls-files`<CR>:lwindow<CR>

"----------------------------- Auto Commands ---------------------

augroup general
    autocmd!
    autocmd BufRead,BufNewFile *.spmd set filetype=fountain
    autocmd BufWritePre * %s/\s\+$//e            " Trim trailing whitespace on save.
augroup END

" Check the ftplugin folder for filetype specific settings!

" Hy (Pythonic Lisp)
augroup hy_filetype
    autocmd!
    autocmd BufNewFile, BufRead *.hy set filetype=hy
    autocmd FileType hy RainbowParentheses
augroup END

