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
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

" Interactive Scratchpad
Plug 'metakirby5/codi.vim'

" Git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
nnoremap <leader>gs       :Gstatus<CR>

" -------------------------- Language Specific Plugins -----------------------

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
set smartindent
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

"----------------------------- Keys ------------------------------------------

nnoremap -  :e .<CR>
nnoremap ]l @=execute('lnext')<CR>
nnoremap [l @=execute('lprev')<CR>
nnoremap ]c @=execute('cnext')<CR>
nnoremap [c @=execute('cprev')<CR>
nnoremap // :silent! lgrep <c-r>/ %<CR>:lopen<CR>

let mapleader = ' '
nnoremap <leader><tab>         :b#<CR>
nnoremap <leader>bb            :buf
nnoremap <leader>z             zMzvzz

" Run the command on the current line and dump the results in the buffer
nnoremap Q !!$SHELL<CR>

" Search through command history based on current command line
cnoremap <c-n> <down>
cnoremap <c-p> <up>

"----------------------------- Auto Commands ---------------------

augroup general
    autocmd!
    autocmd BufRead,BufNewFile *.spmd set filetype=fountain
    autocmd BufWritePre * %s/\s\+$//e            " Trim trailing whitespace on save.
augroup END

" Hy (Pythonic Lisp)
augroup hy_filetype
    autocmd!
    autocmd BufNewFile, BufRead *.hy set filetype=hy
    autocmd FileType hy RainbowParentheses
augroup END

" Make
augroup make_filetype
    autocmd!
    autocmd FileType make setlocal noexpandtab
augroup END

" Markdown
augroup markdown_filetype
    autocmd!
    autocmd FileType markdown setlocal textwidth=79
    autocmd FileType markdown setlocal colorcolumn=80
    autocmd FileType markdown setlocal spell
    autocmd FileType markdown highlight ColorColumn ctermbg=0
    autocmd FileType markdown setlocal fo+=t
    autocmd FileType markdown setlocal fo-=l
augroup END

" Python
augroup python_filetype
    autocmd!
    autocmd FileType python setlocal equalprg=autopep8\ -
    autocmd FileType python setlocal makeprg=flake8\ %
    autocmd FileType python au QuickFixCmdPost <buffer> :lwindow
    autocmd FileType python au BufWritePost <buffer> :silent lmake
    autocmd FileType python au BufWritePost <buffer> :silent !ctags -R -f .tags .
augroup END

" R
augroup r_filetype
    autocmd!
    autocmd FileType r inoremap <buffer> _  <-
    autocmd FileType r inoremap <buffer> __ _
    autocmd FileType r setlocal tabstop=2 shiftwidth=2 softtabstop=2
    autocmd FileType r setlocal comments="b:#,b:#'"
augroup END

" RST
augroup rst_filetype
    autocmd!
    autocmd FileType rst let @t='yypVr='
    autocmd FileType rst let @s='@tyykPj'
    autocmd FileType rst setlocal textwidth=79
    autocmd FileType rst setlocal colorcolumn=80
    autocmd FileType rst highlight ColorColumn ctermbg=0
    autocmd FileType rst setlocal fo+=t
    autocmd FileType rst setlocal fo-=l
augroup END

" Tex
augroup tex_filetype
    autocmd!
    autocmd FileType tex setlocal textwidth=79
    autocmd FileType tex setlocal colorcolumn=80
    autocmd FileType tex highlight ColorColumn ctermbg=0
    autocmd FileType tex setlocal fo+=t
    autocmd FileType tex setlocal fo-=l

    autocmd FileType tex let b:vimtex_main = 'main.tex'
augroup END
