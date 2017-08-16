" Set the path the plugins will live in
let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'
let s:status = s:path . 'statusline'

" -------------------------- Plugins! ---------------------------------------
call plug#begin(s:path . 'plugged')

" Interface
Plug 'romainl/flattened'
Plug 'junegunn/rainbow_parentheses.vim'
let g:rainbow#pairs = [['(', ')'], ['[', ']']]

set noshowmode
set ttimeoutlen=10

" Generic editing plugins
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'wellle/targets.vim'
Plug 'w0rp/ale', {'for': ['python', 'cpp']}

let g:ale_sign_column_always = 1
let g:ale_open_list = 1
let g:ale_sign_error= ''
let g:ale_sign_warning = ''
let g:ale_echo_msg_error_str = ''
let g:ale_echo_msg_format = '[%linter%] %s'
let g:ale_lint_on_text_changed = 'never'

" Git
Plug 'tpope/vim-fugitive'
nnoremap <leader>gs       :Gstatus<CR>

" -------------------------- Language Specific Plugins -----------------------

" C++
let g:load_doxygen_syntax=1

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
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0

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

" Automatically center lines after a few common motions
nnoremap <c-o> <c-o>zz
nnoremap <c-i> <c-i>zz
nnoremap <s-g> <s-g>zz

" Search through command history based on current command line
cnoremap <c-n> <down>
cnoremap <c-p> <up>

let mapleader = ' '
nnoremap <leader><tab>         :b#<CR>
nnoremap <leader>b             :ls<CR>:b<Space>
nnoremap <leader>F             :filter /
nnoremap <leader>f             :find<Space>
nnoremap <leader>i             :ilist /
nnoremap <leader>l             :lclose<CR>
nnoremap <leader>z             zMzvzz

" Open all matches of the previous search in the current buffer in a loclist
nnoremap <leader>// :silent! lgrep <c-r>/ %<CR>:lwindow<CR>

" Open all matches of the previous search in all files in a loclist
nnoremap <leader>/f :silent! lgrep <c-r>/ *<CR>:lwindow<CR>

" Open all matches of the previous search in all files in the current 'project'
" in a loclist
nnoremap <leader>/p :silent! lgrep <c-r>/ `git ls-files`<CR>:lwindow<CR>

" Quick and dirty expansions
inoremap (<Space> ()<Esc>i
inoremap [<Space> []<Esc>i
inoremap '<Space> ''<Esc>i
inoremap "<Space> ""<Esc>i

"----------------------------- Auto Commands ---------------------

augroup general
    autocmd!
    autocmd BufRead,BufNewFile *.spmd set filetype=fountain
    autocmd BufWritePre * %s/\s\+$//e            " Trim trailing whitespace on save.
    autocmd BufWritePre * silent $g/^$/d                " Delete the last line if blank
augroup END

" Check the ftplugin folder for filetype specific settings!

" Hy (Pythonic Lisp)
augroup hy_filetype
    autocmd!
    autocmd BufNewFile, BufRead *.hy set filetype=hy
    autocmd FileType hy RainbowParentheses
augroup END
