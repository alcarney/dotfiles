" Set the path the plugins will live in
let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'

" -------------------------- Plugins! ---------------------------------------
call plug#begin(s:path . 'plugged')

" Interface
Plug 'jez/vim-colors-solarized'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

let g:airline_theme='solarized'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

set noshowmode
set ttimeoutlen=10

" Generic editing plugins
Plug 'godlygeek/tabular'
Plug 'jiangmiao/auto-pairs', {'for' : ['r', 'vim', 'javascript', 'cpp', 'python']}
Plug 'junegunn/vim-peekaboo'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

" Completion - TODO: Replace this with builtin completion stuff
function! DoRemote(arg)
    UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim' , { 'do': function('DoRemote') }
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-jedi'

let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path='/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header='/usr/lib/clang'
let g:deoplete#sources#jedi#show_docstring=1

" Interactive Scratchpad
Plug 'metakirby5/codi.vim'

" Git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
nnoremap <leader>gs       :Gstatus<CR>

" -------------------------- Language Specific Plugins -----------------------

" Idris
Plug 'idris-hackers/idris-vim', { 'for': ['idris'] }
let g:idris_conceal = 1

" Latex
Plug 'lervag/vimtex', { 'for': ['tex'] }

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
colorscheme solarized
highlight Comment cterm=italic

" Non printable characters
set list
set listchars=tab:».,trail:·,extends:→,precedes:←
set hidden

" Be able to switch away from modified buffers without saving
set hidden

" Make find recursive
set path=**

" No *.swp files please
set noswapfile

" Live search and replace in a new split
set inccommand=split

" Keys
nnoremap - :e .<CR>

let mapleader = ' '
nnoremap <leader><tab>         :b#<CR>
nnoremap <leader>bb            :buf
nnoremap <leader>z             zMzvzz
nnoremap <leader>n             :cn<CR>
nnoremap <leader>N             :cp<CR>

"----------------------------- Auto Commands ---------------------

augroup general
    autocmd!
    autocmd BufWritePre * %s/\s\+$//e            " Trim trailing whitespace on save.
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
    autocmd FileType markdown highlight ColorColumn ctermbg=0
    autocmd FileType markdown setlocal fo+=t
    autocmd FileType markdown setlocal fo-=l
augroup END

" Python
augroup python_filetype
    autocmd!
    autocmd FileType python setlocal equalprg=autopep8\ -
    autocmd FileType python setlocal makeprg=flake8\ %
    autocmd FileType python au QuickFixCmdPost <buffer> :cwindow
    autocmd FileType python au BufWritePost <buffer> :silent make
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
    autocmd FileType tex,plaintex setlocal textwidth=79
    autocmd FileType tex,plaintex setlocal colorcolumn=80
    autocmd FileType tex,plaintex highlight ColorColumn ctermbg=0
    autocmd FileType tex,plaintex setlocal fo+=t
    autocmd FileType tex,plaintex setlocal fo-=l
augroup END
