" Set the path the plugins will live in
let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'

" Invoke vim-plug
call plug#begin(s:path . 'plugged')

" ColorSchemes
Plug 'jez/vim-colors-solarized'

" Interface Plugins
Plug 'junegunn/limelight.vim'
Plug 'kabbamine/zeavim.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'tpope/vim-unimpaired'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Generic editing plugins
Plug 'godlygeek/tabular'
Plug 'jiangmiao/auto-pairs', {'for' : ['r', 'vim', 'javascript', 'cpp']}
Plug 'junegunn/vim-peekaboo'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

" Completion and snippets
function! DoRemote(arg)
    UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim' , { 'do': function('DoRemote') }
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-jedi'

" Linting
Plug 'w0rp/ale'

" Interactive Scratchpad
Plug 'metakirby5/codi.vim'

" LaTeX
Plug 'lervag/vimtex', { 'for': ['tex'] }

" Git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

call plug#end()

" ----------------------------- General Config ------------------

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
augroup END

" ------------------- Plugins -----------------------------------

" Ale
let g:ale_sign_error = ''
let g:ale_sign_warning = ''

let g:ale_statusline_format = [' %d', ' %d', ' ok']

" Airline config
let g:airline_theme='solarized'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

let g:airline_section_error = '%{ALEGetStatusLine()}'
set noshowmode

set ttimeoutlen=10

" Deoplete
let g:deoplete#enable_at_startup = 1

" Deoplete-Clang
let g:deoplete#sources#clang#libclang_path='/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header='/usr/lib/clang'

" Deoplete Jedi
let g:deoplete#sources#jedi#show_docstring=1

" Fugitive
nnoremap <leader>gs       :Gstatus<CR>

" Git Gutter
nnoremap <c-j> <Plug>GitGutterNextHunk
nnoremap <c-k> <Plug>GitGutterPrevHunk

" Indent Guides
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
highlight IndentGuidesOdd ctermbg=black
highlight IndentGuidesEven ctermbg=grey

" Limelight
function! ToggleLimelight()
    set cursorline!
    Limelight!!0.8
endfunction
nnoremap <leader>l :call ToggleLimelight()<CR>

" Zeavim
nnoremap <leader>d    <Plug>Zeavim
