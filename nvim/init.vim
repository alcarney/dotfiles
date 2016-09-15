" Set the path the plugins will live in
let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'

" Invoke vim-plug
call plug#begin(s:path . 'plugged')

" ColorSchemes
Plug 'jez/vim-colors-solarized'
"Plug 'sickill/vim-monokai'

" Interface Plugins
Plug 'junegunn/limelight.vim'
Plug 'kabbamine/zeavim.vim'
Plug 'majutsushi/tagbar'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'tpope/vim-unimpaired'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Helmish
Plug 'junegunn/fzf', {'dir' : '~/.fzf', 'do' : './install --all'}
Plug 'junegunn/fzf.vim'

" Generic editing plugins
Plug 'dhruvasagar/vim-table-mode', {'for' : ['rst', 'markdown']}
Plug 'jiangmiao/auto-pairs', {'for' : ['r', 'vim', 'javascript', 'cpp']}
Plug 'junegunn/vim-peekaboo'
Plug 'neomake/neomake'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

" Completion and snippets
function! DoRemote(arg)
    UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim' , { 'do': function('DoRemote') }
Plug 'zchee/deoplete-clang'
Plug 'SirVer/ultisnips'

" Git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" Nice icons
" Plug 'ryanoasis/vim-devicons'  -- FIXME: Icons are only squares

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
set background=light
colorscheme solarized
highlight Comment cterm=italic

set list
set listchars=tab:».,trail:·,extends:→,precedes:←
set hidden

set scrolloff=999
set cursorline

" Be able to switch away from modified buffers without saving
set hidden

" Keys
let mapleader = ' '
nnoremap <leader>c             :set list!<CR>
nnoremap <leader>w             :w<CR>
nnoremap <leader><tab>         :b#<CR>
nnoremap <leader>bd            :bd<CR>
nnoremap <leader>z             zMzvzz

let c_no_comment_fold=1

"----------------------------- Auto Commands ---------------------

augroup general
    autocmd!
    "autocmd BufEnter * silent! lcd %:p:h         " Automatically set the working dir to the current file
    autocmd BufWritePre * %s/\s\+$//e            " Trim trailing whitespace on save.
augroup END

" ----------------------------- File Types -----------------------

" Autowrap at column 79 in 'prose files'
augroup wordwrap
    autocmd!
    autocmd FileType rst set textwidth=79
    autocmd FileType rst set colorcolumn=80
    autocmd FileType rst highlight ColorColumn ctermbg=0
    autocmd FileType rst set fo+=t
    autocmd FileType rst set fo-=l

    autocmd FileType markdown set textwidth=79
    autocmd FileType markdown set colorcolumn=80
    autocmd FileType markdown highlight ColorColumn ctermbg=0
    autocmd FileType markdown set fo+=t
    autocmd FileType markdown set fo-=l
augroup END

" C++
augroup cpp_filetype
    autocmd!
    autocmd FileType cpp let @u='gUiwe'
augroup END

" Make
augroup make_filetype
    autocmd!
    autocmd FileType make setlocal noexpandtab
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
augroup END

" ------------------- Plugins -----------------------------------

" Airline config
let g:airline_theme='solarized'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

set ttimeoutlen=10

" Deoplete
let g:deoplete#enable_at_startup = 1

" Deoplete-Clang
let g:deoplete#sources#clang#libclang_path='/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header='/usr/lib/clang'

" Fugitive
nnoremap <leader>gs       :Gstatus<CR>

" FZF Config
nnoremap <leader>b   :Buffers<CR>
nnoremap <leader>f   :Files ~/<CR>
nnoremap <leader>pf  :GFiles<CR>

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

" Neomake
let g:neomake_open_list=2

augroup neomake
    autocmd!
    autocmd BufWrite * Neomake!
augroup END

" Table Mode
nnoremap <leader>tm    :TableModeToggle<CR>
nnoremap <leader>mt    :Tableize<CR>
let g:table_mode_corner="+"
let g:table_mode_fillchar="="

" Tagbar
nnoremap <leader>t :TagbarToggle<CR>
let g:tagbar_type_r = {
    \ 'ctagstype' : 'r',
    \ 'kinds'     : [
        \ 'f:Functions',
        \ 'g:GlobalVariables',
        \ 'v:FunctionVariables',
    \ ]
\ }

" UltiSnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-n>"
let g:UltiSnipsJumpBackwardTrigger="<c-p>"

" Zeavim
nnoremap <leader>d    <Plug>Zeavim
