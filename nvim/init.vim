" Set the path the plugins will live in
let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'

" Invoke vim-plug
call plug#begin(s:path . 'plugged')

" Interface Plugins
Plug 'sickill/vim-monokai'
Plug 'majutsushi/tagbar'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Helmish
Plug 'junegunn/fzf', {'dir' : '~/.fzf', 'do' : './install --all'}
Plug 'junegunn/fzf.vim'

" Generic editing plugins
Plug 'dhruvasagar/vim-table-mode', {'for' : ['rst', 'markdown']}
Plug 'jiangmiao/auto-pairs', {'for' : ['r']}
Plug 'neomake/neomake'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

" Completion and snippets
function! DoRemote(arg)
    UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim' , { 'do': function('DoRemote') }
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
set background=dark
colorscheme monokai
highlight Comment cterm=italic

set list
set listchars=tab:».,trail:·,extends:→,precedes:←
set hidden

" Be able to switch away from modified buffers without saving
set hidden

" Keys
let mapleader = ' '
nnoremap <leader>c             :set list!<CR>
nnoremap <leader>w             :w<CR>
nnoremap <leader><tab>         :b#<CR>
nnoremap <leader>bd            :bd<CR>

"----------------------------- Auto Commands ---------------------

augroup general
    autocmd!
    autocmd BufEnter * silent! lcd %:p:h         " Automatically set the working dir to the current file
    autocmd BufWritePre * %s/\s\+$//e            " Trim trailing whitespace on save.
augroup END

" ----------------------------- File Types -----------------------

" Autowrap at column 79 in 'prose files'
augroup wordwrap
    autocmd FileType rst set textwidth=79
    autocmd FileType rst set colorcolumn=80
    autocmd FileType rst highlight ColorColumn ctermbg=0
    autocmd FileType rst set fo+=t
    autocmd FileType rst set fo-=l
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
augroup END

" ------------------- Plugins -----------------------------------

" Airline config
let g:airline_theme='base16color'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

set ttimeoutlen=10

" Deoplete
let g:deoplete#enable_at_startup = 1

" Fugitive
nnoremap <leader>gs       :Gstatus<CR>

" FZF Config
nnoremap <leader>b   :Buffers<CR>
nnoremap <leader>f   :Files ~/<CR>
nnoremap <leader>pf  :GFiles<CR>

" Git Gutter
nnoremap <c-j> <Plug>GitGutterNextHunk
nnoremap <c-k> <Plug>GitGutterPrevHunk

" Neomake
augroup neomake
    autocmd!
    autocmd BufWritePost * Neomake
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
