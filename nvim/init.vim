" Set the path the plugins will live in
let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'

" Invoke vim-plug
call plug#begin(s:path . 'plugged')

" Interface Plugins
Plug 'jdkanani/vim-material-theme'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'majutsushi/tagbar'

" Helmish
Plug 'junegunn/fzf', {'dir' : '~/.fzf', 'do' : './install --all'}
Plug 'junegunn/fzf.vim'

" Generic editing plugins
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'neomake/neomake'
Plug 'dhruvasagar/vim-table-mode'

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
colorscheme material-theme

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

" Automatically set the working dir to the current file
autocmd BufEnter * silent! lcd %:p:h

" Trim trailing whitespace on save.
autocmd BufWritePre <buffer> %s/\s\+$//e

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
autocmd FileType make setlocal noexpandtab

" R
autocmd FileType r inoremap <buffer> _  <-
autocmd FileType r inoremap <buffer> __ _
autocmd FileType r set tabstop=2

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

" Table Mode
nnoremap <leader>tm    :TableModeToggle<CR>
nnoremap <leader>mt    :Tableize<CR>
let g:table_mode_corner="+"
let g:table_mode_fillchar="="

" Tagbar
nnoremap <leader>t :TagbarToggle<CR>

" UltiSnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-n>"
let g:UltiSnipsJumpBackwardTrigger="<c-p>"
