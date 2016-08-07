" Set the path the plugins will live in
let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'

" Invoke vim-plug
call plug#begin(s:path . 'plugged')

" Interface Plugins
Plug 'jdkanani/vim-material-theme'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Helmish
Plug 'junegunn/fzf', {'dir' : '~/.fzf', 'do' : './install --all'}
Plug 'junegunn/fzf.vim'

" Generic editing plugins
Plug 'tpope/vim-surround'
Plug 'neomake/neomake'

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

" But make an exception for makefiles
autocmd FileType make setlocal noexpandtab

" Colors
syntax enable
set background=dark
colorscheme material-theme

set list
set listchars=tab:».,trail:·,extends:→,precedes:←

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

" Autowrap at column 79 in 'prose files'
augroup wordwrap
    autocmd FileType rst set textwidth=79
    autocmd FileType rst set colorcolumn=80
    autocmd FileType rst highlight ColorColumn ctermbg=0
    autocmd FileType rst set fo+=t
    autocmd FileType rst set fo-=l
augroup END

" ------------------- Plugins -----------------------------------

" Airline config
let g:airline_theme='base16color'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" Deoplete
let g:deoplete#enable_at_startup = 1

" Fugitive
nnoremap <leader>gs       :Gstatus<CR>

" FZF Config
nnoremap <leader>b   :Buffers<CR>
nnoremap <leader>f   :Files ~/<CR>
nnoremap <leader>pf  :GFiles<CR>

" UltiSnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-n>"
let g:UltiSnipsJumpBackwardTrigger="<c-p>"

