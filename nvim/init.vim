" Set the path the plugins will live in
let s:path = expand('~/.config/nvim/')
let s:config = s:path . 'init.vim'

" Invoke vim-plug
call plug#begin(s:path . 'plugged')

" Interface Plugins
Plug 'jdkanani/vim-material-theme'
Plug 'majutsushi/tagbar' {'on' : 'TagbarToggle'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Helmish
Plug 'junegunn/fzf', {'dir' : '~/.fzf', 'do' : './install --all'}
Plug 'junegunn/fzf.vim'

" Generic editing plugins
Plug 'dhruvasagar/vim-table-mode' {'for' : ['rst', 'markdown']}
Plug 'neomake/neomake'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

" Completion
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
highlight Comment cterm=italic

set list
set listchars=tab:».,trail:·,extends:→,precedes:←
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

" Ultisnips
let g:UltiSnipsSnippetDir='$HOME/.config/nvim/snippets'
let g:UltiSnipsExpandTabTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-n>"
let g:UltiSnipsJumpBackwardTrigger="<c-p>"
