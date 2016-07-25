
let s:path = expand('~/.config/nvim/plugged')

call plug#begin(s:path)

" Interface Plugins
Plug 'jdkanani/vim-material-theme'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'Shougo/unite.vim'

" Generic editing plugins
Plug 'tpope/vim-surround'
Plug 'neomake/neomake'

" Nice icons
" Plug 'ryanoasis/vim-devicons'  -- FIXME: Icons are only squares

call plug#end()

" Colors
syntax enable
set background=dark
colorscheme material-theme

" Keys
let mapleader = ' '
nnoremap <Leader>b :buffers<CR>

" Airline config
let g:airline_theme='base16color'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
