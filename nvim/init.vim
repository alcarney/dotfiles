call plug#begin('.config/nvim/plugged')

" Interface Plugins
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Generic editing plugins
Plug 'tpope/vim-surround'
Plug 'neomake/neomake'

call plug#end()

" Airline config
let g:airline_theme='base16'
let g:airline#extensions#tabline#enabled = 1
