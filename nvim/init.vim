" ---------------------------------- init.vim ----------------------------------
"
" settings that apply everywhere

" --------------------------------- appearance ---------------------------------
"
" colorcolumn: highlight the column at `textwidth` + 1
" list:        display non printable characters according to `listchars`
" listchars:   describes how to visualise certain non printable characters
" nowrap:      don't visually wrap long lines
set colorcolumn=+1
set list
set listchars=tab:».,trail:·,extends:→,precedes:←
set nowrap

highlight ColorColumn ctermbg=0 cterm=italic


" ------------------------------- auto commands --------------------------------
augroup general
    autocmd!
    autocmd BufWritePre * %s/\s\+$//e
augroup END

" ---------------------------------- behavior ----------------------------------
"
" hidden:     allow modified buffers to be sent to the background
" splitbelow: open new windows below the current one
" splitright: open new windows to the right of the current one
" textwidth:  how many characters to a line
set hidden
set splitbelow
set splitright
set textwidth=80


" ---------------------------------- key maps ----------------------------------
"
" mapleader:      prefix to use for <leader> mappings
" maplocalleader: prefix to use for <localleader> mappings
let mapleader = '\'
let maplocalleader = ' '

" buffers ------------------------------
"
" [b:            goto previous buffer
" ]b:            goto next buffer
" <leader>b:     list open buffers, quixfix windows and other 'vim' windows are
"                not included
" <leader><tab>: switch to most recent buffer
nnoremap [b            :bp<cr>
nnoremap ]b            :bn<cr>
nnoremap <leader>b     :filter! /\[/ ls<cr>:b<space>
nnoremap <leader><tab> :b#<cr>

" files --------------------------------
"
" <leader>f: find file
nnoremap <leader>f  :find<space>

" movement -----------------------------
"
" tweaks to a number of movement commands to ensure that the result is always
" centered on the screen
nnoremap n     nzz
nnoremap N     Nzz
nnoremap G     Gzz
nnoremap <c-i> <c-i>zz
nnoremap <c-o> <c-o>zz

" search -------------------------------
"
" <leader>#: flash up the results from the previous search in the messages area
" <leader>/: open a location list containing the results of the previous search
nnoremap <leader># :g/<c-r>//#<cr>
nnoremap <leader>/ :silent! lvimgrep /<c-r>//j %<cr>:lwindow<cr>

" windows ------------------------------
"
" <c-l>:     move to window right
" <c-k>:     move to window above
" <c-j>:     move to window below
" <c-h>:     move to window left
" <leader>o: move to `other` window
nnoremap <c-l>      <c-w><c-l>
nnoremap <c-k>      <c-w><c-k>
nnoremap <c-j>      <c-w><c-j>
nnoremap <c-h>      <c-w><c-h>
nnoremap <leader>o  <c-w>p

" ---------------------------------- plugins -----------------------------------
"
call plug#begin('~/.local/share/nvim/plugged')

" python -------------------------------
"
" psf/black: python source code formatter
let g:black_virtualenv = '/home/alex/.config/nvim/.black'
Plug 'psf/black', {'for': 'python'}

" fisadev/vim-isort: python import statement sorter
let g:vim_isort_map = ''
let g:vim_isort_python_version = 'python3'
Plug 'fisadev/vim-isort', {'for': 'python'}

call plug#end()

" ------------------------------ provider: python ------------------------------
let g:python3_host_prog = '/home/alex/.config/nvim/.py3/bin/python3'


" --------------------------------- searching ----------------------------------
"
" inccommand: preview the results of a :s/.../.../ command in a split window
" incsearch:  jump to matches as they are typed
" nohlsearch: don't highlight matching search results
set inccommand=split
set incsearch
set nohlsearch


" --------------------------------- whitespace ---------------------------------
"
" exapndtab:   insert `tabstop` spaces when hitting the <tab> key
" softtabstop: treat N spaces as a tab character for certain operations.
"              e.g. backspace
" shiftwidth:  how many spaces do the indentation commands use?
" tabstop:     tabs = N spaces
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4
