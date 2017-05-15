
" Set the width to 79 chars and highlight the 80th column
setlocal textwidth=79
setlocal colorcolumn=80
highlight ColorColumn ctermbg=0

" Spell checking
setlocal spell

" Tell gq to format according to textwidth
setlocal formatoptions+=t

" Tell gq to break long lines?
setlocal formatoptions-=l

" Make keyword complete work with my label conventions
" \label{type:description-of-thing}
setlocal iskeyword+=:,-

" Tell vimtex what we call our main tex documents
let b:vimtex_main = 'main.tex'

" Fix the cases where vim doesn't pick the right form of
" tex
autocmd BufRead,BufNewFile *.tex  set filetype=tex
