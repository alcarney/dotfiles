
" Set the width to 79 chars and highlight the 80th column
setlocal textwidth=79
setlocal colorcolumn=80
highlight ColorColumn ctermbg=0

" Tell gq to format according to textwidth
setlocal formatoptions+=t

" Tell gq to break long lines?
setlocal formatoptions-=l

" Tell vimtex what we call our main tex documents
let b:vimtex_main = 'main.tex'
