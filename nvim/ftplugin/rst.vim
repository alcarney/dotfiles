
" Enable spell checking
setlocal spell

" Set some macros which will let us automatically create titles from the
" current line
let @t='yypVr='
let @s='@tyykPj'

" Set the width to 79 chars and highlight the 80th column
setlocal textwidth=79
setlocal colorcolumn=80
highlight ColorColumn ctermbg=0

" Tell gq to format according to textwidth
setlocal formatoptions+=t

" Tell gq to break long lines?
setlocal formatoptions-=l

