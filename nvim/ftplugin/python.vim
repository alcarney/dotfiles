" -- python.vim
"
" Settings and configuration unique to Python files.
setlocal textwidth=88

augroup python
    autocmd!
    autocmd BufWritePre *.py execute ':Black'
augroup END
