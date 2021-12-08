" -- python.vim
"
" Settings and configuration unique to Python files.
setlocal textwidth=88

augroup python
    autocmd!
    " see: autoload/pyimports.vim
    autocmd BufWritePre *.py execute 'call pyimports#Reorder()'
    autocmd BufWritePre *.py execute ':Black'
augroup END

