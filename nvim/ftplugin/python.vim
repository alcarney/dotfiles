" -- python.vim
"
" Settings and configuration unique to Python files.
setlocal textwidth=88

augroup python
    autocmd!
    " see: plugin/reorder_python_imports.vim
    autocmd BufWritePre *.py execute ':ReorderPyImports'
    autocmd BufWritePre *.py execute ':Black'
augroup END
