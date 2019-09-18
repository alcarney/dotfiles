" --------------------------------- python.vim ---------------------------------

" ------------------------------- auto commands --------------------------------
augroup python
    autocmd!
    autocmd BufWritePre *.py execute ':Isort'
    autocmd BufWritePre *.py execute ':Black'
augroup END

" ------------------------------ text formatting -------------------------------
setlocal textwidth=88
