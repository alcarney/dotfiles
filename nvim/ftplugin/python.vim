
" Format python code according to pep8
setlocal equalprg=autopep8\ -

" Use flake8 to lint our code
setlocal makeprg=flake8\ %

" What are we using to query docs?
setlocal keywordprg=pydoc

" Open a loclist with errors (if there are any)
autocmd QuickFixCmdPost <buffer> :lwindow

" Automatically lint on write
autocmd BufWritePost <buffer> :silent! lmake

" Create folds on docstrings
setlocal foldmethod=syntax
setlocal foldminlines=12

" Make the folds more subtle
hi Folded cterm=italic

