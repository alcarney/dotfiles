
" Format python code according to pep8
setlocal equalprg=autopep8\ -

" Use flake8 to lint our code
setlocal makeprg=flake8\ %

" What are we using to query docs?
setlocal keywordprg=pydoc

" Create folds on docstrings
setlocal foldmethod=syntax
setlocal foldminlines=12

" Make the folds more subtle
hi Folded cterm=italic

" Simple Expansions
inoremap <buffer> """<Space> """<CR>"""<Esc>O
inoremap <buffer> def<Space> def ():<Esc>2hi

"nnoremap <buffer> <leader>D :silent call Defs(expand("%"), '^[ ]*(def|class)')<CR>
