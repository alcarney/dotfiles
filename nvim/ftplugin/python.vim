
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

" Color the ALE sign column
hi SignColumn ctermbg=0
hi ALEErrorSign ctermbg=0
hi ALEWarningsSign ctermbg=0

" Simple Expansions
inoremap """<Space> """<CR>"""<Esc>O
inoremap def<Space> def ():<Esc>2hi
