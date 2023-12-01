" python.vim
"
" Tweak to python syntax definition to enable folding of docstrings.
syn region  pythonString matchgroup=pythonTripleQuotes
      \ start=+[uU]\=\z('''\|"""\)+ skip=+\\["']+ end="\z1" keepend fold
      \ contains=pythonEscape,pythonSpaceError,pythonDoctest,@Spell
syn region  pythonRawString matchgroup=pythonTripleQuotes
      \ start=+[uU]\=[rR]\z('''\|"""\)+ end="\z1" keepend fold
      \ contains=pythonSpaceError,pythonDoctest,@Spell

