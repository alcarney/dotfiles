" -- reorder_python_imports.vim
"
" Simple plugin that calls `reorder_python_imports` on the current buffer.
" Loosely modelled after the black.vim plugin
"
" https://github.com/psf/black/blob/main/autoload/black.vim
"
" This assumes that the `reorder_python_imports` package is avaiable to to the
" interpreter specified by g:python3_host_prog
python3<<EOF
from reorder_python_imports import fix_file_contents

def ReorderImports():
    contents = "\n".join(vim.current.buffer) + "\n"
    reformatted = fix_file_contents(contents)

    if contents == reformatted:
        return

    vim.current.buffer[:] = reformatted.split("\n")[:-1]
EOF

command! ReorderPyImports :py3 ReorderImports()
