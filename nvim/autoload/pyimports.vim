
" Calls `reorder_python_imports` on the current buffer.
" Loosely modelled after the black.vim plugin
"
" https://github.com/psf/black/blob/main/autoload/black.vim
"
" This assumes that the `reorder_python_imports` package is avaiable to to the
" interpreter specified by g:python3_host_prog
python3<<EOF

def ReorderImports():
    from reorder_python_imports import fix_file_contents

    contents = "\n".join(vim.current.buffer) + "\n"
    reformatted = fix_file_contents(contents)

    if contents == reformatted:
        return

    vim.current.buffer[:] = reformatted.split("\n")[:-1]
EOF

function pyimports#Reorder()
    :py3 ReorderImports()
endfunction
