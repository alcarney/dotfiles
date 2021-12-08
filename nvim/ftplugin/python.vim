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

" Configure pyright as our LSP server.
lua<<EOF
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

require'lspconfig'.pyright.setup{
  capabilities = capabilities,
  settings = {
    python = {
      -- Use whichever python is active in the current environment
      pythonPath = vim.fn.system("command -v python | tr -d '[:space:]'")
    }
  },
  on_attach = function(client, bufnr)
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    local opts = { noremap = true, silent = true}
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<c-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '[g', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', ']g', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  end
}
EOF
