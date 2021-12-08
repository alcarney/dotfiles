local lspconfig = require('lspconfig')

local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap = true, silent = true}
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd',        '<cmd>lua vim.lsp.buf.definition()<CR>',    opts)

  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gh',        '<cmd>lua vim.lsp.buf.hover()<CR>',         opts)

  vim.api.nvim_buf_set_keymap(bufnr, 'n', '[g',        '<cmd>lua vim.diagnostic.goto_prev()<CR>',  opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', ']g',        '<cmd>lua vim.diagnostic.goto_next()<CR>',  opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)

end

-- Esbonio
lspconfig.esbonio.setup{
  capabilities = capabilities,
  on_attach = on_attach
}

-- Pyright
lspconfig.pyright.setup{
  capabilities = capabilities,
  on_attach = on_attach,
  settings = {
    python = {
      -- Use whichever python is active in the current environment
      pythonPath = vim.fn.system("command -v python | tr -d '[:space:]'")
    }
  },
}

