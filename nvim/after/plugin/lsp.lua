local lspconfig = require('lspconfig')
local has_telescope, _ = pcall(require, 'telescope')

local capabilities = require('cmp_nvim_lsp').default_capabilities()


-- UI for $/progress and other notifications
require('fidget').setup {
  notification = {
    override_vim_notify = true,
  }
}

-- Global diagnostic config.
vim.diagnostic.config({
  virtual_text = false,
  float = {
    focusable = false,
    style = "minimal",
    border = "rounded",
    source = "always"
  }
})

local keymap_opts = { noremap = true, silent = true}
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, keymap_opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, keymap_opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, keymap_opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, keymap_opts)

local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', 'gh', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
end


-- C/C++
lspconfig.clangd.setup{
  capabilities = capabilities,
  on_attach = on_attach
}

-- CMake
lspconfig.cmake.setup{
  capabilities = capabilities,
  on_attach = on_attach
}

-- Esbonio
lspconfig.esbonio.setup{
  capabilities = capabilities,
  cmd = {"lsp-devtools", "agent", "--", "esbonio"},
  filetypes = {"rst"},
--  init_options = { },
  settings = {
    esbonio = {
      server = {
        logLevel = "debug"
      },
      sphinx = { },
    },
  },
  on_attach = on_attach,
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

-- Rust
lspconfig.rust_analyzer.setup {
  capabilities=capabilities,
  on_attach = on_attach,
}

-- Tailwind
lspconfig.tailwindcss.setup{
  capabilities = capabilities,
  on_attach = on_attach
}
