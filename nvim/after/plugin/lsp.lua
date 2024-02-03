local lspconfig = require('lspconfig')
local util = require('lspconfig.util')
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


-- Helper functions

--- Attempt to find the relevant virtualenv for the project.
local function find_venv()

  -- If there is an active virtualenv, use that
  if vim.env.VIRTUAL_ENV then
    return vim.env.VIRTUAL_ENV .. "/bin/python"
  end

  -- Search within the current git repo to see if we can find a virtualenv to
  -- use.
  local repo = util.find_git_ancestor(vim.fn.getcwd())
  if not repo then
    return nil
  end

  local candidates = vim.fs.find("pyvenv.cfg", { path = repo })
  if #candidates == 0 then
    return nil
  end

  -- TODO: If more than one found, prompt to see which I want to use?
  return vim.fn.resolve(candidates[1] .. "./../bin/python")
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
      sphinx = {
        buildCommand = {"sphinx-build", "-M", "dirhtml", "docs", "docs/_build"},
        pythonCommand = { find_venv() }
      },
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
      pythonPath = find_venv()
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
