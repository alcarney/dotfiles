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

--- Options/keybinds to apply to every LSP enabled buffer
local common_lsp_setup = function(client, bufnr)
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
--lspconfig.clangd.setup{
--  capabilities = capabilities,
--  on_attach = common_lsp_setup
--}

-- CMake
--lspconfig.cmake.setup{
--  capabilities = capabilities,
--  on_attach = common_lsp_setup
--}

-- Esbonio

-- TODO: Rewrite sync-scrolling to rely on this more.
require('neoscroll').setup {}

local function scroll_view(ev)
  local esbonio = vim.lsp.get_active_clients({bufnr = 0, name = "esbonio"})[1]
  local view = vim.fn.winsaveview()

  local params = { line = view.topline }
  esbonio.notify("view/scroll", params)
end

local function esbonio_preview_file()
  local params = {
    command = "esbonio.server.previewFile",
    arguments = {
      { uri = vim.uri_from_bufnr(0), show = false },
    }
  }
  local result = vim.lsp.buf.execute_command(params)
  print(vim.inspect(result))

  -- Setup sync scrolling
  local augroup = vim.api.nvim_create_augroup("EsbonioSyncScroll", { clear = true })
  vim.api.nvim_create_autocmd({"WinScrolled"}, {
    callback = scroll_view,
    group = augroup,
    buffer = 0,
  })
end


lspconfig.esbonio.setup{
  capabilities = capabilities,
  -- TODO: Make it easy to switch between these?
  -- cmd = { "esbonio" },
  cmd = {"/var/home/alex/Projects/lsp-devtools/.env/bin/lsp-devtools", "agent", "--", "esbonio"},
  --cmd = {
  --  "python", "-m", "debugpy",
  --  "--listen", "localhost:5678",
  --  "--wait-for-client",
  --  "-m", "esbonio",
  --},
  filetypes = {"rst"},
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
  handlers = {
    ["editor/scroll"] = function(err, result, ctx, config)
      -- TODO: This needs improvement...
      vim.cmd('normal '.. result.line .. 'Gzt')
    end
  },
  on_attach = function (client, bufnr)
    common_lsp_setup(client, bufnr)

    vim.api.nvim_create_user_command(
      "EsbonioPreviewFile", esbonio_preview_file, { desc = "Preview file" }
    )
  end
}

-- Pyright
lspconfig.pyright.setup{
  capabilities = capabilities,
  on_attach = common_lsp_setup,
  settings = {
    python = {
      pythonPath = find_venv()
    }
  },
}

-- Rust
lspconfig.rust_analyzer.setup {
  capabilities=capabilities,
  on_attach = common_lsp_setup,
}

-- Tailwind
--lspconfig.tailwindcss.setup{
--  capabilities = capabilities,
--  on_attach = common_lsp_setup
--}
