
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup END
]]

local ok, packer = pcall(require, 'packer')
if not ok then
  return
end

packer.init {
  display = {
    open_fn = function()
      return require("packer.util").float { border = "rounded" }
    end
  }
}

return packer.startup(function()
  use 'wbthomason/packer.nvim'

  -- Appearance
  use {
    "arcticicestudio/nord-vim",
    config = function()
      vim.cmd [[
        colorscheme nord

        highlight Comment term=italic cterm=italic gui=italic
      ]]
    end
  }

  use {
    'nvim-lualine/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }

  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  -- Editing
  use 'machakann/vim-sandwich'
  use 'hrsh7th/nvim-cmp'
  use 'L3MON4D3/LuaSnip'
  use 'tpope/vim-unimpaired'

  -- Git
  use {
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    },
    config = function()
      require('gitsigns').setup()
    end
  }

  -- LSP
  use 'neovim/nvim-lspconfig'
  use 'hrsh7th/cmp-nvim-lsp'

  -- Python
  use {
    'psf/black',
    ft = { 'python' },
    setup = function ()
      vim.g.black_virtualenv = vim.fn.stdpath('data') .. '/venv'
    end
  }
end)


