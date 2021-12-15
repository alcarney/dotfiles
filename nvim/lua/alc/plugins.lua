
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup END
]]

return require('packer').startup(function()
  use 'wbthomason/packer.nvim'

  -- Appearance
  use {
    "arcticicestudio/nord-vim",
    config = function()
      vim.cmd [[colorscheme nord]]
    end
  }

  use {
    'nvim-lualine/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }

  use 'onsails/lspkind-nvim'

  -- Editing
  use 'machakann/vim-sandwich'
  use 'hrsh7th/nvim-cmp'
  use 'L3MON4D3/LuaSnip'

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


