
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

  -- Generic Editing
  use 'machakann/vim-sandwich'

  -- LSP
  use 'neovim/nvim-lspconfig'

  -- Python
  use {
    'psf/black',
    ft = { 'python' },
    setup = function ()
      vim.g.black_virtualenv = vim.fn.stdpath('data') .. '/venv'
    end
  }
end)


