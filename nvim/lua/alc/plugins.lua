
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup END
]]

return require('packer').startup(function()
  use 'wbthomason/packer.nvim'

  -- Appearance

  -- Generic Editing
  use 'machakann/vim-sandwich'

  -- Python
  use {
    'psf/black',
    ft = { 'python' },
    setup = function ()
      vim.g.black_virtualenv = vim.fn.stdpath('data') .. '/venv'
    end
  }
end)


