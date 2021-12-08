
local install_packer = function()
  -- Adapted from:
  -- https://github.com/tjdevries/config_manager/blob/master/xdg_config/nvim/lua/tj/first_load.lua
  if vim.fn.input("Install packer? [y/n] ") ~= "y" then
    return false
  end

  local dir = string.format("%s/site/pack/packer/start/", vim.fn.stdpath("data"))
  vim.fn.mkdir(dir, "p")

  local output = vim.fn.system(
    string.format(
      "git clone %s %s",
      "https://github.com/wbthomason/packer.nvim",
      dir .. "/packer.nvim"
    )
  )

  print("\n" .. output .. "\n")
  return true
end

local bootstrap_python = function(venv)
  if vim.fn.input("Bootstrap python? [y/n] ") ~= "y" then
    return
  end

  local python = venv .. "/bin/python"
  print("\nCreating environment:" .. venv .. "\n")

  local output = vim.fn.system(string.format("python3 -m venv %s", venv))
  output = output .. "\n" .. vim.fn.system(
    string.format("%s -m pip install --upgrade pip", python)
  )
  output = output .. "\n" .. vim.fn.system(
    string.format("%s -m pip install neovim", python)
  )

  print(output)
  return true
end

return function()
  local did_stuff = false

  if os.getenv("LSP_DEBUG") then
    vim.lsp.set_log_level("debug")
  end

  -- Is packer available?
  if not pcall(require, 'packer') then
    did_stuff = did_stuff or install_packer()
  end

  -- Is there a python env?
  local venv = vim.fn.stdpath("data") .. "/venv"
  vim.g.python3_host_prog = venv .. "/bin/python"

  if vim.fn.empty(vim.fn.glob(venv)) > 0 then
    did_stuff = did_stuff or bootstrap_python(venv)
  end

  if did_stuff then
    if vim.fn.input("restart nvim? [y/n] ") == 'y' then
      vim.cmd('qa')
    end
  end
end
