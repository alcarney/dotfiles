local ok, everforest = pcall(require, 'everforest')
if not ok then
  return
end

vim.cmd.colorscheme 'everforest'
