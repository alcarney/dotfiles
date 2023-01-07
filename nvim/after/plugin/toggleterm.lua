local ok, toggleterm = pcall(require, "toggleterm")
if not ok then
  return
end

toggleterm.setup({
  open_mapping = [[<leader>t]],
  insert_mappings = false,
  terminal_mappings = false,
})
