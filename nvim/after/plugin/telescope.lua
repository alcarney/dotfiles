local ok, telescope = pcall(require, 'telescope')
if not ok then
  return
end

telescope.setup {
  pickers = {
    buffers = {
      theme = "ivy"
    },
    find_files = {
      theme = "ivy"
    },
    lsp_code_actions = {
      theme = "cursor"
    }
  }
}

vim.cmd [[
nnoremap <leader>f  <cmd>Telescope find_files<cr>
nnoremap <leader>b  <cmd>Telescope buffers<cr>
]]
