local ok, notify = pcall(require, 'notify')
if not ok then
  return
end

-- Override the default notification handler
vim.notify = notify

notify.setup {
  stages = "slide"
}
