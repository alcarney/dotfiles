-- UI for $/progress and other notifications
local ok, fidget = pcall(require, 'fidget')
if not ok then
  return
end

fidget.setup {
  notification = {
    override_vim_notify = true,
  }
}
