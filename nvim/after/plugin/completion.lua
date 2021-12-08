local cmp = require('cmp')

cmp.setup({
  sources = {
    { name = "nvim_lsp" }
  },
  snippet = {
    expand = function(args)
      require("luasnip").lsp_expand(args.body)
    end,
  },
})
