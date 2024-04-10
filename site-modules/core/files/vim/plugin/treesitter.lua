require("nvim-treesitter.configs").setup {
  auto_install = true,
  playground = { enable = true },
  indent = { enable = false },
  rainbow = { enable = true },
  highlight = {
    -- `false` will disable the whole extension
    enable = true,
    additional_vim_regex_highlighting = true,
  },
}


vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldenable = false
