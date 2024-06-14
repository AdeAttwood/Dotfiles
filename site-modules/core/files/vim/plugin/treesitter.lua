require("nvim-treesitter.configs").setup {
  auto_install = true,
  playground = { enable = true },
  indent = { enable = true },
  rainbow = { enable = true },
  highlight = {
    -- `false` will disable the whole extension
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["aa"] = "@parameter.outer", -- this is also arguments
        ["ia"] = "@parameter.inner", -- this is also arguments
        ["ab"] = "@block.outer",
        ["ib"] = "@block.inner",
        ["ac"] = "@comment.outer",
        ["ic"] = "@comment.inner",
      },
    },
    move = {
      enable = true,
      set_jumps = true,
      goto_next_start = {
        ["]f"] = "@function.outer",
      },
      goto_previous_start = {
        ["[f"] = "@function.outer",
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ["L"] = "@parameter.inner",
      },
      swap_previous = {
        ["H"] = "@parameter.inner",
      },
    },
  },
}

vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldenable = false
