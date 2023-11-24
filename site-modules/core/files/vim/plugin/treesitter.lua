require("nvim-treesitter.configs").setup {
  playground = { enable = true },
  indent = { enable = false },
  rainbow = { enable = true },
  highlight = {
    -- `false` will disable the whole extension
    enable = true,
    additional_vim_regex_highlighting = true,
  },
  ensure_installed = {
    "typescript",
    "javascript",
    "tsx",
    "php",
    "html",
    "go",
    "clojure",
  },
}
