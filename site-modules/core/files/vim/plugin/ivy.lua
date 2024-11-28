local config = require "ivy.config"

-- Set up ivy.nvim
-- See: https://github.com/AdeAttwood/ivy.nvim
require("ivy").setup {
  backends = {
    "ivy.backends.buffers",
    "ivy.backends.files",
    "ivy.backends.lines",
    "ivy.backends.lsp-workspace-symbols",
    "ivy.backends.rg",
  },
  mappings = vim.tbl_extend("force", config:get { "mappings" }, {
    ["<C-M-n>"] = "next_checkpoint",
    ["<C-M-p>"] = "previous_checkpoint",
  }),
}
