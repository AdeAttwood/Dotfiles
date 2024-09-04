-- Set up ivy.nvim
-- See: https://github.com/AdeAttwood/ivy.nvim
require('ivy').setup {
  backends = {
    "ivy.backends.buffers",
    "ivy.backends.files",
    "ivy.backends.lines",
    "ivy.backends.lsp-workspace-symbols",
    "ivy.backends.rg",
  },
}
