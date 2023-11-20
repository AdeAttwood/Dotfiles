local oil = require "oil"

-- A list of file or directory names that will never be shown in the oil view
local always_hidden = { [".git"] = true }

oil.setup {
  view_options = {
    show_hidden = true,
    is_always_hidden = function(name)
      return always_hidden[name] or false
    end,
  },
}

vim.keymap.set("n", "-", oil.open, { desc = "Open parent directory" })
