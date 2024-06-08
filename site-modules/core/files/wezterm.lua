local wezterm = require "wezterm"

return {
  -- Use a sexy terminal font with ligatures.
  -- You will need to install the beta version of the font to get the ligatures
  -- https://github.com/intel/intel-one-mono/issues/9#issuecomment-1994958719
  font = wezterm.font {
    family = "Intel One Mono",
  },

  -- The nord theme to fit with everyting else
  color_scheme = 'nord',

  -- Give the font some more line height, just makes thinks look a bit nicer
  line_height = 1.4,

  -- Remove the window boarders so we have a nice clean look
  window_decorations = "NONE",

  -- Who wants their music interrupted every time there is no tab completion
  -- available in the shell, Who wants their music interrupted evert time there
  -- is no tab completion available in the shell
  audible_bell = "Disabled",

  -- Have a really clean UI when there is only one tab open
  hide_tab_bar_if_only_one_tab = true,

  -- Disabled all the padding, this makes vim look a lot nicer when all the
  -- window bars go to the edges of the terminal
  window_padding = { left = 0, right = 0, top = 10, bottom = 0 },

  warn_about_missing_glyphs = false,

  enable_wayland = false,

  keys = {
    -- Bind <CTRL-Backspace> to <CTRL-w> to `werase` in bash. This is to keep
    -- the terminal binding the same to delete a word. The default <CTRL-w> has
    -- the unfortunate conflicting key biding with close tab in chrome. Moving
    -- muscle memory away from <CTRL-w> will be a game changer for me.
    {
      mods = "CTRL",
      key = "Backspace",
      action = wezterm.action.SendKey { mods = "CTRL", key = "w" },
    },
  },
}
