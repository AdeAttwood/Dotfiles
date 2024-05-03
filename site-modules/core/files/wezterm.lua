local wezterm = require "wezterm"

-- Base16 Theme: Tomorrow
-- local window_frame_fg = "#000000"
-- local window_frame_bg = "#ffffff"

-- Base16 Theme: Tomorrow Night
local window_frame_bg = "#1d1f21"
local window_frame_fg = "#ffffff"

return {
  -- Use a sexy terminal font with ligatures.
  --font = wezterm.font("Liga SFMono Nerd Font"),
  font = wezterm.font {
    family = "CommitMono",
  },

  -- Give the font some more line height, just makes thinks look a bit nicer
  line_height = 1.4,

  -- Who wants their music interrupted every time there is no tab completion
  -- available in the shell, Who wants their music interrupted evert time there
  -- is no tab completion available in the shell
  audible_bell = "Disabled",

  -- Have a really clean UI when there is only one tab open
  hide_tab_bar_if_only_one_tab = true,

  -- Disabled all the padding, this makes vim look a lot nicer when all the
  -- window bars go to the edges of the terminal
  window_padding = { left = 0, right = 0, top = 0, bottom = 0 },

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

  window_frame = {
    inactive_titlebar_bg = window_frame_bg,
    active_titlebar_bg = window_frame_bg,
    inactive_titlebar_fg = window_frame_fg,
    active_titlebar_fg = window_frame_fg,
    inactive_titlebar_border_bottom = window_frame_bg,
    active_titlebar_border_bottom = window_frame_bg,
    button_fg = window_frame_fg,
    button_bg = window_frame_bg,
    button_hover_fg = window_frame_bg,
    button_hover_bg = window_frame_fg,
  },
}
