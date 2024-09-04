local wezterm = require "wezterm"

local scheme = wezterm.get_builtin_color_schemes()
local nord = scheme["nord"]

local function vim_pass_though_action(config)
  return {
    key = config.key,
    mods = config.mods,
    action = wezterm.action_callback(function(win, pane)
      local process_name = pane:get_foreground_process_name()

      -- If we are in vim then we want to send the key to go to the net pain
      if string.match(process_name, "vim") or string.match(process_name, "emacs") then
        win:perform_action({ SendKey = { key = config.key, mods = config.mods } }, pane)
        return
      end

      win:perform_action({ ActivatePaneDirection = config.direction }, pane)
    end),
  }
end

return {
  -- Use a sexy terminal font with ligatures.
  -- You will need to install the beta version of the font to get the ligatures
  -- https://github.com/intel/intel-one-mono/issues/9#issuecomment-1994958719
  font = wezterm.font {
    family = "Intel One Mono",
  },

  -- The nord theme to fit with everyting else
  color_scheme = "nord",
  colors = {
    tab_bar = {
      background = nord.background,
      active_tab = {
        bg_color = "#88c0d0", -- nord.background,
        fg_color = nord.background,
      },
      inactive_tab = {
        bg_color = nord.background,
        fg_color = nord.foreground,
      },
      inactive_tab_hover = {
        bg_color = "#4c566a",
        fg_color = nord.foreground,
        italic = false,
      },
      new_tab = {
        bg_color = nord.background,
        fg_color = nord.foreground,
      },
      new_tab_hover = {
        bg_color = "#4c566a",
        fg_color = nord.foreground,
        italic = false,
      },
    },
  },

  use_fancy_tab_bar = false,

  -- Give the font some more line height, just makes thinks look a bit nicer
  line_height = 1.4,

  -- Remove the window boarders so we have a nice clean look
  window_decorations = "NONE",

  -- Who wants their music interrupted every time there is no tab completion
  -- available in the shell, Who wants their music interrupted evert time there
  -- is no tab completion available in the shell
  audible_bell = "Disabled",

  -- Have a really clean UI when there is only one tab open
  hide_tab_bar_if_only_one_tab = false,

  -- Disabled all the padding, this makes vim look a lot nicer when all the
  -- window bars go to the edges of the terminal
  window_padding = { left = 0, right = 0, top = 10, bottom = 0 },

  warn_about_missing_glyphs = false,

  tab_bar_at_bottom = true,

  enable_wayland = true,

  leader = { key = "b", mods = "CTRL" },

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

    -- tmux features to complete
    --   - Fuzzy finding / switching between pains
    --   - Prompt search back
    --   - Find out the copy and pasteing story
    --   - Is there a "copy mode" like in tmux?
    --   - Nvim split navigation intergation

    -- Pane navigation like vim, this is what I have been using in tmux and how
    -- the finger move
    vim_pass_though_action { key = "h", mods = "CTRL", direction = "Left" },
    vim_pass_though_action { key = "j", mods = "CTRL", direction = "Down" },
    vim_pass_though_action { key = "k", mods = "CTRL", direction = "Up" },
    vim_pass_though_action { key = "l", mods = "CTRL", direction = "Right" },

    -- Split panes with the tmux keys. Again this alrady uses the same
    -- directory as the current pane. Again no shinanigans needed
    { key = "s", mods = "LEADER", action = wezterm.action { SplitVertical = { domain = "CurrentPaneDomain" } } },
    { key = "v", mods = "LEADER", action = wezterm.action { SplitHorizontal = { domain = "CurrentPaneDomain" } } },

    -- Tab navigation via numbers. This already starts a 1 so we don't need
    -- todo any shinangans to make that work better
    { key = "1", mods = "LEADER", action = wezterm.action { ActivateTab = 0 } },
    { key = "2", mods = "LEADER", action = wezterm.action { ActivateTab = 1 } },
    { key = "3", mods = "LEADER", action = wezterm.action { ActivateTab = 2 } },
    { key = "4", mods = "LEADER", action = wezterm.action { ActivateTab = 3 } },
    { key = "5", mods = "LEADER", action = wezterm.action { ActivateTab = 4 } },
    { key = "6", mods = "LEADER", action = wezterm.action { ActivateTab = 5 } },
    { key = "7", mods = "LEADER", action = wezterm.action { ActivateTab = 6 } },
    { key = "8", mods = "LEADER", action = wezterm.action { ActivateTab = 7 } },
    { key = "9", mods = "LEADER", action = wezterm.action { ActivateTab = 8 } },
  },
}
