local git = require "lib.git"

local plugin_dir = os.getenv "HOME" .. "/.tmux/plugins"
local config_file = os.getenv "HOME" .. "/.tmux.conf"

if not configz.is_file(config_file) then
  configz.link(config_file, {
    source = os.getenv "PWD" .. "/site-modules/core/files/tmux.conf",
  })
end

if not configz.is_directory(plugin_dir) then
  configz.directory(plugin_dir)
end

local plugins = {
  ["tmux-yank"] = { url = "https://github.com/tmux-plugins/tmux-yank.git" },
  ["tmux-open"] = { url = "https://github.com/tmux-plugins/tmux-open.git" },
  ["tmux-copycat"] = { url = "https://github.com/tmux-plugins/tmux-copycat.git" },
  ["base16-tmux"] = { url = "https://github.com/tinted-theming/base16-tmux.git", revision = "main" },
}

for plugin, config in pairs(plugins) do
  git.repo {
    src = config.url,
    target = plugin_dir .. "/" .. plugin,
    version = config.revision or "master",
  }
end
