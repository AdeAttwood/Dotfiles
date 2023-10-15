local git = require "lib.git"

local nvim_dir = os.getenv "HOME" .. "/.config/nvim"
local nvim_pluing_dir_start = nvim_dir .. "/pack/bundle/start"

if not configz.is_directory(nvim_dir .. "/snippets") then
  configz.link(nvim_dir .. "/snippets", {
    source = os.getenv "PWD" .. "/site-modules/core/files/vim/snippets/snippets",
  })
end

git.repo {
  src = "https://github.com/sbdchd/neoformat",
  target = nvim_pluing_dir_start .. "/neoformat",
  version = "master",
}
