local git = require('lib.git')

local nvim_pluing_dir_start = os.getenv("HOME") .. "/.config/nvim/pack/bundle/start"

git.repo({
  src = "https://github.com/sbdchd/neoformat",
  target = nvim_pluing_dir_start .. "/neoformat",
  version = "master"
})
