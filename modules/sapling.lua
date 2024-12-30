local data = require "lib.data"

local config_dir
if os.getenv "OS" == "Windows_NT" then
  config_dir = os.getenv "HOME" .. "/AppData/Roaming/sapling"
else
 config_dir = os.getenv "HOME" .. "/.config/sapling"
end


configz.directory(config_dir);

configz.template(config_dir .. "/sapling.conf", {
  source = os.getenv "PWD" .. "/site-modules/core/templates/sapling.conf.liquid",
  data = {
    user_name = data.lookup("common", "user_name"),
    email = data.lookup("personal", "email"),
  },
})
