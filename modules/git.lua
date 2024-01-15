local data = require "lib.data"

configz.template(os.getenv "HOME" .. "/.gitconfig", {
  source = os.getenv "PWD" .. "/site-modules/core/templates/gitconfig.liquid",
  data = {
    user_name = data.lookup("common", "user_name"),
    email = data.lookup("personal", "email"),
  },
})
