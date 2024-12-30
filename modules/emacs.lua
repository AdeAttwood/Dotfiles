local git = require "lib.git"


local emacs_dir
if os.getenv "OS" == "Windows_NT" then
  emacs_dir = os.getenv "HOME" .. "/AppData/Roaming/.emacs.d"
else
  emacs_dir = os.getenv "HOME" .. "/.emacs.d"
end

if not configz.is_directory(emacs_dir .. "/straight") then
  configz.directory(emacs_dir .. "/straight")
end

if not configz.is_file(emacs_dir .. "/init.el") then
  configz.link(emacs_dir .. "/init.el", {
    source = os.getenv "PWD" .. "/site-modules/core/files/emacs/init.el",
  })
end

if not configz.is_directory(emacs_dir .. "/mbwatch") then
  configz.link(emacs_dir .. "/mbwatch", {
    source = os.getenv "PWD" .. "/site-modules/core/files/emacs/mbwatch",
  })
end

if not configz.is_directory(emacs_dir .. "/straight/versions") then
  configz.link(emacs_dir .. "/straight/versions", {
    source = os.getenv "PWD" .. "/site-modules/core/files/emacs/straight/versions",
  })
end


