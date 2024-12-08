
local nu_dir = os.getenv "HOME" .. "/.config/nushell"
if not configz.is_file(nu_dir) then
  configz.link(nu_dir, {
    source = os.getenv "PWD" .. "/site-modules/core/files/nushell",
  })
end
