local systemd_user_dir = os.getenv "HOME" .. "/.config/systemd/user"
local unit_files = {
  "opencode.service",
}

if not configz.is_directory(systemd_user_dir) then
  configz.directory(systemd_user_dir)
end

for _, unit_file in ipairs(unit_files) do
  local target = systemd_user_dir .. "/" .. unit_file
  if not configz.is_file(target) then
    configz.link(target, {
      source = os.getenv "PWD" .. "/site-modules/core/files/systemd/user/" .. unit_file,
    })
  end
end

configz.run [[if command -v systemctl >/dev/null 2>&1; then systemctl --user daemon-reload && systemctl --user enable --now opencode.service; fi]]
