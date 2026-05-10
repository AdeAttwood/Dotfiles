local opencode_dir = os.getenv "HOME" .. "/.config/opencode"

local config = opencode_dir .. "/opencode.json"
if not configz.is_file(config) then
  configz.link(config, {
    source = os.getenv "PWD" .. "/site-modules/core/files/opencode/opencode.json",
  })
end

local skills = opencode_dir .. "/skills"
if not configz.is_directory(skills) then
  configz.link(skills, {
    source = os.getenv "PWD" .. "/site-modules/core/files/opencode/skills",
  })
end

local commands = opencode_dir .. "/commands"
if not configz.is_directory(commands) then
  configz.link(commands, {
    source = os.getenv "PWD" .. "/site-modules/core/files/opencode/commands",
  })
end

local plugins = opencode_dir .. "/plugins"
if not configz.is_directory(plugins) then
  configz.link(plugins, {
    source = os.getenv "PWD" .. "/site-modules/core/files/opencode/plugins",
  })
end
