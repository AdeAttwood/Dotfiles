local fs = require "lib.fs"

local v_cache = {}

-- Where all the infomation about what configz has installed is stored.
local configz_dir = os.getenv "HOME" .. "/.config/configz/installed/"

-- Check to see if we have the version of a package installed locally
---@param package string
---@param version string
v_cache.is_installed = function(package, version)
  configz.directory(configz_dir)

  if not configz.is_file(configz_dir .. package) then
    return false
  end

  local ok, installed_version = fs.read_file(configz_dir .. package)
  if not ok then
    return false
  end

  return installed_version == version
end

-- Sets a package to be installed.
---@param package string
---@param version string
v_cache.install = function(package, version)
  configz.directory(configz_dir)
  return fs.write_file(configz_dir .. package, version)
end

return v_cache
