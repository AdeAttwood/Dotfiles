local v_cache = require "lib.v-cache"

if v_cache.is_installed("lua-language-server", "3.6.21") then
  configz.info "Skipping lua lsp install, its already installed!"
  return
end

configz.directory(os.getenv "HOME" .. "/.local/share/lua-lsp")

configz.download(
  os.getenv "HOME" .. "/.local/share/lua-lsp/language-server.tar.gz",
  -- TODO: Add sha to the download
  {
    url = "https://github.com/LuaLS/lua-language-server/releases/download/3.6.21/lua-language-server-3.6.21-linux-x64.tar.gz",
  }
)

local ok = configz.run "cd $HOME/.local/share/lua-lsp && tar -xzf language-server.tar.gz"
if not ok then
  return false
end

v_cache.install("lua-language-server", "3.6.21")
