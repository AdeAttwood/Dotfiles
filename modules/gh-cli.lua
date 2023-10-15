local v_cache = require "lib.v-cache"

if v_cache.is_installed("gh-cli", "v2.29.0") then
  return
end

configz.download("/tmp/gh-cli.tar.gz", {
  url = "https://github.com/cli/cli/releases/download/v2.29.0/gh_2.29.0_linux_amd64.tar.gz",
  sha256 = "9fe05f43a11a7bf8eacf731422452d1997e6708d4160ef0efcb13c103320390e",
})

configz.run "cd /tmp; tar -xzf /tmp/gh-cli.tar.gz"
configz.file(os.getenv "HOME" .. "/.local/bin/gh", { source = "/tmp/gh_2.29.0_linux_amd64/bin/gh" })
v_cache.install("gh-cli", "v2.29.0")
