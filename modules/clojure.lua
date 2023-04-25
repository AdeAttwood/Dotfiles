-- Configz module for the clojure dev tools

local function install_clojure_lsp()
  configz.download("/tmp/closure-lsp.zip", {
    sha256 = "c23a5c9029b3a548a6b8e66a0662103c13e44f220ad8e3f97abf0b7c53a994b1",
    url = "https://github.com/clojure-lsp/clojure-lsp/releases/download/2023.02.27-13.12.12/clojure-lsp-native-static-linux-amd64.zip",
  })

  configz.run "cd /tmp; unzip /tmp/closure-lsp.zip"
  configz.file(os.getenv "HOME" .. "/.local/bin/clojure-lsp", { source = "/tmp/clojure-lsp" })
end

local function has_correct_version(command, version)
  local ok, _ = configz.run(command .. " --version | grep " .. version)
  return ok
end

local clojure_lsp_installed, clojure_lsp = configz.get_executable "clojure-lsp"
if not clojure_lsp_installed or not has_correct_version(clojure_lsp, "2023.02.27-13.12.12") then
  install_clojure_lsp()
end

local function install_babashka()
  configz.download("/tmp/babashka.tar.gz", {
    sha256 = "46c866c28ea9d99a5544c75c554b0c1e22edfa20843d927d3c175b7021ca7252",
    url = "https://github.com/babashka/babashka/releases/download/v1.3.176/babashka-1.3.176-linux-amd64.tar.gz",
  })

  configz.run "cd /tmp; tar -xzf babashka.tar.gz"
  configz.file(os.getenv "HOME" .. "/.local/bin/bb", { source = "/tmp/bb" })
end

local bb_installed, bb = configz.get_executable "bb"
if not bb_installed or not has_correct_version(bb, "1.3.176") then
  install_babashka()
end
