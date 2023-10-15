local is_installed = configz.is_directory(os.getenv "HOME" .. "/.nvm")
if is_installed then
  configz.info "Skipping nvm install"
  return
end

configz.download("/tmp/nvm-install.sh", {
  url = "https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh",
  sha256 = "2ed5e94ba12434370f0358800deb69f514e8bce90f13beb0e1b241d42c6abafd",
})

configz.run "chmod +x /tmp/nvm-install.sh && /tmp/nvm-install.sh"
