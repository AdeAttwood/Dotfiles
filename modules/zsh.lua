local git = require "lib.git"

local zsh_dir = os.getenv "HOME" .. "/.oh-my-zsh"

git.repo { src = "https://github.com/ohmyzsh/ohmyzsh.git", target = zsh_dir, version = "master" }

local zshrc = os.getenv "HOME" .. "/.zshrc"
if not configz.is_file(zshrc) then
  configz.link(zshrc, {
    source = os.getenv "PWD" .. "/site-modules/core/files/zshrc",
  })
end

configz.directory(zsh_dir .. "/custom/plugins")

if not configz.is_directory(zsh_dir .. "/custom/custom") then
  configz.link(zsh_dir .. "/custom/custom", {
    source = os.getenv "PWD" .. "/site-modules/core/oh-my-zsh/custom",
  })
end

if not configz.is_directory(zsh_dir .. "/custom/lib") then
  configz.link(zsh_dir .. "/custom/lib", {
    source = os.getenv "PWD" .. "/site-modules/core/oh-my-zsh/lib",
  })
end

if not configz.is_file(zsh_dir .. "/custom/custom.zsh") then
  configz.link(zsh_dir .. "/custom/custom.zsh", {
    source = os.getenv "PWD" .. "/site-modules/core/oh-my-zsh/custom.zsh",
  })
end

if not configz.is_directory(zsh_dir .. "/custom/plugins/notmuch") then
  configz.link(zsh_dir .. "/custom/plugins/notmuch", {
    source = os.getenv "PWD" .. "/site-modules/core/oh-my-zsh/plugins/notmuch",
  })
end

git.repo {
  src = "https://github.com/zsh-users/zsh-autosuggestions",
  target = zsh_dir .. "/custom/plugins/zsh-autosuggestions",
  version = "master",
}

git.repo {
  src = "https://github.com/zsh-users/zsh-syntax-highlighting.git",
  target = zsh_dir .. "/custom/plugins/zsh-syntax-highlighting",
  version = "master",
}
