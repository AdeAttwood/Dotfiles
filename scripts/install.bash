#!/bin/bash

dotfiles_dir="${DOTFILE_DIR:-$HOME/dotfiles}"

if [ -x "$(command -v zypper)" ]; then
  sudo zypper install --type pattern devel_basis
  sudo zypper install libopenssl-devel git
elif  [ -x "$(command -v apt)" ]; then
  sudo apt-get update
  sudo apt-get install -y pkg-config git
else
  echo "ERROR: Package manager not found. Could not install"
fi

if [ ! -d $dotfiles_dir ]; then
  git clone https://github.com/AdeAttwood/Dotfiles.git $dotfiles_dir
fi

mise_install_path="${MISE_INSTALL_PATH:-$HOME/.local/bin/mise}"
if [ ! -f "$mise_install_path" ]; then
  curl https://mise.run | sh
  eval "$("$mise_install_path" activate bash)"

  mkdir -p  ~/.config/mise
  cp mise.toml ~/.config/mise/config.toml

  mise activate nu > "$dotfiles_dir/site-modules/core/files/nushell/vendor/autoload/mise.local.nu"
  [ -f ~/.bashrc ] && echo "eval \"\$(mise activate bash)\"" >> ~/.bashrc

  if [ ! -s "$DEVPOD_WORKSPACE_ID" ]; then
    (cd "/workspaces/$DEVPOD_WORKSPACE_ID" && mise trust && mise install)
  else
    (cd "$dotfiles_dir" && mise trust && mise install)
  fi
fi

if [ ! -x "$(command -v cargo)" ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  source "$HOME/.cargo/env"
fi

if [ ! -x "$(command -v configz)" ]; then
  cargo install --git https://github.com/AdeAttwood/Configz
fi

if [ ! -s "$DEVPOD_WORKSPACE_ID" ]; then
  git config --global --add safe.directory "/workspaces/$DEVPOD_WORKSPACE_ID"
fi

"$dotfiles_dir/site-modules/core/files/bin/dotfiles" apply bin
