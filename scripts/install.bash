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

if [ ! -x "$(command -v cargo)" ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  source "$HOME/.cargo/env"
fi

if [ ! -x "$(command -v configz)" ]; then
  cargo install --git https://github.com/AdeAttwood/Configz
fi

if [ ! -d $dotfiles_dir ]; then
  git clone https://github.com/AdeAttwood/Dotfiles.git $dotfiles_dir
fi

"$dotfiles_dir/site-modules/core/files/bin/dotfiles" apply bin
