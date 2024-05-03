#!/bin/bash

if [ -x "$(command -v zypper)" ]; then
  sudo zypper install --type pattern devel_basis
  sudo zypper install libopenssl-devel git
else
  echo "ERROR: Package manager not found. Could not install"
fi

if [ ! -x "$(command -v cargo)" ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  source "$HOME/.cargo/env"
fi

if [ ! -x "$(command -v configz)" ]; then
  cargo install --git https://github.com/AdeAttwood/Configz
fi

if [ ! -d ~/Code/src/github.com/AdeAttwood/Dotfiles ]; then
  git clone https://github.com/AdeAttwood/Dotfiles.git ~/Code/src/github.com/AdeAttwood/Dotfiles
fi

echo "cd ~/Code/src/github.com/AdeAttwood/Dotfiles"
