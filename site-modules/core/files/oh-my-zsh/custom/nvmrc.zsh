#!/bin/zsh
#
# Custom plugin to check the .nvmrc file. When entering a directory, it checks
# to see if you have the desired node version installed for the current
# project. If it is, it will use it automatically, if not, then it will ask you
# if you want to install it. After, it will install all the JS developer tools
# for that version of NodeJS.
#
# This is a different implementation than the default oh-my-zsh version. The
# main difference is that it will not reset the node version when leaving a
# directory. It will only preform an action if there is a `.nvmrc` file in the
# current directory. It will not even use the `nvm_find_nvmrc` command because
# any interaction with nvm make the stranded `cd` command extremely sluggish.

maybe_nvm_install() {
  local version="$1"

  if read -q "do?Node version ${version} is not installed, would you like to install it? (Y/N) "; then
    echo ""
    nvm install "${version}"

    echo "Installing all of the JS developer tools for this version of node with npm"
    npm install -g yarn typescript-language-server typescript emmet-ls cspell vscode-langservers-extracted prettier jsdoc eslint
  fi
}

load_nvmrc() {
  local nvmrc_path=".nvmrc"

  if [[ -f "$nvmrc_path" ]]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [[ "$nvmrc_node_version" = "N/A" ]]; then
      maybe_nvm_install "$(cat "${nvmrc_path}")"
    else
      nvm use > /dev/null
    fi
  fi
}

autoload -U add-zsh-hook
add-zsh-hook chpwd load_nvmrc

load_nvmrc
