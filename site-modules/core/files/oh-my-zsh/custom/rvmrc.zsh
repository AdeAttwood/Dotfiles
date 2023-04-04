#!/bin/zsh
#
# Plugin to manage ruby versions with rvm by reading the `.ruby-version` and
# `.ruby-gemset` files to install and setup your ruby version and the gemset.
#
# Like the custom nvmrc one this will only do something if you have the
# `.ruby-version` file in the current directory to try and reduce the amount of
# work that gets done on `cd`

maybe_ruby_install() {
  local version="$1"

  if read -q "do?Ruby version ${version} is not installed, would you like to install it? (Y/N) "; then
    echo ""
    rvm install "${version}"
  fi

  echo ""
}

load_rubyrc() {
  local ruby_version_path=".ruby-version"
  local ruby_gemset_path=".ruby-gemset"

  if [[ -f "$ruby_version_path" ]]; then
    local ruby_version="$(cat "${ruby_version_path}")"

    if [[ -z "$(rvm list strings | grep "$(cat .ruby-version)")" ]]; then
      maybe_ruby_install "$ruby_version"
    else
      rvm use "$ruby_version" > /dev/null
    fi
  fi

  if [[ -f "$ruby_gemset_path" ]]; then
    rvm gemset use --create "$(cat "$ruby_gemset_path")"
  fi
}

autoload -U add-zsh-hook
add-zsh-hook chpwd load_rubyrc

load_rubyrc
