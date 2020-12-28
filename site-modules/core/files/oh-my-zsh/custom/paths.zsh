#!/bin/zsh
#
# Update global path variable when moving around the file system to include
# local project bin directories. When opening the shell in a project with a
# compatible bin directory i.e. `vendor/bin` for PHP composer packages, the
# directory will be added to the `PATH` variable so that the locally installed
# packages can be executed
#
# Compatible directories
#   - vendor/bin
#   - node_modules/.bin
#
# Author:  Ade Attwood <code@adeattwood.co.uk>
# Updated: 2020-12-28
#

#
# Update paths function to be called when `cd` is called
#
update_path() {
    if [[ -d ./vendor/bin ]]; then
        export PATH=$PATH:./vendor/bin
    else
        path=( ${path[@]:#*./vendor/bin*} )
    fi

    if [[ -d ./node_modules/.bin ]]; then
        export PATH=$PATH:./node_modules/.bin
    else
        path=( ${path[@]:#*./node_modules/.bin*} )
    fi
}

#
# Call the update path function whenever the current working directory is
# changed
#
autoload -U add-zsh-hook
add-zsh-hook chpwd update_path

#
# Call update path when zsh is loaded. If the shell is initialised in a
# directory when local bin directory the directory will not be added to the path
# because the `chpwd` hook has not been called
#
update_path

#
# Remove duplicate entries from the path variable
#
typeset -U path
