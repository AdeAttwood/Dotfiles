#
# Custom command not found hander to add links to https://command-not-found.com
# this gives a nice way to display how to install al command
#

[[ -e /etc/zsh_command_not_found ]] && source /etc/zsh_command_not_found
[[ -e /usr/share/doc/pkgfile/command-not-found.zsh ]] && source /usr/share/doc/pkgfile/command-not-found.zsh

command_not_found_handler () {
    [[ "$1" == '_'* ]] && return 1

    echo "
Command '$1' is not found.

You can find out how to install it by visiting

https://command-not-found.com/$1
"
    return 127
}
