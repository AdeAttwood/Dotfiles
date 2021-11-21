#
# Custom command not found hander to add links to https://command-not-found.com
# this gives a nice way to display how to install al command
#
# The not found command are recorded before displaying the messages for the
# error. This will allow you to see the command that you are getting incorrect
# more and maybe add in alias for them. The below command will get all of the
# incorrect commands and sort them by the most used.
#
# cat ~/.command-not-found | cut -d" " -f 2 | sort | uniq -c | sort -n
#

[[ -e /etc/zsh_command_not_found ]] && source /etc/zsh_command_not_found
[[ -e /usr/share/doc/pkgfile/command-not-found.zsh ]] && source /usr/share/doc/pkgfile/command-not-found.zsh

command_not_found_handler () {
    [[ "$1" == '_'* ]] && return 1
	echo "$(date +%s) $@" >> $HOME/.command-not-found;

    echo "
Command '$1' is not found.

You can find out how to install it by visiting

https://command-not-found.com/$1
"
    return 127
}
