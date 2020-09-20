#
# This is the main plugin file for the plugin
#
# AUTHOR:  Ade Attwood <code@adeattwood.co.uk>
#

#
# Move all of the emails matching a search term into a folder
#
function notmuch-move() {

    if [[ "$1" == "--help" ]] || [[ "$1" == "-h" ]]; then
        mdman "$dotfiles/oh-my-zsh/custom/plugins/notmuch/docs/notmuch-move.md";
        return 0;
    fi

    if [ -z "$2" ]; then
        echo "No search";
        return 1;
    fi

    local path="$1"; shift;
    local search="$@";

    /usr/bin/notmuch search --output=files "$search" \
        | /usr/bin/xargs -I{} /bin/mv "{}" "$path";
}

#
# Aliases
#
alias nm="notmuch"
alias nmc="notmuch count"
alias nmm="notmuch-move"
alias nmnew="notmuch search tag:untrad"
alias nms="notmuch search"
alias nmt="notmuch tag"

