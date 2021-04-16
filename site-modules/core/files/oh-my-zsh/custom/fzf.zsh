#!/bin/zsh
#
# FZF scripts to help with various tasks
#
# Author:  Ade Attwood <code@adeattwood.co.uk>
# Updated: 2020-12-28
#

#
# Edit commands with file previews using `fd` to search files to exclude files
# to improve performance
#
alias fe="e \$(FZF_DEFAULT_COMMAND=\"fd -t f\" fzf --layout=reverse --preview 'bat --style=numbers ---color=always {}')"
alias fte="te \$(FZF_DEFAULT_COMMAND=\"fd -t f\" fzf --layout=reverse --preview 'bat --style=numbers ---color=always {}')"

#
# cd in to a porject directory
#
fp() {
	cd ~/Code/src/$(cd ~/Code/src && FZF_DEFAULT_COMMAND="fd -t d --exact-depth 3" fzf --layout=reverse --preview 'bat --style=numbers ---color=always {}/README.md')
}
#
# Searching the git history with diff and show preview support. When selecting a
# commit `git show` output is displayed and `CTRL-d` the `git diff` output is
# displayed
#
fshow() {
    local out shas sha q k
    while out=$(
            git log --color=always \
                --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
                fzf --ansi --multi --no-sort --reverse --query="$q" \
                    --print-query --expect=ctrl-d --toggle-sort=\`); do
        q=$(head -1 <<< "$out")
        k=$(head -2 <<< "$out" | tail -1)
        shas=$(sed '1,2d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')
        [ -z "$shas" ] && continue
        if [ "$k" = ctrl-d ]; then
            git diff --color=always $shas | less -R
        else
            for sha in $shas; do
                git show --color=always $sha | less -R
            done
        fi
    done
}

#
# Checkout a git branch in a fzf context
#
fco() {
    git checkout "$(git branch -vl | fzf --layout=reverse | awk '{print $1}')"
}

#
# Interactive `git add` with FZF and diff preview support.
#
fadd() {
    git add $(git -c color.status=always status --short |
                  fzf -m --reverse --ansi --multi --ansi --nth 2..,.. \
                      --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1}) | head -500' |
                  cut -c4- | sed 's/.* -> //')
}
