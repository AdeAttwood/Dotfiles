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
alias fe="vim \"\$(FZF_DEFAULT_COMMAND=\"fd -t f\" fzf --preview-window=top:70% --preview 'bat --style=numbers ---color=always {}')\""

#
# cd in to a porject directory
#
fp() {
	cd ~/Code/src/$(cd ~/Code/src && FZF_DEFAULT_COMMAND="fd -t d --exact-depth 3" fzf --preview-window=top:70% --preview 'bat --style=numbers --color=always {}/README.md')
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
            git diff --color=always $shas
        else
            for sha in $shas; do
                git show --color=always $sha
            done
        fi
    done
}

fl() {
  local format="%C(red)%h%Creset %s %Cgreen(%cr)%Creset %C(bold blue)<%an>%Creset"
  FZF_DEFAULT_COMMAND="git log --color=always --format=\"$format\"" fzf --preview-window=top:70% --ansi --preview 'git show --color=always {1}'
}

#
# Checkout a git branch in a fzf context
#
fco() {
    git checkout "$(git branch -vl | fzf | awk '{print $1}')"
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
