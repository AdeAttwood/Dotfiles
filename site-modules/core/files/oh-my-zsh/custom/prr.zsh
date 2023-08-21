# Gets the git project name "owner/repo" from the current directory. This uses
# the directory structure like the `GOPATH` this is aliased to `~s`. For
# example the directory would be `~/github.com/AdeAttwood/Dotfiles`, this would
# return `AdeAttwood/Dotfiles`.
function get_project() {
  echo "$(basename "$(dirname "$PWD")")/$(basename "$PWD")"
}

# Gets the PR number from the current git repo and branch.
function get_pr_number() {
  gh pr view --json=number --jq='.number'
}

alias prr-get='prr get "$(get_project)"/"$(get_pr_number)"'
alias prr-submit='prr submit "$(get_project)"/"$(get_pr_number)"'
alias prr-edit='$EDITOR  ~/.local/share/prr/$(get_project)/$(get_pr_number).prr'

function prr-review() {
  gh pr checkout "$1"
  prr-get
  prr-edit
}
