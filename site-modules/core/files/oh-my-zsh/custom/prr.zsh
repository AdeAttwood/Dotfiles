# Gets the git project name "owner/repo" from the current directory. This uses
# the directory structure like the `GOPATH` this is aliased to `~s`. For
# example the directory would be `~/github.com/AdeAttwood/Dotfiles`, this would
# return `AdeAttwood/Dotfiles`.
function get_project() {
  echo "$(basename "$(dirname "$PWD")")/$(basename "$PWD")"
}

# Gets the PR number from the current git repo and branch.
function get_pr_number() {
  if [[ ! -z $PRR_NUMBER ]]; then
    echo "$PRR_NUMBER"
    return
  fi

  gh pr view --json=number --jq='.number'
}

alias prr-get='prr get "$(get_project)"/"$(get_pr_number)"'
alias prr-submit='prr submit "$(get_project)"/"$(get_pr_number)"'
alias prr-edit='$EDITOR  ~/.local/share/prr/$(get_project)/$(get_pr_number).prr'


function prr-review() {
  # Check to see if we are in a git repo, if we are then we want to checkout
  # the branch of the PR.
  if git rev-parse --is-inside-work-tree &>/dev/null; then
    gh pr checkout "$1"
  fi

  # Check to see if we are in a sapling directory, if we are then not only do
  # we need to checkout the PR branch but we also need to export the PPR_NUMBER
  if \sl root &>/dev/null; then
    export PRR_NUMBER="$(gh --repo "$(get_project)" pr view  --json=number --jq='.number' $1)"
    \sl goto "remote/$1"
  fi

  prr-get
  prr-edit
}
