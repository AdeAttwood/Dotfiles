# Alias sl to s. This is so we can keep the workflow much the same as git. You
# can mostly replace `g` with `s` and you are using sapling instead of git.
alias s="\\sl"
alias sl="\\sl log --remote -l40 -T '\\033[31m{node|short}\\033[0m {truncatelonglines(desc|firstline, 80)} \\033[32m({date|age}) \\033[34;1m<{author|person}> {if(github_pull_request_number, \"\033[31mPR #{github_pull_request_number}\")}\\033[0m\n' | tac"
alias sc="\\sl commit -iv"
alias sd="\\sl diff"
alias ss="\\sl status"

function sl-convert() {
  if ! [ -d .git ]; then
    echo "Not a git repository. You can only convert a git repo in to a sapling"
    return 1
  fi

  if [ -s .sl ]; then
    echo "Already converted to sapling."
    return 1
  fi

  \sl clone --git "file://${PWD}/.git" .tmp && mv .tmp/.sl . && rm -rf .tmp

  if [[ "$(git remote get-url --all origin)" == *"github.com"* ]]; then
    local new_path="$(git remote get-url --all origin | sed 's/git@github.com:/https:\/\/github.com\//')"

    echo ""
    echo "Setting sapling path to '${new_path}'"

    \sl paths --add default "${new_path}"
  else
    echo ""
    echo "[WARNING] Not a github url, you will need to set your path manually"
    echo ""
  fi

  echo ""
  echo ""
  echo "Converted yor git repo to a sapling repo."
  echo ""
  echo "Please run 'sl log' to ensure the command has worked. You will also need to"
  echo "give 'sl config' a check to ensure your path has been set correctly. Then you"
  echo "can remove the .git directory"
}

