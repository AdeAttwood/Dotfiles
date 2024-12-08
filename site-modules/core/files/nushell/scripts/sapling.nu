# Helper functions and aliases for working with sapling scm
#
# https://sapling-scm.com/

# Internal alias to rename sl
export alias _sl = sl

# Sapling CLI tool, aliases for `sl`
export alias s = _sl

# Sapling log for your current stack
export alias sl = _sl log --pager never --remote -r '.::top() or last(::., 40)'

# Sapling diff
export alias sd = _sl diff

# Sapling status
export alias ss = _sl status

# Wrapper arround `sl addremove` and `sl amend` commands
export def --wrapped sa [
  ...args # Agguments you can pass to `sl amend` command
] {
  _sl addremove '.'
  _sl amend -iv ...$args
}

# Wrapper arround `sl addremove` and `sl commit` commands
export def --wrapped sc [
  ...args # Agguments you can pass to `sl commit` command
] {
  _sl addremove '.'
  _sl commit -iv ...$args
}

export def sco [] {
  let raw_commit = (_sl log -r 'heads(draft())' | fzf --ansi)
  if ($raw_commit | str length) > 0 {
    let commit = ($raw_commit | split words | get 1);
    _sl goto $commit
  }
}
