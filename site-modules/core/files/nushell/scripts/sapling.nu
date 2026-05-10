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

# Configure the current repository for the preferred Sapling PR workflow.
export def sl-configure [
  branch?: string # Development branch to use, auto-detected when omitted
] {
  let development_branch = if ($branch | is-empty) {
    let remote_head = (git symbolic-ref --quiet --short refs/remotes/origin/HEAD | complete)

    if $remote_head.exit_code == 0 {
      $remote_head.stdout | str trim | str replace --regex '^origin/' ''
    } else {
      let remote_refs = (git for-each-ref '--format=%(refname:short)' refs/remotes/origin | lines | each {|ref| $ref | str trim | str replace --regex '^origin/' '' })
      let local_refs = (git for-each-ref '--format=%(refname:short)' refs/heads | lines | each {|ref| $ref | str trim })
      let candidates = [development main master 0.x]
      let detected_remote = ($candidates | where {|candidate| $candidate in $remote_refs } | first)
      let detected_local = if ($detected_remote | is-empty) {
        $candidates | where {|candidate| $candidate in $local_refs } | first
      } else {
        null
      }
      let current_branch = (git symbolic-ref --quiet --short HEAD | complete)
      let detected = if (not ($detected_remote | is-empty)) {
        $detected_remote
      } else if (not ($detected_local | is-empty)) {
        $detected_local
      } else if ($current_branch.exit_code == 0 and (($current_branch.stdout | str trim) in $candidates)) {
        $current_branch.stdout | str trim
      } else {
        null
      }

      if ($detected | is-empty) {
        error make {
          msg: "Could not detect development branch. Pass one explicitly, e.g. sl-configure development"
        }
      }

      $detected
    }
  } else {
    $branch
  }

  _sl config --local $"remotenames.publicheads=remote/($development_branch),origin/($development_branch)" $"remotenames.selectivepulldefault=($development_branch)" $"smartlog.master=($development_branch)" github.pr_workflow=single github.preferred_submit_command=pr

  print $"Configured Sapling for ($development_branch)"
}
