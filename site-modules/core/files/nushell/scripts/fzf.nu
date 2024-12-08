# FZF Helper Functions
use lib/project-dirs.nu

def fzf-get-project [] {
  cd $"($env.GOPATH)/src/";
  FZF_DEFAULT_COMMAND="fd -t d --exact-depth 3" fzf --preview-window=top:70% --preview 'bat --style=numbers --color=always {}/README.md'
}

export def --env fp [] {
  let dir = fzf-get-project
  if ($dir | str trim | str length) > 0 {
    cd $"($env.GOPATH)/src/($dir)"
  }
}
