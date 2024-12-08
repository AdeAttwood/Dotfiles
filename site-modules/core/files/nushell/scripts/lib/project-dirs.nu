# Project directories are directories that contain a `.git` directory. They are
# structured in the style of GOPATH. This module contains functions to get the
# relevant project information from the current directory.

export-env {
  $env.GOPATH = $env.HOME + "/Code"
}

# Get the project information from a path. The path can be passed $in and the
# project information will be returned.
#
# > $"($env.GOPATH)/src/github.com/nushell/nushell" | project
# ╭───────┬────────────────────────────────────╮
# │ host  │ github.com                         │
# │ owner │ nushell                            │
# │ repo  │ nushell                            │
# │ path  │ nushell/nushell                    │
# │ url   │ https://github.com/nushell/nushell │
# ╰───────┴────────────────────────────────────╯
#
# If you are looking to get the project for the directory you'r in, you can use
# the helper function `current-project`.
export def project [] {
  let path = $in
  let parts = $path | str replace $"($env.GOPATH)/src/" "" | split row "/"

  {
    host: $parts.0,
    owner: $parts.1,
    repo: $parts.2,
    path: $"($parts.1)/($parts.2)"
    url: $"https://($parts.0)/($parts.1)/($parts.2)"
  }
}

# Helper function to get the project information from your current directory.
# This is a simple wrapper around the `project` function.
export def current-project [] { $env.PWD | project }
