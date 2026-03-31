


export-env {
  $env.GOPATH = if ($env.COMPUTERNAME? == "LAFITE") {
    $"D:/Code"
  } else {
    $"($env.HOME)/Code"
  }
  $env.PATH = $env.PATH
    | prepend ([$env.HOME ".cargo" "bin"] | path join)

  let bob_path = ([$env.HOME ".local" "share" "bob" "nvim-bin"] | path join)
  if ($bob_path | path exists) {
    $env.PATH = $env.PATH | prepend $bob_path
  }
}


export def --env node-modules-dir-hook [dir: string] {
  let nodePath = ([$dir, "node_modules", ".bin"] | path join)
  if ($nodePath | path exists) {
    $env.PATH = $env.PATH
    | where {|x| not ($x | str contains "node_modules")}
    | prepend $nodePath
  }
}

