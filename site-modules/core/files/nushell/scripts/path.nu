


export-env {
  $env.GOPATH = if ($env.COMPUTERNAME == "LAFITE") {
    $"D:/Code"
  } else {
    $"($env.HOME)/Code"
  }
  $env.PATH = $env.PATH
    | prepend ([$env.HOME ".cargo" "bin"] | path join)
}

export def --env node-modules-dir-hook [dir: string] {
  let nodePath = ([$dir, "node_modules", ".bin"] | path join)
  if ($nodePath | path exists) {
    $env.PATH = $env.PATH
    | where {|x| not ($x | str contains "node_modules")}
    | prepend $nodePath
  }
}

