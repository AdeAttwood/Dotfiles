
export-env {
  $env.GOPATH = $"($env.HOME)/Code"
  $env.PATH = $env.PATH
    | split row ":"
    | prepend ([$env.HOME ".cargo" "bin"] | path join)
    | str join ":"
}

export def --env node-modules-dir-hook [dir: string] {
  let nodePath = ([$dir, "node_modules", ".bin"] | path join)
  if ($nodePath | path exists) {
    $env.PATH = $env.PATH
    | split row ":"
    | where {|x| not ($x | str contains "node_modules")}
    | prepend $nodePath
    | str join ":"
  }
}

