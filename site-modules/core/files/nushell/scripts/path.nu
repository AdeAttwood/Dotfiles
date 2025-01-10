
export-env {
  $env.GOPATH = $"($env.HOME)/Code"
  $env.PATH = $env.PATH
    | split row ":"
    | prepend ([$env.HOME ".cargo" "bin"] | path join)
    | str join ":"
}
