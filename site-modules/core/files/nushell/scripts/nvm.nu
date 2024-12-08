
def 'nvm path' [subdir?: string]: nothing -> string {
  if ($subdir | into string | str length) == 0 {
    return $"($env.HOME)/.nvm"
  }

  $"($env.HOME)/.nvm/($subdir)"
}

export def 'nvm get-alias' [alias: string]: nothing -> string {
  mut alias_version = if ($alias | find "stable" | is-not-empty)  {
    ["lts/*"]
  } else {
    nvm aliases | where {|x| $x == $alias}
  }

  if $alias_version  == ["stable"] {
    $alias_version = ["lts/*"]
  }

  if ($alias_version | is-empty) {
    return null
  }

  if ($alias_version | length) > 1 {
    $alias_version = [$"($alias_version | first)/*"]
  }

  let version = (open ([(nvm path "alias"), ($alias_version | first)] | path join)) | str trim
  if not ($version | str starts-with "v") {
    return (nvm get-alias $version)
  }

  $version
}

def 'nvm index' [] {
  http get 'https://nodejs.org/dist/index.json' | get version
}

export def 'nvm aliases' [] {
  ls -a ...(glob (nvm path "alias/**/*"))
    | each {|p| $p.name | path relative-to (nvm path "alias") }
    | filter {|p| not ($p | str ends-with "*")}
    | uniq
}

export def 'nvm list' [] {
  ls -a (nvm path "versions/node")
    | each {|p| $p.name | path relative-to (nvm path "versions/node") }
    | filter {|p| not ($p | str ends-with "*")}
}

export def 'nvm resolve' [version: string] {
  let resolved_version = (nvm list | find $version)
  let resolved_alias = (nvm get-alias $version)
  if (($resolved_version | length) != 1) and ($resolved_alias | is-empty) {
    (error make --unspanned { msg: $"Unable to resolve '($version)'" })
  }

  if not ($resolved_version | is-empty) {
    $resolved_version | first
  } else {
    $resolved_alias
  }
}

export def --env 'nvm use' [version: string] {
  let resolved = (nvm resolve $version)
  let node_path = [(nvm path "versions/node") $resolved] | path join

  $env.PATH = $env.PATH
    | split row ":"
    | where {|x| not ($x | str starts-with (nvm path "versions/node"))}
    | prepend ([$node_path "bin"] | path join)
    | str join ":"
}

export def --env 'nvm dir-hook' [dir: string] {
  let file = $"($dir)/.nvmrc"
  if ($file | path exists) and ($file | path type) == "file" {
    nvm use (open $file | lines | get 0) | str trim
  }
}
