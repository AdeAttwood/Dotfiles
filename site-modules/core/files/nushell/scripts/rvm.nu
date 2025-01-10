# Plugin to manage ruby versions with rvm by reading the `.ruby-version` and
# `.ruby-gemset` files to install and setup your ruby version and the gemset.
#
# Like the custom nvmrc one this will only do something if you have the
# `.ruby-version` file in the current directory to try and reduce the amount of
# work that gets done on `cd`

def file-exists [file: string] {
  ($file | path exists) and ($file | path type) == "file"
}

export def --env rvm-use [version: string, gemset?: string] {
  let env_map = if ($gemset | is-empty) {
    (bash --login -c $"rvm use ($version); env" | lines | split column "=" | group-by column1)
  } else {
    (bash --login -c $"rvm use ($version); rvm gemset use --create ($gemset); env" | lines | split column "=" | group-by column1)
  }

  let old_env = $env.PATH | split row ":" | filter { not ($in | str contains "rvm") }
  let new_env = $env_map.PATH.column2 | split row ":" | filter { $in | str contains "rvm" }

  load-env {
    RUBY_VERSION: ($env_map.RUBY_VERSION.column2 | first | str trim),
    GEM_HOME: ($env_map.GEM_HOME.column2 | first | str trim),
    GEM_PATH: ($env_map.GEM_PATH.column2 | first | str trim),
    IRBRC: ($env_map.IRBRC.column2 | first | str trim),
    MY_RUBY_HOME: ($env_map.MY_RUBY_HOME.column2 | first | str trim),
    rvm_path: ($env_map.rvm_path.column2 | first | str trim),
    PATH: ($old_env | prepend $new_env | uniq | str join ":"),
  }
}

export def --env rvm-dir-hook [dir: string] {
  let ruby_version_file = $"($dir)/.ruby-version"
  let ruby_gemset_file = $"($dir)/.ruby-gemset"
  if (file-exists $ruby_gemset_file) {
    rvm-use (open $ruby_version_file | lines | get 0) (open $ruby_gemset_file | lines | get 0)
  } else if (file-exists $ruby_version_file) {
    rvm-use (open $ruby_version_file | lines | get 0) ""
  }
}
