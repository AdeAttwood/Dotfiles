export def --env 'nvm dir-hook' [dir: string] {
  let file = $"($dir)/.nvmrc"
  if ($file | path exists) and ($file | path type) == "file" {
    let version = (open $file | lines | get 0 | str trim)
    if (nvm list | find $version | is-empty) {
      nvm install $version
      nvm use $version
      npm i -g typescript-language-server typescript emmet-ls cspell vscode-langservers-extracted yarn
    } else {
      nvm use $version | complete | ignore
    }
  }
}
