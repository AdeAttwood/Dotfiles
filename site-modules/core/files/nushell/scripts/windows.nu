
export def ms-devenv [] {
  let env_load_file = nu -c "cd 'C:/Program Files/Microsoft Visual Studio/'; glob '**/vcvars64.bat' | sort | uniq | str join '\n'" | complete | get stdout | lines | input list 'Choose a ENV to load'
  $"@echo off\ncall \"($env_load_file)\"\nset" | save -f ~/msvc-env.bat

  let ms_env = cmd /c ~/msvc-env.bat | complete | get stdout | lines | parse "{key}={value}"
  $env.PATH = ($ms_env | where key == 'Path' | get value | split row ';')
  $ms_env | transpose --ignore-titles -r -d | reject 'Path' 'FILE_PWD' 'PROMPT_INDICATOR' 'CURRENT_FILE' 'PWD' | load-env
}
