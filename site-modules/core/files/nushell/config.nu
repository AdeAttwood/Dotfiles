if ((sys host | get name) == "Windows") {
  $env.HOME = $env.USERPROFILE
  $env.USER = $env.USERNAME
}

use path.nu *

use fzf.nu *
use neovim.nu *
use prompt.nu *
use sapling.nu *
use nvm.nu *

$env.config = {
  show_banner: false,
  hooks: {
    env_change: {
      PWD: [
        {|_, after| nvm dir-hook $after }
        {|_, after| node-modules-dir-hook $after }
      ]
    }
  }
}

$env.config.show_banner = false
$env.config.shell_integration.osc133 = false

if $env.OS_THEME == "Light" {
  use std/config light-theme
  $env.config.color_config = (light-theme)
}
