use path.nu

use fzf.nu *
use neovim.nu *
use prompt.nu *
use sapling.nu *
use nvm.nu *

nvm use default

$env.config = {
  show_banner: false,
  hooks: {
    env_change: {
      PWD: [
        {|_, after| nvm dir-hook $after }
      ]
    }
  }
}

