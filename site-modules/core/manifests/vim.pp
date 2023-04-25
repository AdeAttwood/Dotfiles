class core::vim {
  file { "${user_home}/.config/nvim":
    ensure => 'directory',
    force  => true,
    owner  => $user,
  }

  file { "${user_home}/.config/nvim/after":
    ensure => 'link',
    force  => true,
    owner  => $user,
    target => find_file('core/vim/after'),
  }

  file { "${user_home}/.config/nvim/ftplugin":
    ensure => 'link',
    force  => true,
    owner  => $user,
    target => find_file('core/vim/ftplugin'),
  }

  file { "${user_home}/.config/nvim/plugin":
    ensure => 'link',
    force  => true,
    owner  => $user,
    target => find_file('core/vim/plugin'),
  }

  file { "${user_home}/.config/nvim/spell":
    ensure => 'link',
    force  => true,
    owner  => $user,
    target => find_file('core/vim/spell'),
  }

  file { "${user_home}/.config/nvim/init.vim":
    ensure => 'link',
    force  => true,
    owner  => $user,
    target => find_file('core/vim/init.vim'),
  }

  $plugins_start= {
    'auto-pairs'         => { url => 'https://github.com/jiangmiao/auto-pairs.git' },
    'base16-vim'         => { url => 'https://github.com/tinted-theming/base16-vim.git', revision => 'main' },
    'cmp_luasnip'        => { url => 'https://github.com/saadparwaiz1/cmp_luasnip.git' },
    'cmp-buffer'         => { url => 'https://github.com/hrsh7th/cmp-buffer.git', revision => 'main' },
    'cmp-nvim-lsp'       => { url => 'https://github.com/hrsh7th/cmp-nvim-lsp.git', revision => 'main' },
    'cmp-path'           => { url => 'https://github.com/hrsh7th/cmp-path.git', revision => 'main' },
    'Comment.nvim'       => { url => 'https://github.com/numToStr/Comment.nvim.git' },
    'ferret'             => { url => 'https://github.com/wincent/ferret.git' },
    'indent-line'        => { url => 'https://github.com/Yggdroot/indentLine.git' },
    'LuaSnip'            => { url => 'https://github.com/L3MON4D3/LuaSnip.git' },
    'nvim-cmp'           => { url => 'https://github.com/hrsh7th/nvim-cmp.git', revision => 'main' },
    'nvim-lint'          => { url => 'https://github.com/mfussenegger/nvim-lint.git' },
    'nvim-lspconfig'     => { url => 'https://github.com/neovim/nvim-lspconfig.git' },
    'nvim-treesitter'    => { url => 'https://github.com/nvim-treesitter/nvim-treesitter.git' },
    'orgmode'            => { url => 'https://github.com/nvim-orgmode/orgmode.git' },
    'vim-puppet'         => { url => 'https://github.com/rodjek/vim-puppet.git' },
    'vim-surround'       => { url => 'https://github.com/tpope/vim-surround.git' },
    'vim-tmux-navigator' => { url => 'https://github.com/christoomey/vim-tmux-navigator.git' },
    'vim-fugitive'       => { url => 'https://github.com/tpope/vim-fugitive.git' },
    'vim-rhubarb'        => { url => 'https://github.com/tpope/vim-rhubarb.git' },
    'conjure'            => { url => 'https://github.com/Olical/conjure.git' }
  }

  create_resources(core::vim::plugin_start, $plugins_start)

  $plugins_opt= {
    'command-t' => { url => 'https://github.com/wincent/command-t.git' },
  }

  create_resources(core::vim::plugin_opt, $plugins_opt)
}
