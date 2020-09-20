class core::vim {
  file { "${user_home}/.vim":
    ensure => 'link',
    force  => true,
    owner  => $user,
    target => find_file('core/vim'),
  }

  file { "${user_home}/.vimrc":
    ensure => 'link',
    force  => true,
    owner  => $user,
    target => find_file('core/vim/init.vim'),
  }

  file { "${user_home}/.config/nvim":
    ensure => 'link',
    force  => true,
    owner  => $user,
    target => find_file('core/vim'),
  }

  $plugins_start= {
    'base16-vim'     => { url => 'https://github.com/chriskempson/base16-vim.git' },
    'vim-easy-align' => { url => 'https://github.com/junegunn/vim-easy-align.git' },
    'auto-pairs'     => { url => 'git://github.com/jiangmiao/auto-pairs.git' },
    'indent-line'    => { url => 'https://github.com/Yggdroot/indentLine.git' },
    'nerdcommenter'  => { url => 'https://github.com/scrooloose/nerdcommenter.git' },
    'pdv'            => { url => 'https://github.com/tobyS/pdv.git' },
    'vim-airline'    => { url => 'https://github.com/vim-airline/vim-airline' },
    'vim-fugitive'    => { url => 'git://github.com/tpope/vim-fugitive.git' },
    'vim-tmux-navigator'    => { url => 'https://github.com/christoomey/vim-tmux-navigator.git' },
    'ultisnips'    => { url => 'https://github.com/SirVer/ultisnips.git' },
    'vim-airline-themes'    => { url => 'https://github.com/vim-airline/vim-airline-themes.git' },
    'vim-javascript'    => { url => 'https://github.com/pangloss/vim-javascript.git' },
    'vim-jsx'    => { url => 'https://github.com/mxw/vim-jsx.git' },
    'command-t'    => { url => 'https://github.com/wincent/command-t.git' },
    'ferret'    => { url => 'https://github.com/wincent/ferret.git' },
    #
    # Nerdtree has now been loaded after vim start up
    #
    'nerdtree'       => {
      ensure => 'absent',
      url    => 'https://github.com/scrooloose/nerdtree.git',
    },
  }

  exec { 'Compile command t':
      path     => [ '/bin/', '/sbin/' , '/usr/bin/', '/usr/sbin/' ],
      command  => 'ruby extconf.rb && make',
      cwd      => "${find_file('core/vim')}/pack/bundle/start/command-t/ruby/command-t/ext/command-t",
      provider => 'shell',
      #require  => [Vim::Plugin_start[command-t]],
  }

  create_resources(core::vim::plugin_start, $plugins_start)

  $plugins_opt= {
    'emmet-vim'     => { url => 'https://github.com/mattn/emmet-vim.git' },
    'nerdtree' => { url => 'https://github.com/scrooloose/nerdtree.git' },
  }

  create_resources(core::vim::plugin_opt, $plugins_opt)
}
