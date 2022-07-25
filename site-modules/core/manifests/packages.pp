class core::packages {
  #
  # Main packages to in stall from the archives
  #
  package { [
      'cmake',
      'git',
      'neovim',
      'silversearcher-ag',
      'tmux',
      'bat',
      'zsh',
      'build-essential',
      'software-properties-common',
      'fd-find'
      'fzf'
    ]:
      ensure => installed,
  }

  package { 'thunderbird':
    ensure => absent
  }

  exec { 'Link catbat to bat':
    command  => 'ln -s /usr/bin/batcat /usr/bin/bat',
    onlyif   => 'test -e /usr/bin/batcat',
    creates  => '/usr/bin/bat',
    path     => '/bin:/usr/bin',
    provider => 'shell',
  }

  exec { 'Link fdfind to fd':
    command  => 'ln -s /usr/bin/fdfind /usr/bin/fd',
    onlyif   => 'test -e /usr/bin/fdfind',
    creates  => '/usr/bin/fd',
    path     => '/bin:/usr/bin',
    provider => 'shell',
  }

  # core::packages::npm { [
  #     'grunt',
  #     'intelephense',
  #     'javascript-typescript-langserver',
  #     'prettier',
  #     'typescript-language-server',
  # ]: }

  #
  # Ruby gems to install globally
  #
  core::packages::gem { [
      'neovim',
      'mail',
      'pandoc-ruby',
      'puppet-lint',
  ]: }
}
