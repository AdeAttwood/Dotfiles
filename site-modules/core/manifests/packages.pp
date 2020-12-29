class core::packages {
  #
  # Main packages to in stall from the archives
  #
  package { [
      'cmake',
      'filezilla',
      'firefox',
      'git',
      'mutt',
      'neovim',
      'notmuch-mutt',
      'notmuch',
      'offlineimap',
      'silversearcher-ag',
      'tmux',
      'bat',
      'urlscan',
      'vim',
      'zsh',
      'build-essential',
      'chromium-browser',
      'keepass2',
      'python-dev',
      'ruby-dev',
      'software-properties-common',
    ]:
      ensure => installed,
  }

  package { 'thunderbird':
    ensure => absent
  }

  exec { 'Link catbat to bat':
    command => 'ln -s /usr/bin/batcat /usr/bin/bat',
    onlyif  => 'test -e /usr/bin/batcat',
    creates => '/usr/bin/bat',
    path    => '/bin:/usr/bin',
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
