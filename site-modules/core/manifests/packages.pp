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
      'thunderbird',
      'tmux',
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
  ]: }
}
