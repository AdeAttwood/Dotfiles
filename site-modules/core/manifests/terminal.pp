class core::terminal {
  vcsrepo { "${user_home}/.config/base16-shell":
    ensure   => latest,
    source   => 'https://github.com/tinted-theming/base16-shell.git',
    provider => git,
    revision => 'main',
  }
}
