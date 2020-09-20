class core::terminal {
  file { "${user_home}/.config/alacritty":
    ensure => 'directory',
  }

  file { "${user_home}/.config/alacritty/alacritty.yml":
    ensure => 'link',
    target => find_file('core/alacritty.yml'),
  }

  vcsrepo { "${user_home}/.config/base16-shell":
    ensure   => latest,
    source   => 'https://github.com/chriskempson/base16-shell.git',
    provider => git,
    revision => 'master',
  }
}
