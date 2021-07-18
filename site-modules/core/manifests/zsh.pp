class core::zsh {
  vcsrepo { "${user_home}/.oh-my-zsh":
    source   => 'git://github.com/robbyrussell/oh-my-zsh.git',
    provider =>  'git',
    user     => $user,
    revision => 'master',
  }

  file { "${user_home}/.zshrc":
    ensure => 'link',
    owner  => $user,
    target => find_file('core/zshrc'),
  }

  file { "${user_home}/.oh-my-zsh/custom/plugins":
    ensure => 'directory',
    owner  => $user,
  }

  file {"${user_home}/.oh-my-zsh/custom/custom":
    ensure  => 'link',
    owner   => $user,
    target  => find_file('core/oh-my-zsh/custom'),
    require => [Vcsrepo["${user_home}/.oh-my-zsh"]],
    force   => true,
  }

  file {"${user_home}/.oh-my-zsh/custom/lib":
    ensure  => 'link',
    owner   => $user,
    target  => find_file('core/oh-my-zsh/lib'),
    require => [Vcsrepo["${user_home}/.oh-my-zsh"]],
    force   => true,
  }

  file {"${user_home}/.oh-my-zsh/custom/custom.zsh":
    ensure  => 'link',
    owner   => $user,
    target  => find_file('core/oh-my-zsh/custom.zsh'),
    require => [Vcsrepo["${user_home}/.oh-my-zsh"]],
    force   => true,
  }

  file {"${user_home}/.oh-my-zsh/custom/plugins/notmuch":
    ensure  => 'link',
    owner   => $user,
    target  => find_file('core/oh-my-zsh/plugins/notmuch'),
    require => [Vcsrepo["${user_home}/.oh-my-zsh"]],
    force   => true,
  }

  vcsrepo { "${user_home}/.oh-my-zsh/custom/plugins/zsh-autosuggestions":
    source   => 'git://github.com/zsh-users/zsh-autosuggestions',
    provider =>  'git',
    user     => $user,
  }

  vcsrepo { "${user_home}/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting":
    source   => 'git://github.com/zsh-users/zsh-syntax-highlighting.git',
    provider => 'git',
    user     =>  $user,
  }
}
