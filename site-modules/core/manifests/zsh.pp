class core::zsh {

  exec { 'git clone ohmyzsh':
    creates => "${user_home}/.oh-my-zsh",
    path    => '/usr/bin:/usr/sbin:/bin',
    command => "git clone git://github.com/robbyrussell/oh-my-zsh.git ${user_home}/.oh-my-zsh",
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
    require => [Exec['git clone ohmyzsh']],
    force   => true,
  }

  file {"${user_home}/.oh-my-zsh/custom/lib":
    ensure  => 'link',
    owner   => $user,
    target  => find_file('core/oh-my-zsh/lib'),
    require => [Exec['git clone ohmyzsh']],
    force   => true,
  }

  file {"${user_home}/.oh-my-zsh/custom/custom.zsh":
    ensure  => 'link',
    owner   => $user,
    target  => find_file('core/oh-my-zsh/custom.zsh'),
    require => [Exec['git clone ohmyzsh']],
    force   => true,
  }

  file {"${user_home}/.oh-my-zsh/custom/plugins/notmuch":
    ensure  => 'link',
    owner   => $user,
    target  => find_file('core/oh-my-zsh/plugins/notmuch'),
    require => [Exec['git clone ohmyzsh']],
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
