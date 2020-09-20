class core::emacs {
  vcsrepo { "${user_home}/.emacs.d":
    source   => 'https://github.com/syl20bnr/spacemacs.git',
    provider =>  'git',
    user     => $user,
    revision => 'develop',
  }

  file { "${user_home}/.spacemacs":
    ensure => 'link',
    target => find_file('core/spacemacs'),
  }
}
