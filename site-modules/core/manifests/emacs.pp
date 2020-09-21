class core::emacs {
  vcsrepo { "${user_home}/.emacs.d":
    source   => 'https://github.com/syl20bnr/spacemacs',
    provider =>  'git',
    user     => $user,
    revision => 'develop',
  }
}
