class core::emacs {
  file { "${user_home}/.emacs.d":
    ensure => 'link',
    target => find_file('core/emacs'),
  }
}
