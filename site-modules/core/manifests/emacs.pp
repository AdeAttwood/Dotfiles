class core::emacs {
  file { "${user_home}/.emacs.d":
    ensure => 'directory',
  }

  file { "${user_home}/.emacs.d/src":
    ensure => 'link',
    target => find_file('core/emacs/src'),
  }

  file { "${user_home}/.emacs.d/snippets":
    ensure => 'link',
    target => find_file('core/emacs/snippets'),
  }

  file { "${user_home}/.emacs.d/init.el":
    ensure => 'link',
    target => find_file('core/emacs/init.el'),
  }
}
