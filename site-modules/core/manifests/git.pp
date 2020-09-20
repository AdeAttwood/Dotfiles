class core::git {
  file { "${user_home}/.gitconfig":
    owner   => $user,
    content => template('core/git/gitconfig.erb')
  }
}
