define core::packages::npm {
  exec { "npm_install_${title}":
    command => "${find_file('/core/scripts/npm-install-package')} ${title}",
    #command  => ". $HOME/.nvim/nvim.sh && npm install -g ${title}",
    # path     => '/usr/bin:/usr/local/bin:/usr/sbin:/bin',
    provider => shell,
    user => $user,
  }
}
