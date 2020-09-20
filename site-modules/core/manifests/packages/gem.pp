define core::packages::gem {
  exec { "gem_install_${title}":
    command  => "gem install ${title}",
    path     => '/usr/bin:/usr/local/bin:/usr/sbin:/bin',
    provider => shell,
  }
}
