class core::mkcert {
  archive { "${user_home}/.local/bin/mkcert":
    ensure        => present,
    source        => 'https://github.com/FiloSottile/mkcert/releases/download/v1.4.4/mkcert-v1.4.4-linux-amd64',
    checksum      => '6d31c65b03972c6dc4a14ab429f2928300518b26503f58723e532d1b0a3bbb52',
    checksum_type => 'sha256',
  }

  exec { 'mkcert permission':
    command   => "chmod +x ${user_home}/.local/bin/mkcert",
    path      => '/bin:/usr/bin',
    subscribe => Archive["${user_home}/.local/bin/mkcert"],
  }

  exec { 'install root ca':
    command     => "${user_home}/.local/bin/mkcert -install",
    environment => [ "HOME=${user_home}" ], 
    path        => '/bin:/usr/bin',
    subscribe   => Archive["${user_home}/.local/bin/mkcert"],
  }
}
