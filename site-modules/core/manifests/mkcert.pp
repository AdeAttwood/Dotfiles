class core::mkcert {
  archive { "${user_home}/.local/bin/mkcert":
    ensure        => present,
    source        => 'https://github.com/FiloSottile/mkcert/releases/download/v1.4.2/mkcert-v1.4.2-linux-amd64',
    checksum      => 'e116543bfabb4d88010dda8a551a5d01abbdf9b4f2c949c044b862365038f632',
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
