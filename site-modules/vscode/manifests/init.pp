class vscode {

  archive { '/tmp/microsoft.asc':
    ensure => present,
    source => 'https://packages.microsoft.com/keys/microsoft.asc',
  }

  exec { 'gpg --dearmor /tmp/microsoft.asc':
    path    => ['/usr/bin', '/usr/sbin',],
    creates => '/tmp/microsoft.asc.gpg',
    require => [
      Archive['/tmp/microsoft.asc'],
    ]
  }

  file { '/etc/apt/trusted.gpg.d/microsoft.gpg':
    ensure  => present,
    source  => 'file:///tmp/microsoft.asc.gpg',
    require => [
      Exec['gpg --dearmor /tmp/microsoft.asc'],
    ]
  }

  apt::source { 'vscode':
    comment      => 'This is the official VSCode repository',
    location     => 'https://packages.microsoft.com/repos/vscode',
    architecture => 'amd64',
    release      => 'stable',
    repos        => 'main',
    include      => {
      'deb' => true,
    },
  }

  package { 'code':
    ensure  => installed,
    require => [
      Apt::Source['vscode'],
    ],
  }
}
