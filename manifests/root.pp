# Disable filebucket by default for all File resources:
# https://github.com/puppetlabs/docs-archive/blob/master/pe/2015.3/release_notes.markdown#filebucket-resource-no-longer-created-by-default
File { backup => false }

node default {
  #
  # Find the username
  #
  # This can be set in the hira data so you can still do things as your user
  # when running puppet as root
  #
  $user = lookup('user', String, 'first', $id)
  notify { "Running as ${id} for ${user} with home of ${user_home}": }

  #
  # Install docker and docker-compose
  #
  class { 'docker': version => 'latest' }
  class { 'docker::compose': ensure  => present, version => '1.29.2' }

  #
  # Install vscode
  #
  class { 'vscode': }

  #
  # Install emacs
  #
  apt::ppa { 'ppa:kelleyk/emacs': }
  package { 'emacs27':
    ensure  => installed,
    require => [
      Apt::Ppa['ppa:kelleyk/emacs'],
    ],
  }

  #
  # Neovim PPA to get a better version
  #
  apt::ppa { 'ppa:neovim-ppa/stable': }

  #
  # Install alacritty from deb archive is not available
  #
  archive { '/tmp/alacritty.deb':
    ensure        => present,
    source        => 'https://github.com/alacritty/alacritty/releases/download/v0.4.3/Alacritty-v0.4.3-ubuntu_18_04_amd64.deb',
    checksum      => 'f4e40511e1e1495d15e518f5bfccfff2ed69b171ff53964622c1e66bbe954000',
    checksum_type => 'sha256',
  }

  package { 'alacritty':
    ensure   => present,
    provider => dpkg,
    source   => '/tmp/alacritty.deb',
    require  => [
      Archive['/tmp/alacritty.deb']
    ],
  }

  #
  # Install all the packages that the workstation needs to run
  #
  include core::packages
}
