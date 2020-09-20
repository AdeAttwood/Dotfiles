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
  class { 'docker::compose': ensure  => present }

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
  # Install all the packages that the workstation needs to run
  #
  include core::packages
}
