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
  # Install all the packages that the workstation needs to run
  #
  include core::packages

  #
  # Install docker and docker-compose
  #
  class { 'docker': version => 'latest' }
  class {'docker::compose': ensure  => present }
}
