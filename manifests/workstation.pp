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
  # Set up core components
  #
  include core::git
  include core::zsh
  include core::bin
  include core::fonts
  include core::emacs
  include core::terminal
  include core::mkcert
  include core::delta
  include core::language_tool

  #
  # Add the private puppet module for all the proprietary bits only if the
  # module is installed. You can see the puppet file
  #
  if defined('private::main') {
    include private::main
  }
}
