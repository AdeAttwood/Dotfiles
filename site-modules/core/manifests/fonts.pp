#
# Install the custom font
#
# Author  Ade Attwood <code@adeattwood.co.uk>
# Updated 2018-11-11
#

class core::fonts {
  file { "${user_home}/.local/share/fonts":
    ensure => 'directory',
  }

  file { "${user_home}/.local/share/fonts/LigaFreeMono.ttf":
    ensure => 'link',
    target => find_file('core/fonts/LigaFreeMono.ttf'),
  }
}
