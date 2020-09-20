#
# Links all of the bin files into `~/.local/bin`
#
# Author  Ade Attwood <code@adeattwood.co.uk>
# Updated 2018-07-16
#

class core::bin {
  file { "${user_home}/.local/bin":
    ensure => 'directory',
    owner  => $user,
  }

  exec { 'Link bin files':
    path    => '/usr/bin:/usr/sbin:/bin',
    command => "mkdir -p ${user_home}/.local/bin \
      && ln -sf ${find_file('core/bin')}/* ${user_home}/.local/bin/",
  }
}
