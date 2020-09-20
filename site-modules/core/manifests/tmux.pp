#
# Config for tmux terminal multiplexer
#
# Author  Ade Attwood <code@adeattwood.co.uk>
# Updated 2018-07-16
#

class core::tmux {
  exec { 'make tmux dir':
    creates => "${user_home}/.tmux/plugins",
    path    => '/usr/bin:/usr/sbin:/bin',
    command => "mkdir -p ${user_home}/.tmux/plugin",
  }

  file { "${user_home}/.tmux.conf":
    ensure => 'link',
    owner  => $user,
    target => find_file('core/tmux.conf'),
  }

  vcsrepo { "${user_home}/.tmux/plugins/tmux-yank":
    ensure   => latest,
    source   => 'git://github.com/tmux-plugins/tmux-yank.git',
    provider => git,
    revision => 'master',
  }

  vcsrepo { "${user_home}/.tmux/plugins/tmux-open":
    ensure   => latest,
    source   => 'git://github.com/tmux-plugins/tmux-open.git',
    provider => git,
    revision => 'master',
  }

  vcsrepo { "${user_home}/.tmux/plugins/tmux-copycat":
    ensure   => latest,
    source   => 'git://github.com/tmux-plugins/tmux-copycat.git',
    provider => git,
    revision => 'master',
  }
}
