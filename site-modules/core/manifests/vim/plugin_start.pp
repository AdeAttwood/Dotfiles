define core::vim::plugin_start($url, $provider = 'git', $ensure = 'latest', $revision = 'master') {
  vcsrepo { "${user_home}/.config/nvim/pack/bundle/start/${title}":
    ensure   => $ensure,
    source   => $url,
    provider => $provider,
    revision => $revision,
  }
}
