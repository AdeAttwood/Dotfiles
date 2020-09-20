define core::vim::plugin_opt($url, $provider = 'git', $ensure = 'latest', $revision = 'master') {
  vcsrepo { "${user_home}/.config/nvim/pack/bundle/opt/${title}":
    ensure   => $ensure,
    source   => $url,
    provider => $provider,
    revision => $revision,
  }
}
