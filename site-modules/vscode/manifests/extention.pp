define vscode::extention {
  exec { "code_install_${title}":
    command  => "code --install-extension ${title}",
    path     => '/usr/bin:/usr/local/bin:/usr/sbin:/bin',
    unless   => "code --list-extensions | grep ${title}",
    provider => shell,
  }
}
