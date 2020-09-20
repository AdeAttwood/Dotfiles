define vscode::extention {
  exec { "code_install_${title}":
    command  => "code --install-extension ${title}",
    path     => '/usr/bin:/usr/local/bin:/usr/sbin:/bin',
    provider => shell,
  }
}
