class core::vscode {

  file { [
    "${user_home}/.config/Code",
    "${user_home}/.config/Code/User",
  ]:
      ensure => 'directory',
  }

  vscode::extention { [
    'adamwalzer.string-converter',
    'andrsdc.base16-themes',
    'bmewburn.vscode-intelephense-client',
    'christian-kohler.path-intellisense',
    'donjayamanne.githistory',
    'eamodio.gitlens',
    'ikappas.phpcs',
    'jpogran.puppet-vscode',
    'neilbrayfield.php-docblocker',
    'ms-azuretools.vscode-docker',
    'stkb.rewrap',
    'streetsidesoftware.code-spell-checker',
    'uriberman.colonizer',
    'vscodevim.vim',
  ]: }

  file { "${user_home}/.config/Code/User/settings.json":
    ensure => 'link',
    force  => true,
    target => find_file('core/vscode/settings.json'),
  }

  file { "${user_home}/.config/Code/User/keybindings.json":
    ensure => 'link',
    force  => true,
    target => find_file('core/vscode/keybindings.json'),
  }

  file { "${user_home}/.config/Code/User/snippets":
    ensure => 'link',
    force  => true,
    target => find_file('core/vscode/snippets'),
  }

}
