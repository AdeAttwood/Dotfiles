# Module to install the languagetool service locally for creating spelling and
# grammar checking
class core::language_tool {
  file { "${user_home}/.local/share/LanguageTool":
    ensure => 'directory'
  }

  archive { 'LanguageTool-5.6.zip':
    path         => '/tmp/LanguageTool-5.6.zip',
    source       => 'https://languagetool.org/download/LanguageTool-5.6.zip',
    extract      => true,
    extract_path => "${user_home}/.local/share/LanguageTool",
    creates      => "${user_home}/.local/share/LanguageTool/LanguageTool-5.6",
    require      => File["${user_home}/.local/share/LanguageTool"],
  }
}
