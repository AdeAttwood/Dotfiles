# Module to install dandavison/delta diff tool
#
# See: https://github.com/dandavison/delta
class core::delta {
  archive { 'delta':
    path          => '/tmp/delta.tar.gz',
    source        => 'https://github.com/dandavison/delta/releases/download/0.13.0/delta-0.13.0-x86_64-unknown-linux-gnu.tar.gz',
    extract       => true,
    extract_path  => "/tmp",
    checksum_type => 'sha256',
    checksum      => '69ddecc92e86b77016692589c094bf3a32e7cb8a0dfd9f97f253c173cce830e9',
  }

  file { 'delta':
    path   => "${user_home}/.local/bin/delta",
    mode   => '0755',
    source => "/tmp/delta-0.13.0-x86_64-unknown-linux-gnu/delta"
  }
}
