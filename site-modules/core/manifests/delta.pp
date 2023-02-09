# Module to install dandavison/delta diff tool
#
# See: https://github.com/dandavison/delta
class core::delta {
  archive { 'delta':
    path          => '/tmp/delta.tar.gz',
    source        => 'https://github.com/dandavison/delta/releases/download/0.15.1/delta-0.15.1-x86_64-unknown-linux-gnu.tar.gz',
    extract       => true,
    extract_path  => "/tmp",
    checksum_type => 'sha256',
    checksum      => '9d9b42a7b9ea554b4bf123f245d5f2014478ac87ddc508744a3f34f5c2d512c7',
  }

  file { 'delta':
    path   => "${user_home}/.local/bin/delta",
    mode   => '0755',
    source => "/tmp/delta-0.15.1-x86_64-unknown-linux-gnu/delta"
  }
}
