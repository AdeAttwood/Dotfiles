#
# Provisions all of my machines ssh keys for remote access
#
# Author  Ade Attwood <code@adeattwood.co.uk>
# Updated 2018-09-03
#

class core::ssh {
  ssh_authorized_key { 'Ade-Home-Workstation':
    ensure => present,
    user   => $user,
    type   => 'ssh-rsa',
    key    => 'AAAAB3NzaC1yc2EAAAADAQABAAACAQDMTg++6fHLdQJxP2Q8q+INqXxAEsvJKZHadupjX8VMi1Pk8BYiYNy7cMdgmhg+KbGfBZLKTK5xbQb3261VMHUISOju0V/dwA2XwrX6yN+kuvK/nDkQsZbaDTQZhAz0RHXyHThpu5I8oAA3dzzFAdslnmJV0J4aYA0woOfxgYU3339o7RSKW8kR7fuWxLwfCO2SK3tlgfgiYHsQ5yHDUojbOGPwmvS7SXI6+U+x7ndZiTsCp/Dn+n+z8M5R2mfyk16Wz/N1+khbvuHllVGkpD0XXSR8BopxglZ6zZFuhR23ASjdmj/lSQvZoOhxgMYaapRuNj4cyp5tt2HEzoJVf9uhLvi3eW96FMaTSl/ALE+UTA3FGXPMB8aT7WbwiaQm6B2Y2SHDToI2HeUy5K6P6keijDxSkC0LCpifDAUyM61zcoi042kAWaJpu+7zB+A4sz1XeTjVA3VzMl98P9z3h612S97hbMACXC9ZHxA/Qgcm6P5ZJty9XUPJkybeOZ7K0QUdr0RUfnxX91ZtSDPwMPIK0KNcSuo5NbdR/U5Ke87zgaR15tZ2rXcL2m61uU1TSXo2VUd9XP9amLbgjEYT+bMYDTw31+THOn9v28YCdQSux3TNM3HnkrV541N9A3T6bz4LFyBB7yqSUzKSiNbP55a+rIOWN4BWejdc7yvJegxEdw==',
  }

  ssh_authorized_key { 'Ade-Home-Laptop':
    ensure => present,
    user   => $user,
    type   => 'ssh-rsa',
    key    => 'AAAAB3NzaC1yc2EAAAADAQABAAACAQDvQkUPiWwPI/FqOCdVkGRCWkbGQjhmjIv7FI5YJ8RuTaVVwoR3BDT/AOiKEanpSMk/BWsbbgLSgOF72JIH0Nrykm9kTOS4rmPOGt5RNOtYZ2kLm4hfzzvurUvIR0J8UDXYRoxQ4J+hakeLhcmNLBXahCF0L70a9eqPPQw7AWRrBDouGzymZVZJOtC1kcybzlU7KTU/zXd9/pdgH7967aShwc5OeJraXFZdqk6TEqH1CTkgF7K7KfeWU5uyGCGVA3Gf6De1CEnZ0SL9ZTkXOX2o9GcZ7ctWBTjjRLtvnJYtb0z5PgMxCqH7OAqgiwbzTx8tMGn+4HYe8mTSZC1XOxNQHZ1EiIX7pypfN0dBCEHDFoqnaDwtcMUHNE7pk1bcdz9eeh/RUAT9iDJB1EDIOuQl+vGPWNbzyGfL+klnyxIwnkd9PGY3YUurlVXAaFGxaKe1IIlNi5DwVE4zFhj9HSydrpKVz70pdpNTN7MglWYGRA8IawgWV72xfkKesR/vq3FzPKbM51aIhAjZOowurJ2LEMpOKbgDHvikwoXbjfIjRbVPa5n3Pw56l9Ixhu4KBmHY9iyjBRtumE1XDO8RIJQtkfm0kSnX5yld5wn2aZtvxHHFgskRtl6dFr1UqL0y/n56F/FfV4j/ZRQgTkBKx35sJANyXvpHdKKw4X2P8tAAEQ==',
  }

  ssh_authorized_key { 'Ade-Work-Workstation':
    ensure => present,
    user   => $user,
    type   => 'ssh-rsa',
    key    => 'AAAAB3NzaC1yc2EAAAABJQAAAQEA0SZ866Ih270Qs3H0SQVJpXkq6mzeklybEbDj2HI2TwSZuExxwqNsaHw+NDBW/dlrtEQz0SYUDGhdFEllgl8Cyv8wSqXh+yFYD1ZhdUvwVEr4t9SXbTMjnjJojMEZbzVXAimWVw8Y6Geuk7y9wpTVMi49dfjL44ZUQ/qwlkswIiMzF1raluz38G3r+O8DVVk08mw148g3y7+d+F2oPO252X8eKZ+GSfwbZ/u7hb0WdVFxBLRPUjHw4LZPNMrWFYblwu7FS/mNz6y68muUVz/qeCEn6ty2WnS7flHp5l4CIJ5Z6HcyAH3Gi7E3HrLwV1dCPYMueCaW8DQlLSX/4HnGXw==',
  }
}
