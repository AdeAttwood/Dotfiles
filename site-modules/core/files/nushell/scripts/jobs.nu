# Nushell uses `job unfreeze` for the traditional bash/zsh `fg` workflow:
# resume the latest frozen job, or a specific frozen job id, in the foreground.
#
# This job-control support, including `job unfreeze`, was introduced in
# Nushell 0.103.0. It is Unix-oriented functionality and is gated behind
# Nushell's `os` feature in builds that customize feature flags.
export alias fg = job unfreeze
