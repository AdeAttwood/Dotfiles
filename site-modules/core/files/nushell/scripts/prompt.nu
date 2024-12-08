# All the functions use to create the shell prompt. The prompt is based on the
# oh-my-zsh theme "pygmalion". Its way simpler than the original theme but it
# has all the features I have used over the years.

# Creates the left prompt
#
# The format of the prompt is: username@hostname:current_path
def create_left_prompt [] {
    let username = $env.USER
    let hostname = $env.HOSTNAME

    let formatted_path = $env.PWD
      | str replace $env.HOME '~'
      | str replace '~/Code/src/' '~s/'
      | str replace '~s/github.com' '~gh'

    $"(ansi magenta)($username)(ansi light_cyan)@(ansi yellow)($hostname)(ansi red):(ansi light_cyan)($formatted_path)(ansi reset)"
}

export-env {
  $env.PROMPT_COMMAND = { create_left_prompt }
  $env.PROMPT_INDICATOR = "  "
  $env.PROMPT_COMMAND_RIGHT = ""
}
