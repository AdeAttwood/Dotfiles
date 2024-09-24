
export EMCAS_BINARY="/usr/bin/emacs"
export EMACS_CLIENT="/usr/bin/emacsclient"

export EMACS_PLUGIN_LAUNCHER="$EMACS_CLIENT --alternate-editor '$EMACS_BINARY --daemon && $EMACS_CLIENT'"

alias emacs="$EMACS_PLUGIN_LAUNCHER --no-wait"
alias e=emacs
alias te="$EMACS_PLUGIN_LAUNCHER -nw"
