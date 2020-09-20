if (( $+commands[kubectl] )); then
    __KUBECTL_COMPLETION_FILE="${ZSH_CACHE_DIR}/kubectl_completion"

    if [[ ! -f $__KUBECTL_COMPLETION_FILE ]]; then
        kubectl completion zsh >! $__KUBECTL_COMPLETION_FILE
    fi

    [[ -f $__KUBECTL_COMPLETION_FILE ]] && source $__KUBECTL_COMPLETION_FILE

    unset __KUBECTL_COMPLETION_FILE
fi

# This command is used a LOT both below and in daily life
alias k=kubectl

alias kube-get-url="kubectl cluster-info | grep 'Kubernetes master' | awk '/http/ {print \$NF}'"
alias kube-get-cert="kubectl get secret \`kubectl get secrets | grep default-token | cut -d \" \" -f1\` -o jsonpath=\"{['data']['ca\.crt']}\" | base64 --decode"
alias kube-get-token="kubectl get secret \`kubectl get secrets | grep default-token | cut -d \" \" -f1\` -o jsonpath=\"{['data']['token']}\" | base64 --decode"
