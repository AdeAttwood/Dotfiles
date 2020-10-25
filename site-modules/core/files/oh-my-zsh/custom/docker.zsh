#
# Docker
#
alias dk="docker"
alias dkl="docker logs -f"
alias dkps="docker ps --format '{{.ID}} ~ {{.Names}} ~ {{.Status}} ~ {{.Image}}'  | column -t -s'~'"
alias dkls="docker container ps -a --format '{{.ID}} ~ {{.Names}} ~ {{.Status}} ~ {{.Image}}'  | column -t -s'~'"
alias dkrm="docker rm"

function dke() {
    #
    # Get the container name you want to execute in
    #
    local container="$1"
    #
    # Shift the params so we can pass the rest to the container
    #
    shift
    #
    # Run the command in the container
    #
    docker exec -it $container /bin/bash -c "$@"
}

function dklogin() {
    docker exec -it $1 /bin/sh -c "[ -e /bin/bash ] && /bin/bash || /bin/sh"
}

function dktop() {
  docker stats --format "table {{.Container}}\t{{.Name}}\t{{.CPUPerc}}  {{.MemPerc}}\t{{.NetIO}}\t{{.BlockIO}}"
}

#
# Docker Compose
#
alias dkc="docker-compose"
alias dkcdown="docker-compose down"
alias dkcup="docker-compose up -d"

function dkcrestart() {
    docker-compose stop $1
    docker-compose up -d $1
}
