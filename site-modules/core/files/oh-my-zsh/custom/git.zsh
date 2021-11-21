source $ZSH/lib/git.zsh

alias gcon="git diff --name-only --diff-filter=U"
alias gml="git merge --log"
alias gl="git --no-pager log --format=\"%C(red)%h%Creset %s %Cgreen(%cr)%Creset %C(bold blue)<%an>%Creset\" --reverse --max-count=40"
alias gap="git add -p"
alias gs="gss"

function git_get_fetch_url() {
    git remote show origin -n | awk '/Fetch URL:/{print $3}'
}

function gi() {
    curl -sL https://www.gitignore.io/api/$@;
}

function ggpushpr() {
    if [[ -z "$1" ]]; then
		ggpush -o merge_request.merge_when_pipeline_succeeds -o merge_request.create -o merge_request.remove_source_branch
	else
		ggpush -o merge_request.merge_when_pipeline_succeeds -o merge_request.create -o merge_request.remove_source_branch -o merge_request.target="$1"
    fi
}

function grc() {
    cat "$(git rev-parse --show-toplevel)/.git/COMMIT_EDITMSG" \
		| grep -B 99999999 '# ------------------------ >8 ------------------------' \
		> "$(git rev-parse --show-toplevel)/.git/COMMIT_TEMPLATE"

	git commit -t "$(git rev-parse --show-toplevel)/.git/COMMIT_TEMPLATE"
}

function my-clone() {
    local url="$1"
    local re="^(https|git)(:\/\/|@)([^\/:]+)[\/:](.*?).git$"

	if [[ -z "$url" ]]; then
		echo "ERROR: Invalid url"
		return
	fi

    if [[ $url =~ $re ]]; then
        local protocol="${match[1]}"
        local separator="${match[2]}"
        local hostname="${match[3]}"
        local repo="${match[4]}"

        full_path=~s/$hostname/$repo

        git clone $url ~s/$hostname/$repo
        cd ~s/$hostname/$repo
    fi
}
