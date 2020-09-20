
#
# Displays markdown files in a man format
#
# Arguments:
#     1. The path to the markdown file
# Examples:
#     mdman ~/docs/file.md
#
function mdman() {
    local file="$1";

    local tmp="/tmp/mdman";
    local tmp_file="$tmp/$(basename $file).man";

    if [[ -f "$tmp_file" ]]; then
        man "$tmp_file";
        return
    fi

    mkdir -p "$tmp";

    if [[ ! -f "$file" ]]; then
        file=`locate "$file.md"`

        if [[ ! -f "$file" ]]; then
            echo "$file is not found";
            return 1;
        fi
    fi

    if [[ ! -f "$tmp_file" ]];then
        pandoc -s -t man "$file" -o "$tmp_file";
    fi

    man "$tmp_file";
}
