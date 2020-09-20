function compress() {
    local compression_type="gz"

	if (( $# == 0 )); then
		cat <<-'EOF' >&2
			Usage: compress [-option] [file ...]

			Options:
			    -t, --type    The compression tool you would like use to compress the target
		EOF
    fi

	if [[ "$1" == "-t" ]] || [[ "$1" == "--type" ]]; then
		compression_type="$1"
		shift
	fi

	while (( $# > 0 )); do
		if [[ ! -f "$1" ]] && [[ ! -d "$1" ]]; then
			echo "compress: '$1' is not a valid file or directory" >&2
			shift
			continue
		fi

        success=0

        if [[ "$compression_type" == 'gz' ]] && [[ -d "$1" ]]; then
            tar -czvf "$1.tar.gz" $1
        fi

        if [[ "$compression_type" == 'gz' ]] && [[ -f "$1" ]]; then
            gzip $1
        fi

		(( success = $success > 0 ? $success : $? ))
		shift
    done
}
