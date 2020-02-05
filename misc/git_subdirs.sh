###
# Jimmy Hickey
# 2020-02-04
# Check all subdirectories and check for any uncommited git changes
###


shopt -s nullglob
subdirs=(*/)
shopt -u nullglob

declare -a uncommitted_dirs
declare -a unpushed_dirs


for idir in "${subdirs[@]}"; do
	cd ${idir}

	if [[ -d .git ]]; then

		uncommitted="$(git status --porcelain)"

		if [[ "${uncommitted}" ]]; then
			uncommitted_dirs+=( "${idir}" )
		fi


		unpushed="$(git log origin/master..HEAD)"
		
		if [[ "${unpushed}" ]]; then
			unpushed_dirs+=( "${idir}" )
		fi
	fi

	# go back
	cd ..
done

echo "DIRECTORIES WITH UNCOMMITED CHANGES"
printf '%s\n' "${uncommitted_dirs[@]}"

echo "DIRECTORIES WITH UNPUSHED CHANGES"
printf '%s\n' "${unpushed_dirs[@]}"
