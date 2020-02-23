###
# Jimmy Hickey
# 2020-02-04
# Check all subdirectories for any uncommited and unpushed git changes
###


shopt -s nullglob
subdirs=(*/)
shopt -u nullglob

declare -a uncommitted_dirs=()
declare -a unpushed_dirs=()

# because my array lengths weren't changing for some reason
ANY_UNCOMMITTED=0
ANY_UNPUSHED=0

for idir in "${subdirs[@]}"; do
	cd ${idir}

	if [[ -d .git ]]; then

		uncommitted="$(git status --porcelain)"

		if [[ "${uncommitted}" ]]; then
			uncommitted_dirs+=("${idir}")
			ANY_UNCOMMITTED=1
		fi


		unpushed="$(git log origin/master..HEAD)"
		
		if [[ "${unpushed}" ]]; then
			unpushed_dirs+=("${idir}")
			ANY_UNPUSHED=1
		fi
	fi

	# go back
	cd ..
done


if [ "${ANY_UNCOMMITTED}" -eq 1 ]; then
	echo "DIRECTORIES WITH UNCOMMITED CHANGES"
	printf '%s\n' "${uncommitted_dirs[@]}"
fi


if [ "${ANY_UNPUSHED}" -eq 1 ]; then
	echo "DIRECTORIES WITH UNPUSHED CHANGES"
	printf '%s\n' "${unpushed_dirs[@]}"
fi
