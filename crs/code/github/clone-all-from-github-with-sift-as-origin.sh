#!/usr/bin/env bash
set -euxo pipefail

fails=()
for repo in $(gh repo list aixcc-sc | cut -f1 | cut -c 10-); do
	test ! -d "$repo"
	GIT_SSH_COMMAND="ssh -i $HOME/.ssh/id_lacrosse -o IdentitiesOnly=yes" gh repo clone "aixcc-sc/$repo"
	cd "$repo"
	git remote set-url origin ""
	if ! git push --all; then
		fails+=("$repo")
	fi
	cd ..
done

set +x
if [[ "${#fails[@]}" != 0 ]]; then
	echo "Failed to push ${#fails[@]} repos:"
	for repo in "${fails[@]}"; do
		echo "- $repo"
	done
fi
