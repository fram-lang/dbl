#!/bin/bash

set -euo pipefail

if [[ "$#" -ne "1" ]]; then
	echo "usage: $0 <build_log_file>"
	exit 1
fi
if [[ ! -f "$1" ]]; then
	echo "$0: cannot access '$1': No such file or directory"
	exit 1
fi

if grep -qE -e "reduce conflict" -- "$1"; then
	echo "Conflicts found"
	exit 1
fi
