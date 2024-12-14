#!/usr/bin/env bash
if ! [ -n "$1" ] || ! [ -n "$2" ]; then
	echo "USAGE: ./test.sh PROGRAM TEST_SUITE"
	exit 1
fi

# if ! dune build; then
# 	exit 1
# fi

export DBL_LIB="lib/"

TIMEOUT=1.0

binary="_build/default/src/$1.exe"
flags=""

RED='\033[0;31m'
NC='\033[0m'

total_tests=0
passed_tests=0

function start_test {
	total_tests=$(($total_tests + 1))
}

function pass_test {
	passed_tests=$(($passed_tests + 1))
}

function print_status {
	echo "stdout: >>>>>"
	cat $1
	echo "<<<<<"
	echo "stderr: >>>>>"
	cat $2
	echo "<<<<<"
	echo "exit_code: $3"
}

function check_output {
	IFS=$'\n'
	for exp in $(cat $2 | grep "^// @$1:" | sed "s/^\/\/ @$1://"); do
		if ! grep $exp $3 > /dev/null; then
			echo -e "${RED}$1 does not contain phrase: $exp${NC}"
			unset IFS
			return 1
		fi
	done
	unset IFS
	return 0
}

function run_test {
	start_test
	local tmp_stdout=$(mktemp)
	local tmp_stderr=$(mktemp)
	echo "$1"
	timeout $TIMEOUT $1 >$tmp_stdout 2>$tmp_stderr
	local status=$?
	if $2 $status $tmp_stdout $tmp_stderr; then
		if check_output "stderr" $4 $tmp_stderr && check_output "stdout" $4 $tmp_stdout; then
			pass_test
		else
			print_status $tmp_stdout $tmp_stderr $status
		fi
	else
		echo -e "${RED}$3${NC}"
		print_status $tmp_stdout $tmp_stderr $status
	fi
	rm $tmp_stdout
	rm $tmp_stderr
}

function check_simple_test {
	[ $1 -eq 0 ]
}

function simple_test {
	local cmd="$binary $flags $1"
	local err="Command ${cmd} failed."
	run_test "$cmd" check_simple_test "$err" $1
}

function check_exit_code_test {
	[ $2 -eq $1 ]
}

function exit_code_test {
	local cmd="$binary $flags $2"
	local err="Command ${cmd} does not exited with code $1."
	run_test "$cmd" "check_exit_code_test $1" "$err" $2
}

function run_with_flags {
	flags=$2
	$1
}

source "$2"

echo "Passed: ${passed_tests}/${total_tests}"

if [ $passed_tests -eq $total_tests ]; then
	exit 0
else
	exit 1
fi
