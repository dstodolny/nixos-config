#!/bin/sh

if [ "$1" = "-h" ]; then
	cat <<EOF>&2
Usage: ${0##*/} [OPTIONS]

Display weather forecast.

Use '${0##*/} :help' to list all options.

EOF
	exit
fi

agent=curl
agent_flags=""
if ! command -v curl >/dev/null 2>&1; then
	agent=wget
	agent_flags="-q -O -"
fi
$agent $agent_flags wttr.in/"$1"
