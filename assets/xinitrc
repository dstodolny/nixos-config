#!/bin/sh

[ -f ~/.profile ] && . ~/.profile

if [ $# -eq 0 ]; then
	conky | dzen2 -p -dock -ta l -fn "DejaVu Sans Mono" &
	emacs --daemon --eval "(require 'exwm)" -f exwm-enable
	exec emacsclient -c
fi

exec "$@"
