#!/bin/sh
## This file should be automatically sourced by the login manager or Bash if
## .bash_profile does not exist.  If this file is not automatically sourced,
## do it from the shell config to me sure it applies to TTY as well.

## Mask
## Result for 027 is "rwxr-x---".  022 is the popular default.
##
## As a result applications make the bad assumption # that "others" have access.
## Another drawback of 027 is that is behaves badly with default sudo config: for
## instance "sudo mkdir foo" will effectively create a "foo" folder whose owner
## is root and with permission 027, even if root's umask is 022.  This is
## usually very bad.
## See https://wiki.archlinux.org/index.php/Sudo#Permissive_umask.
##
## It is possible to override sudo's umask by adding the following to the
## sudoers file:
##
## Defaults umask = 0022
## Defaults umask_override
# umask 027


## Preliminary path definitions.  For security reasons (and bad programming
## assumptions) you should always append entries to PATH, not prepend them.
appendpath () {
	[ $# -eq 2 ] && PATHVAR=$2 || PATHVAR=PATH
	[ -d "$1" ] || return
	eval echo \$$PATHVAR | grep -q "\(:\|^\)$1\(:\|$\)" && return
	eval export $PATHVAR="\$$PATHVAR:$1"
}
prependpath () {
	[ $# -eq 2 ] && PATHVAR=$2 || PATHVAR=PATH
	[ -d "$1" ] || return
	eval echo \$$PATHVAR | grep -q "\(:\|^\)$1\(:\|$\)" && return
	eval export $PATHVAR="$1:\$$PATHVAR"
}

## Use this to override system executables.
prependpath "${HOME}/personal/hackpool"

## Last PATH entries.
appendpath "${HOME}/.local/bin"

## mcron: needs to be run after PATH is fully set or else local programs could
## be missing.
#if command -v mcron >/dev/null 2>&1; then
	# TODO: Only start if not already started?
	# pkill mcron
#	mcron &
#fi

## Remove less history.
LESSHISTFILE='-'

## Manpage.
export MANPAGER="less -s"
export MANWIDTH=80

## Time display (with ls command for example).  GNU 'ls' only.
export TIME_STYLE=+"|%Y-%m-%d %H:%M:%S|"

## Linux specific
if [ "$(uname -o)" = "GNU/Linux" ] ; then
	## Startup error log.
	## dmesg
	log_dmesg="$(dmesg | grep -i error)"
	[ -n "$log_dmesg" ] && echo "$log_dmesg" > "$HOME/errors-dmesg.log" || rm "$HOME/errors-dmesg.log" 2>/dev/null
fi

## Default text editor
## 'em' is a custom wrapper for emacsclient. See '.bin/em'.
## VISUAL is given priority by some programs like Mutt. This way we can separate
## editors that wait from those that don't.
for i in emacsclient em emacs vim vi nano; do
	command -v $i >/dev/null 2>&1 && export EDITOR=$i && break
done
GIT_EDITOR="$EDITOR"
VISUAL="$EDITOR"
[ "$GIT_EDITOR" = em ] && GIT_EDITOR=emc
[ "$VISUAL" = em ] && VISUAL=emw
export GIT_EDITOR
export VISUAL

## Hook. Should be sourced last
[ -f ~/.profile_hook ] && . ~/.profile_hook
## Hook example
#
# export CPPFLAGS=-I$HOME/local/usr/include
# export LDFLAGS=-L$HOME/local/usr/lib
#
# appendpath "$HOME/local/usr/lib/python2.7/dist-packages/" PYTHONPATH
# export LUA_CPATH="$HOME/local/usr/lib/lib?.so;$(lua -e "print(package.cpath)")"
#
# umask 077

## End: Source .bashrc. The rc file should guard against non-interactive shells.
[ "$(ps -o comm= $$)" != bash ] && return
[ -f ~/.bashrc ] && . ~/.bashrc
