# ~/.profile: login shells -*- shell-script -*-

# BACKGROUND
#
# Some things only need to be done once when a user logs into the machine. Do
# that here. Anything else that needs to be done for every individual
# interactive shell should be placed in ~/.shrc.

# echo >&2 "$SHELL (login): running $HOME/.profile"

# The default umask is set in /etc/profile; for setting the umask for ssh
# logins, install and configure the libpam-umask package.
umask 022

# Determine OS and add OS specific PATH if present
os=$(uname -s | tr '[:upper:]' '[:lower:]')

# Linux has a few variants that change the packages one must install, and the
# command used to install those packages.
if [ "$os" = "linux" ] ; then
    # OS-specific compiled binaries
    if [ -r /etc/os-release ] ; then
		os=el$(awk -F\" '/VERSION_ID/ {print $2}' /etc/os-release | cut -d. -f1)
    elif [ -f /etc/debian-release ] ; then
		os=debian
    fi
fi

# To prioritize access latency over availability, ensure that highly ephemeral
# cache data is stored on local machine rather than a home directory that is
# potentially mounted over a network. However, do place all cache files in a
# directory that makes it trivial to identify the owner and optionally remove
# all cache data.
if [ -z "$TMPDIR" ] ; then
    export TMPDIR="/var/tmp/$LOGNAME"
elif [ "$TMPDIR" = "${TMPDIR%${LOGNAME}}" ] ; then
	# NOTE: The below removes any trailing "/" character at the end of TMPDIR
	# before appending the account name.
    export TMPDIR="${TMPDIR%/}/$LOGNAME"
fi
mkdir -p "$TMPDIR"
echo "Feel free to erase this directory and all of its contents." > "$TMPDIR/README.txt"

# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
# https://xdgbasedirectoryspecification.com
[ ! -f "$HOME/.config/user-dirs.dirs" ] || . "$HOME/.config/user-dirs.dirs"
[ -n "$XDG_CACHE_HOME" ] || export XDG_CACHE_HOME="$TMPDIR" # Cache: /var/cache
[ -n "$XDG_CONFIG_HOME" ] || export XDG_CONFIG_HOME="$HOME/.config" # Configuration: /etc
[ -n "$XDG_DATA_HOME" ] || export XDG_DATA_HOME="$HOME/.local/share" # Application Data: /usr/share
[ -n "$XDG_STATE_HOME" ] || export XDG_STATE_HOME="$HOME/.local/state" # Application State: /var/lib

# Prefix several file-system locations to the PATH.
#
# When user has private program directory, configure it to be the first in
# their PATH environment variable, to allow the user to specify which exact
# path to each program they might need to override.

# echo INITIAL PATH
# echo $PATH | tr : '\n' >&2

for i in \
	/opt/local/sbin \
	/opt/local/bin \
	/opt/local/libexec/gnubin \
	$HOME/.cargo/bin \
	$XDG_DATA_HOME/bin \
	$XDG_DATA_HOME/${os}/bin \
    ; do
    # [ -d "$i" ] && export PATH="${i}:${PATH}" && echo "--> [Y] $i" || echo "--> [N] $i"
    [ -d "$i" ] && export PATH="${i}:${PATH}"
done
unset i

# No need to search directories multiple times: Omit duplicate entries from
# PATH, but keep items in the same relative order.
export PATH="$(echo "$PATH" | awk 'BEGIN { FS=":"; ORS="" } { for (i=1; i<=NF; i++) if (foo[$i] != 1) { if (i>1) print ":"; print $i; foo[$i]=1 } }')"

unset MANPATH
export MANPATH=$(echo "$PATH" | tr : '\n' | (while read -r p ; do
												 for mp in "${p%/*}/man" "${p%/*}/share/man"; do
													 if [ -d "$mp" ] && [ "$MANPATH" = "${MANPATH%${mp}*}" ] ; then
														 MANPATH="${MANPATH:+$MANPATH:}${mp}"
													 fi
												 done
											 done ; echo "$MANPATH"))

# echo >&2 "MANPATH: $MANPATH"

export EDITOR=emacsclient
export VISUAL=emacsclient

# GOBIN: The directory where 'go install' will install a command. (To allow
# sharing among different machines, places in HOME.)
[ -n "$GOBIN" ] || export GOBIN="$XDG_DATA_HOME/${os}/bin"

# GOCACHE: The directory where the go command will store cached information
# for reuse in future builds. (Highly ephemeral data. Access latency
# prioritized over availability.)
[ -n "$GOCACHE" ] || export GOCACHE="$TMPDIR/go-build"

# GOMODCACHE: The directory where the go command will store downloaded
# modules. (Once downloaded, should never need to download again. Defaults to
# $GOPATH/pkg/mod.)
[ -n "$GOMODCACHE" ] || export GOMODCACHE="$XDG_DATA_HOME/go/pkg/mod"

# GOPATH: Lists places to look for Go code.  (NOTE: Not sure whether this is
# still needed.)
#
# [ -n "$GOPATH" ] || export GOPATH="$XDG_DATA_HOME/go"

# GOTMPDIR: The directory where the go command will write temporary source
# files, packages, and binaries. (Highly ephemeral data. Access latency
# prioritized over availability.)
[ -n "$GOTMPDIR" ] || export GOTMPDIR="$TMPDIR/go-tmp"
mkdir -p "$GOTMPDIR" # As of Go v1.20, it does not create this directory.

mkdir -p "$XDG_STATE_HOME/history"
export HISTCONTROL=ignoredups
export HISTFILE="$XDG_STATE_HOME/history/$(date +%F-%T)-$(hostname)"
export HISTSIZE=2000

unset os

if test -n "$BASH_VERSION" ; then
    _SHELL="bash"
elif test -n "$KSH_VERSION" ; then
    _SHELL="ksh"
elif test -n "$ZSH_VERSION" ; then
    _SHELL="zsh"
elif test -z "$PS3" ; then
    _SHELL="sh"
else
    _SHELL="unknown"
fi

case $_SHELL in
    bash)
		# /bin/bash only invokes $BASH_ENV when non-interactive, while /bin/sh
		# and /bin/ksh only invoke $ENV when interactive. The goal is to
		# enable re-use of ~/.profile and ~/.shrc for both /bin/sh, /bin/bash,
		# /bin/ksh, and potentially other command line shells.
		[ ! -r "$HOME/.shrc" ] || . "$HOME/.shrc"
		;;
    ksh|sh)
		# /bin/ksh and /bin/sh merely need $ENV to be defined and point to the
		# desired file to run for interactive shells.
		[ ! -r "$HOME/.shrc" ] || export ENV="$HOME/.shrc"
		;;
    zsh)
		# Zsh will invoke ~/.zshenv for all shells, then invoke ~/.zprofile
		# for all login shells, then ~/.zshrc for all interactive shells. It
		# is sufficient to create symbolic links to obtain similar behavior as
		# other shells which are special cased above.
		# 
		#     ln -s .profile .zprofile
		#     ln -s .shrc    .zshrc
		:
		;;
    *) echo >&2 "unrecognized shell: '${_SHELL}'" ;;
esac

# No operations follow the above in this file, to ensure behavior on /bin/sh
# is functionally the same as /bin/bash.
