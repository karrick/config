# ~/.profile: login shells -*- shell-script -*-

# BACKGROUND
#
# Some things only need to be done once when a user logs into the machine. Do
# that here. Anything else that needs to be done for every individual
# interactive shell should be placed in ~/.shrc.
#
# Initialization done in this file should be considered for inclusion in
# Emacs' initialization file.

# echo >&2 "$SHELL (login): running $HOME/.profile"

# The default umask is set in /etc/profile; for setting the umask for ssh
# logins, install and configure the libpam-umask package.
umask 022

# # https://stackoverflow.com/questions/48574369/setting-term-variable-for-emacs-shell
# case ${INSIDE_EMACS#*,} in
#	comint) echo >&2 "INSIDE_EMACS: comint" ;;
#	tramp) echo >&2 "INSIDE_EMACS: tramp" ;;
#	"") : ;; # echo >&2 "Not INSIDE_EMACS" ;;
#	*) echo >&2 "INSIDE_EMACS: [$INSIDE_EMACS]" ;;
# esac

# Determine OS and add OS specific PATH if present
os=$(uname -s | tr A-Z a-z)
# echo >&2 "os: [$os]"

# Determine architecture
arch=$(uname -m)
# echo >&2 "arch: [$arch]"

# Linux has a few variants that change the packages one must install, and the
# command used to install those packages.
if [ "$os" = "linux" ] ; then
	# OS-specific compiled binaries
	if [ -r /etc/os-release ] ; then
		distro=$(awk -F= < /etc/os-release '/ID|VERSION_ID/ {s=$2 ; if ("\"" == substr(s, 1, 1)) s = substr(s, 2) ; if ("\"" == substr(s, length(s))) s = substr(s, 1, length(s)-1) ; arr[$1] = s} END {printf("%s%s\n", arr["ID"], arr["VERSION_ID"])}' | cut -d. -f1)
	fi
fi

# echo >&2 "arch: [$arch]"
# echo >&2 "os: [$os]"
# echo >&2 "distro: [$distro]"
# if : ; then return ; fi

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
echo "Feel free to erase this directory and all of its contents." > "$TMPDIR/DELETE-THIS-DIRECTORY-IF-TOO-LARGE.txt"

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

# echo >&2 INITIAL PATH
# echo $PATH | tr : '\n' >&2
PATH0=$PATH
PATH=

for i in \
		$HOME/bin \
		$XDG_DATA_HOME/${os}/${distro}/${arch}/bin \
		$XDG_DATA_HOME/${os}/${distro}/bin \
		$XDG_DATA_HOME/${os}/${arch}/bin \
		$XDG_DATA_HOME/${os}/bin \
		$XDG_DATA_HOME/bin \
		$HOME/.cargo/bin \
		/opt/local/sbin \
		/opt/local/libexec/gnubin \
		/opt/local/bin \
		/usr/sbin \
		/usr/bin \
		/sbin \
		/bin \
	; do
	if ! : ; then
		[ -d "$i" ] && PATH="${PATH:+$PATH:}${i}"
	else
		[ -d "$i" ] && PATH="${PATH:+$PATH:}${i}" && echo "--> [Y] $i" || echo "--> [N] $i"
	fi
done

export PATH
# echo >&2 "PATH:"
# echo $PATH | tr : '\n' >&2

# Track components from original PATH that are not in custom PATH, removing
# duplicates, but leaving elements in same relative order.
PATH_ADDITIONS=$(echo "$PATH0" | awk -v drop=$PATH '
BEGIN {
	  FS=":";
	  c = split(drop, drop_list);
	  for (i=1; i<=c; i++) items[drop_list[i]] = 1;
	  ORS="";
}

{
	for (i=1; i<=NF; i++)
		if ($i != "" && items[$i] != 1) {
		   if (pc == 0) pc=1; else print ":";
		   print $i;
		   items[$i]=1;
		}
}')
# echo >&2 "PATH_ADDITIONS:"
# echo $PATH_ADDITIONS | tr : '\n' >&2
export PATH_ADDITIONS

unset MANPATH
export MANPATH=$(echo "$PATH" | tr : '\n' | (while read -r i ; do
												 for mp in "${i%/*}/man" "${i%/*}/share/man"; do
													 if [ "$MANPATH" = "${MANPATH%${mp}*}" ] && [ -d "$mp" ] ; then
														 MANPATH="${MANPATH:+$MANPATH:}${mp}"
													 fi
												 done
											 done ; echo "$MANPATH"))
# echo >&2 "MANPATH: [$MANPATH]"

MANPATH_ADDITIONS=$(echo "$PATH_ADDITIONS" | tr : '\n' | (while read -r i ; do
												 for mp in "${i%/*}/man" "${i%/*}/share/man"; do
													 if [ -d "$mp" ] && [ "$MANPATH_ADDITIONS" = "${MANPATH_ADDITIONS%${mp}*}" ] ; then
														 MANPATH_ADDITIONS="${MANPATH_ADDITIONS:+$MANPATH_ADDITIONS:}${mp}"
													 fi
												 done
											 done ; echo "$MANPATH_ADDITIONS"))
# echo >&2 "MANPATH_ADDITIONS: [$MANPATH_ADDITIONS]"
export MANPATH_ADDITIONS

PATH_CLEAN=$PATH
MANPATH_CLEAN=$MANPATH

path_clean() {
	export PATH=$PATH_CLEAN
	export MANPATH=$MANPATH_CLEAN
	path_additions() {
		export PATH="${PATH}:${PATH_ADDITIONS}"
		export MANPATH="${MANPATH}:${MANPATH_ADDITIONS}"
		path_additions() {
			echo >&2 "path additions already added..."
		}
	}
}

path_clean

unset PATH0
unset i

export EDITOR=emacsclient
export VISUAL=emacsclient

# GOBIN: The directory where 'go install' will install a command. (To allow
# sharing among different machines, places in HOME.)
[ -n "$GOBIN" ] || export GOBIN="$XDG_DATA_HOME/${os}/${arch}/bin"

# GOCACHE: The directory where the go command will store cached information
# for reuse in future builds. (Highly ephemeral data. Access latency
# prioritized over availability.)
[ -n "$GOCACHE" ] || export GOCACHE="$XDG_CACHE_HOME/${os}/${arch}/go-build"

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
[ -n "$GOTMPDIR" ] || export GOTMPDIR="$XDG_CACHE_HOME/${os}/${arch}/go-tmp"
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
		#
		#     ln -s .config/.profile .profile
		#     ln -s .config/.shrc .bashrc
		[ -e "$HOME/.bashrc" ] || ln -s .config/.shrc .bashrc
		. "$HOME/.bashrc"
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
