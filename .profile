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
umask 027

# https://stackoverflow.com/questions/48574369/setting-term-variable-for-emacs-shell
# case ${INSIDE_EMACS#*,} in
#	comint) echo >&2 "INSIDE_EMACS: comint" ;;
#	tramp) echo >&2 "INSIDE_EMACS: tramp" ;;
#	"") : ;; # echo >&2 "Not INSIDE_EMACS" ;;
#	*) echo >&2 "INSIDE_EMACS: [$INSIDE_EMACS]" ;;
# esac

basename=${0##*/}
LOGNAME=${LOGNAME:-$(id -un)}

arch=$(uname -m)
case $arch in
	amd64|x86_64)  arch=amd64 ;;
	arm64|aarch64) arch=arm64 ;;
	*) echo >&2 "$basename: unrecognized architecture: \"$arch\"" ;;
esac

os=$(uname -s | tr A-Z a-z)
case $os in
	darwin) : ;;
	linux) [ ! -r /etc/os-release ] || os=$(awk -F= '/^(ID|VERSION_ID)=/{gsub(/"/,"",$2);a[$1]=$2}END{split(a["VERSION_ID"],v,".");printf "%s%s\n",a["ID"],v[1]}' /etc/os-release) ;;
	*) echo >&2 "$basename: unrecognized os: \"$os\"" ;;
esac

echo >&2 "arch: [$arch]"
echo >&2 "os: [$os]"
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
chmod 750 "$TMPDIR"
echo "Feel free to erase this directory and all of its contents." > "$TMPDIR/DELETE-THIS-DIRECTORY-IF-TOO-LARGE.txt"

# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
# https://xdgbasedirectoryspecification.com
[ ! -f "$HOME/.config/user-dirs.dirs" ] || . "$HOME/.config/user-dirs.dirs"
[ -n "$XDG_CACHE_HOME" ] || export XDG_CACHE_HOME="$TMPDIR" # Cache: /var/cache
[ -n "$XDG_CONFIG_HOME" ] || export XDG_CONFIG_HOME="$HOME/.config" # Configuration: /etc
[ -n "$XDG_DATA_HOME" ] || export XDG_DATA_HOME="$HOME/.local/share" # Application Data: /usr/share
[ -n "$XDG_STATE_HOME" ] || export XDG_STATE_HOME="$HOME/.local/state" # Application State: /var/lib
[ -n "$PARALLEL_JOBLOG_DIR" ] || export PARALLEL_JOBLOG_DIR="${XDG_STATE_HOME}/parallel" # parallel-* job logs

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
		$XDG_DATA_HOME/${os}/${arch}/bin \
		$XDG_DATA_HOME/${os}/bin \
		$XDG_DATA_HOME/bin \
		/opt/local/sbin \
		/opt/local/libexec/gnubin \
		/opt/local/bin \
		/usr/sbin \
		/usr/bin \
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
PATH_ADDITIONS=$(echo "$PATH0" | awk -v drop="$PATH" '
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

# Durable, shared, mostly OS/arch-neutral dependency downloads.
# 
# CARGO_HOME is not purely a cache: it may contain bin/, config.toml, and
# credentials.toml. This shared setting is appropriate when using
# system-provided cargo/rustc, or when not relying on rustup proxies from
# "$CARGO_HOME/bin".
if : ; then
    # Shared Cargo dependency cache/home. This is good for system-provided
    # cargo/rustc, or when not relying on rustup proxies from "$CARGO_HOME/bin".
    [ -n "$CARGO_HOME" ] || export CARGO_HOME="$XDG_DATA_HOME/cargo"
else
    # If using rustup-managed cargo/rustc from "$CARGO_HOME/bin", prefer instead:
    [ -n "$CARGO_HOME" ] || export CARGO_HOME="$XDG_DATA_HOME/${os}/${arch}/cargo"
fi

# Go module source cache. Mostly OS/arch-neutral.
[ -n "$GOMODCACHE" ] || export GOMODCACHE="$XDG_DATA_HOME/go/pkg/mod"

# Durable, platform-specific installed binaries/toolchains.
#
# cargo install puts binaries in "$CARGO_INSTALL_ROOT/bin".
[ -n "$CARGO_INSTALL_ROOT" ] || export CARGO_INSTALL_ROOT="$XDG_DATA_HOME/${os}/${arch}"

# go install puts binaries directly in "$GOBIN".
[ -n "$GOBIN" ] || export GOBIN="$XDG_DATA_HOME/${os}/${arch}/bin"

# Rustup toolchains are native/platform-specific.
[ -n "$RUSTUP_HOME" ] || export RUSTUP_HOME="$XDG_DATA_HOME/${os}/${arch}/rustup"

# Ephemeral, local, platform-specific build artifacts.
[ -n "$CARGO_TARGET_DIR" ] || export CARGO_TARGET_DIR="$XDG_CACHE_HOME/${os}/${arch}/cargo-target"
[ -n "$GOCACHE" ] || export GOCACHE="$XDG_CACHE_HOME/${os}/${arch}/go-build"
[ -n "$GOTMPDIR" ] || export GOTMPDIR="$XDG_CACHE_HOME/${os}/${arch}/go-tmp"

mkdir -p \
	"$CARGO_HOME" \
	"$CARGO_INSTALL_ROOT/bin" \
	"$CARGO_TARGET_DIR" \
	"$GOBIN" \
	"$GOCACHE" \
	"$GOMODCACHE" \
	"$GOTMPDIR" \
	"$RUSTUP_HOME"

chmod 750 \
	"$CARGO_INSTALL_ROOT/bin" \
	"$CARGO_TARGET_DIR" \
	"$GOBIN" \
	"$GOCACHE" \
	"$GOMODCACHE" \
	"$GOTMPDIR" \
	"$RUSTUP_HOME"

# CARGO_HOME may contain credentials.toml, so keep it private.
chmod 700 "$CARGO_HOME"

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

# ksh and sh use $ENV to find the interactive shell config.  Export it
# unconditionally so that ksh sub-shells inherit it even when the login shell
# is bash or zsh.  ksh ignores ENV for non-interactive shells, so this is safe
# to export regardless of the current shell or mode.
[ ! -r "$HOME/.shrc" ] || export ENV="$HOME/.shrc"

case $_SHELL in
	bash)
		# /bin/bash only invokes $BASH_ENV when non-interactive, while /bin/sh
		# and /bin/ksh only invoke $ENV when interactive. The goal is to
		# enable re-use of ~/.profile and ~/.shrc for both /bin/sh, /bin/bash,
		# /bin/ksh, and potentially other command line shells.  Symlinks are
		# managed by running 'make setup' from ~/.config.
		. "$HOME/.bashrc"
		;;
	ksh|sh)
		: # ENV is already exported above.
		;;
	zsh)
		# Zsh will invoke ~/.zshenv for all shells, then invoke ~/.zprofile
		# for all login shells, then ~/.zshrc for all interactive shells.
		# Symlinks are managed by running 'make setup' from ~/.config.
		:
		;;
	*) echo >&2 "unrecognized shell: '${_SHELL}'" ;;
esac

# No operations follow the above in this file, to ensure behavior on /bin/sh
# is functionally the same as /bin/bash.
