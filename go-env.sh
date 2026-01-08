os=$(uname -s | tr A-Z a-z)
arch=$(uname -m)

[ -n "$XDG_CACHE_HOME" ] || export XDG_CACHE_HOME="$TMPDIR" # Cache: /var/cache
[ -n "$XDG_DATA_HOME" ] || export XDG_DATA_HOME="$HOME/.local/share" # Application Data: /usr/share

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
