# The following lines were added by Docker Desktop to add commands to your PATH.
export PATH="$PATH:/Users/ben/.docker/bin"
# End of Docker Desktop section.


# --- Homebrew / build toolchain -------------------------------------------
# These MUST live here rather than in ~/.zshenv. /etc/zprofile runs
# `path_helper`, which rebuilds PATH with the system dirs first and demotes
# anything ~/.zshenv prepended (that is why `python3` was resolving to
# /usr/bin/python3 3.9.6 instead of Homebrew's). ~/.zprofile runs *after* that
# reshuffle, so prepends here actually win. Keeping the `brew` forks out of
# ~/.zshenv also stops them running on every single zsh invocation.
if [[ "$OSTYPE" == "darwin"* ]] && [[ -x /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
    export MANPATH="$MANPATH:/opt/homebrew/opt/coreutils/libexec/gnuman"

    OPENSSL_PREFIX=$(brew --prefix openssl 2>/dev/null)
    if [[ -n "$OPENSSL_PREFIX" ]]; then
        export PATH="$OPENSSL_PREFIX/bin:$PATH"
        export LDFLAGS="-L$OPENSSL_PREFIX/lib"
        export CPPFLAGS="-I$OPENSSL_PREFIX/include"
        export PKG_CONFIG_PATH="$OPENSSL_PREFIX/lib/pkgconfig"
    fi

    export PATH="/opt/homebrew/opt/python/libexec/bin:$PATH"
fi
