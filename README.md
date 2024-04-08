# README

## Windows Pretest

https://alpha.gnu.org/gnu/emacs/pretest/windows/

## Building Windows with Native Compilation
I actually haven't actually tried this so I don't really know whether
this works.

https://gist.github.com/nauhygon/f3b44f51b34e89bc54f8

## Security Tokens
If you find yourself somehow magically using a security token, you can
disable it by running

```
git config commit.gpgsign false
```

## `local.el`

We define a `local.el` to store local links

-   Do not expect `local.el` to exist all the time
-   Do not expect `local.el` to define a consistent set of variables
-   Expect `local.el` to vary wildly in content throughout different
    machines
-   Basically don't trust `local.el`. It is a necessary evil


[emacs-wsl]: https://github.com/hubisan/emacs-wsl

## Linux
Edit `/etc/apt/sources.list` to comment back in `deb-src` lines.
Then `sudo apt update`.

``` shell
sudo apt build-dep emacs
```

``` shell
# depends on which version of gcc is installed
sudo apt install autoconf build-essential texinfo libx11-dev \
     libmagick++-dev                                         \
     libsqlite3-dev                                          \
     libgccjit0 libgccjit-11-dev                             \
     libtree-sitter-dev                                      \
     gnutls-bin                                              \
     libxft-dev                                              \
     libwebp-dev
```

``` shell
./autogen.sh
./configure --with-imagemagick        \
            --with-native-compilation \
            --with-mailutils          \
            --with-pgtk
```

We would normally also use `--with-tree-sitter` if it's `libtree-sitter-dev` is
present.

```
make
sudo make install
```

### WSL2
There's [an issue][wsl-glitch] currently where windows look weird.

Workaround:
- Create a `~/.wslgconfig` with
``` ini
[system-distro-env]
;disable GPU in system-distro
LIBGL_ALWAYS_SOFTWARE=1
```

[wsl-glitch]: https://github.com/microsoft/wslg/issues/1148

## Emacs-Plus
on macos (here the M1 specifically) [issue](https://github.com/d12frosted/homebrew-emacs-plus/issues/485#issuecomment-1230545946)
```
# Optional step
brew uninstall emacs-plus
```
```
LIBRARY_PATH="$(brew --prefix)/lib" brew install --debug -v emacs-plus@30 --with-mailutils --with-imagemagick --with-native-comp --with-xwidgets --with-poll
```

There might be a problem with the install. Verify if running [`make
bootstrap`](https://lists.gnu.org/r/bug-gnu-emacs/2021-01/msg00051.html)
makes sense here.

# Daemon
```sh
arch -arm64 emacs --daemon
emacsclient -a "" -c "SOME FILENAME"
```

# TODO

- [ ] `consult-*` functions are not correctly adding themselves to the evil jump list
