# README
## Security Tokens
If you find yourself somehow magically using a security token, you can
disable it by running

```
git config commit.gpgsign false
```

## Build Commands
Ensure the following dependencies are installed:

(taken from [here][emacs-wsl])

```
sudo apt install -y autoconf automake autotools-dev bsd-mailx build-essential \
    diffstat gnutls-dev imagemagick libasound2-dev libc6-dev libdatrie-dev \
    libdbus-1-dev libgconf2-dev libgif-dev libgnutls28-dev libgpm-dev libgtk2.0-dev \
    libgtk-3-dev libice-dev libjpeg-dev liblockfile-dev liblqr-1-0 libm17n-dev \
    libmagickwand-dev libncurses5-dev libncurses-dev libotf-dev libpng-dev \
    librsvg2-dev libsm-dev libthai-dev libtiff5-dev libtiff-dev libtinfo-dev libtool \
    libx11-dev libxext-dev libxi-dev libxml2-dev libxmu-dev libxmuu-dev libxpm-dev \
    libxrandr-dev libxt-dev libxtst-dev libxv-dev quilt sharutils texinfo xaw3dg \
    xaw3dg-dev xorg-dev xutils-dev zlib1g-dev libjansson-dev libxaw7-dev \
    libselinux1-dev libmagick++-dev libacl1-dev gir1.2-javascriptcoregtk-4.0 \
    gir1.2-webkit2-4.0 libenchant1c2a libglvnd-core-dev libicu-le-hb-dev \
    libidn2-0-dev libjavascriptcoregtk-4.0-dev liboss4-salsa2 libsoup2.4-dev \
    libsystemd-dev libwebkit2gtk-4.0-dev libx11-xcb-dev libxcb-dri2-0-dev \
    libxcb-dri3-dev libxcb-glx0-dev libxcb-present-dev libxshmfence-dev \
    x11proto-composite-dev x11proto-core-dev x11proto-damage-dev \
    x11proto-fixes-dev
```

## `local.el`

We define a `local.el` to store local links

-   Do not expect `local.el` to exist all the time
-   Do not expect `local.el` to define a consistent set of variables
-   Expect `local.el` to vary wildly in content throughout different
    machines
-   Basically don't trust `local.el`. It is a necessary evil


[emacs-wsl]: https://github.com/hubisan/emacs-wsl

## Linux builds
https://emacs.stackexchange.com/questions/59538/compile-emacs-from-feature-native-comp-gccemacs-branch-on-ubuntu

`native-comp` has been merged into master

```
sudo add-apt-repository ppa:ubuntu-toolchain-r/ppa
sudo apt install gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev
git clone git://git.sv.gnu.org/emacs.git gccemacs
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
./configure --with-cairo --with-modules --without-compress-install --with-x-toolkit=no --with-gnutls --without-gconf --without-xwidgets --without-toolkit-scroll-bars --without-xaw3d --without-gsettings --with-mailutils --with-native-compilation --with-json --with-harfbuzz --with-imagemagick --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" prefix=/usr/local
make -j2 NATIVE_FULL_AOT=1
make -j2
make install
```
