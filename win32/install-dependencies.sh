#!/bin/sh

mkdir -p /opt/win/mingw || exit 1

pkgs="ftp://ftp.gnome.org/pub/gnome/binaries/win32/glib/2.28/glib_2.28.1-1_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/glib/2.28/glib-dev_2.28.1-1_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/libxml2_2.7.7-1_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/libxml2-dev_2.7.7-1_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/libiconv-1.9.1.bin.woe32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/zlib_1.2.4-2_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/zlib-dev_1.2.4-2_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/gettext-runtime-dev_0.18.1.1-1_win32.zip"

for i in $pkgs;
do
	wget -O /opt/win/mingw/pkg.zip "$i"
	(cd /opt/win/mingw && unzip pkg.zip && rm pkg.zip)
done
