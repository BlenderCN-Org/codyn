#!/bin/bash

if [ -z "$1" ]; then
	echo "Please provide the install directory"
	exit 1
fi

installdir="$1"
mkdir -p "$installdir" || exit 1

pkgs="ftp://ftp.gnome.org/pub/gnome/binaries/win32/glib/2.28/glib_2.28.1-1_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/glib/2.28/glib-dev_2.28.1-1_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/libxml2_2.7.7-1_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/libxml2-dev_2.7.7-1_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/libiconv-1.9.1.bin.woe32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/zlib_1.2.4-2_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/zlib-dev_1.2.4-2_win32.zip \
ftp://ftp.gnome.org/pub/gnome/binaries/win32/dependencies/gettext-runtime-dev_0.18.1.1-2_win32.zip \
ftp://ftp.gnome.org/pub/GNOME/binaries/win32/dependencies/gettext-runtime_0.18.1.1-2_win32.zip \
ftp://ftp.gnome.org/pub/GNOME/binaries/win32/intltool/0.40/intltool-dev_0.40.4-1_win32.zip \
ftp://ftp.gnome.org/pub/GNOME/binaries/win32/intltool/0.40/intltool_0.40.4-1_win32.zip"

for i in $pkgs;
do
	wget -O "$installdir/pkg.zip" "$i"
	(cd "$installdir" && unzip pkg.zip && rm pkg.zip)
done
