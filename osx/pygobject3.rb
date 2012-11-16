require 'formula'

class Pygobject3 < Formula
  homepage 'http://live.gnome.org/PyGObject'
  url 'http://ftp.gnome.org/pub/GNOME/sources/pygobject/3.2/pygobject-3.2.2.tar.xz'
  sha256 '4653790baaff0176fd814b88cfb5378c45906a120b25d01be2554f423b726eb0'

  depends_on 'pkg-config' => :build
  depends_on 'gobject-introspection'
  depends_on 'glib'
  depends_on 'cairo' => '--without-x'
  depends_on 'py2cairo'

  option :universal

  def install
    ENV.universal_binary if build.universal?

    system "./configure", "--disable-dependency-tracking",
                          "--prefix=#{prefix}"

    system "make install"
  end
end
