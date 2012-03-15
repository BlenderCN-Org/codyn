AC_INIT(codyn, 3.0)

AC_PREREQ(2.62)
AM_INIT_AUTOMAKE([-Wno-portability])

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_MACRO_DIR([m4])

AC_ISC_POSIX
AC_STDC_HEADERS
AC_PROG_CC
AM_PROG_CC_STDC
AC_HEADER_STDC
AM_PROG_LEX
AC_PROG_YACC

GTK_DOC_CHECK(1.9)

LT_PREREQ([2.2.6])
LT_INIT(disable-static)

AM_MAINTAINER_MODE([enable])
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

AC_PROG_INSTALL
AC_PROG_MAKE_SET

PKG_PROG_PKG_CONFIG

AC_PATH_PROG(GLIB_GENMARSHAL, glib-genmarshal)
AC_PATH_PROG(GLIB_MKENUMS, glib-mkenums)

AM_PATH_PYTHON

AC_ARG_ENABLE([webots],
	AS_HELP_STRING([--enable-webots],[Enable building webots bindings (default: disabled)]),
	[enable_webots=$enableval],
	[enable_webots=auto])

# look for webots
if test "x$enable_webots" != "xno"; then
	AC_MSG_CHECKING(for webots)
	dirs="$WEBOTS_HOME_PATH $WEBOTS_HOME /usr/local/webots"

	for i in $dirs;
	do
		if test -r $i/include/controller/c/webots/robot.h;
		then
			AC_MSG_RESULT($i)
			WEBOTS_CFLAGS="-I$i/include/controller/c"
			WEBOTS_LIBS="-L$i/lib -lController"
			
			break;
		fi
	done

	if test "$WEBOTS_CFLAGS" = none; then
		if test "x$enable_webots" = "xauto"; then
			AC_MSG_WARN(disabling webots support)
		else
			AC_MSG_ERROR(could not locate webots)
		fi
	else
		enable_webots=yes
	fi
else
	WEBOTS_CFLAGS=
	WEBOTS_LIBS=
fi

AM_CONDITIONAL([ENABLE_WEBOTS],[test "x$enable_webots" = "xyes"])

AC_SUBST(WEBOTS_CFLAGS)
AC_SUBST(WEBOTS_LIBS)

AC_ARG_ENABLE([doc],
	AS_HELP_STRING([--disable-doc],[Enable building webots bindings (default: enabled)]),
	[enable_doc=$enableval],
	[enable_doc=yes])

if test "x$enable_doc" = "xyes"; then
	AC_PATH_PROG([XSLTPROC], [xsltproc])
fi

AM_CONDITIONAL([ENABLE_DOC],[test "x$enable_doc" = "xyes"])

# Introspection
GOBJECT_INTROSPECTION_CHECK([0.6.10])

if test "$found_introspection" = "yes"; then
	enable_introspection=yes
	AC_DEFINE([ENABLE_INTROSPECTION], [1], [Define to enable GObject Introspection])
else
	enable_introspection=no
fi

dnl ================================================================
dnl Start of pkg-config checks
dnl ================================================================

PKG_CHECK_MODULES(CODYN, [
	glib-2.0
	gobject-2.0
	libxml-2.0
	gio-2.0
	gio-unix-2.0
])

PKG_CHECK_MODULES(GLIB26, [
	glib-2.0 >= 2.26
],[have_networking=yes], [have_networking=no])

AM_CONDITIONAL(ENABLE_NETWORKING, test "x$have_networking" = "xyes")

CODYN_LIBS="${CODYN_LIBS}"

if test "$USE_MAINTAINER_MODE" = "yes"; then
	CODYN_CFLAGS="$CODYN_CFLAGS -Wall -Werror -Wuninitialized -Wmissing-declarations -O2 -g"
fi

AC_SUBST(CODYN_LIBS)
AC_SUBST(CODYN_CFLAGS)

CODYN_API_VERSION=3.0
AC_SUBST(CODYN_API_VERSION)
AC_DEFINE_UNQUOTED([API_VERSION], ["$CODYN_API_VERSION"], [API version])

AC_CHECK_LIB([termcap],
             [tgetent],
              [CDN_PARSER_LIBS="$CDN_PARSER_LIBS -ltermcap"
               CDN_CONTEXT_LIBS="$CDN_CONTEXT_LIBS -ltermcap"
               CDN_ARCHIVE_LIBS="$CDN_ARCHIVE_LIBS -ltermcap"
               AC_DEFINE_UNQUOTED([ENABLE_TERMCAP], [1], [Termcap support])],
             [])

AC_ARG_ENABLE([tar],
              AS_HELP_STRING([--enable-tar=[auto/yes/no]],[Enable tar support (default: auto)]),
              [enable_tar=$enableval],
              [enable_tar=auto])

if test "x$enable_tar" != "xno"; then
	AC_CHECK_LIB([tar],
	             [tar_open],
	             [have_tar=yes
	              CDN_ARCHIVE_LIBS="$CDN_ARCHIVE_LIBS -ltar"],
	             [have_tar=no])
else
	have_tar=no
fi

if test "x$enable_tar" = "xyes" && test "x$have_tar" = "xno"; then
	AC_MSG_ERROR(could not find tar library)
fi

if test "x$have_tar" = "xyes"; then
	AC_CHECK_LIB([bz2],
	             [BZ2_bzCompressInit],
	             [CDN_ARCHIVE_LIBS="$CDN_ARCHIVE_LIBS -lbz2"],
	             [have_tar=no])
fi

AM_CONDITIONAL(ENABLE_ARCHIVE, test "x$have_tar" = "xyes")

AC_CHECK_LIB([clapack],
             [clapack_dgetri],
             [LAPACK_LIBS="-lclapack"],
             [AC_CHECK_LIB([lapack-3],
                           [clapack_dgetri],
                           [LAPACK_LIBS="-llapack-3"],
                           [AC_CHECK_FILE(["/usr/lib64/atlas/libclapack.so"],
                                          [LAPACK_LIBS="-lclapack"
                                           LAPACK_CFLAGS="-L/usr/lib64/atlas"],
                                           [AC_MSG_ERROR(could not find lapack library)])])])

AC_CHECK_LIB([blas],
             [cblas_dgemm],
             [BLAS_LIBS="-lblas"],
             [AC_CHECK_FILE(["/usr/lib64/atlas/libcblas.so"],
                            [BLAS_LIBS="-lcblas"
                             BLAS_CFLAGS="-I/usr/lib64/atlas"],
                            [AC_MSG_ERROR(could not find blas library)])])

AC_SUBST(CDN_PARSER_LIBS)
AC_SUBST(CDN_CONTEXT_LIBS)
AC_SUBST(CDN_ARCHIVE_LIBS)
AC_SUBST(LAPACK_LIBS)
AC_SUBST(LAPACK_CFLAGS)

dnl Check for wii
AC_ARG_ENABLE([wii],
              AS_HELP_STRING([--enable-wii=auto/yes/no],[Enable wii support (default: auto)]),
              [enable_wii=$enableval],
              [enable_wii=auto])

if test "x$enable_wii" != "xno"; then
	PKG_CHECK_MODULES(CDN_IO_WII, [
		bluez,
		cwiid
	], [have_wii=yes], [have_wii=no])

	if test "x$have_wii" = "xyes"; then
		enable_wii="yes"
	else
		if test "x$enable_wii" = "xyes"; then
			AC_MSG_ERROR([Could not find required libraries (cwiid) for wii support])
		fi

		enable_wii="no"
	fi
fi

AC_SUBST(CDN_IO_WII_LIBS)
AC_SUBST(CDN_IO_WII_CFLAGS)

AM_CONDITIONAL(ENABLE_WII, test "x$enable_wii" = "xyes")

dnl ================================================================
dnl Check for gedit
dnl ================================================================

AC_ARG_ENABLE([gedit],
              AS_HELP_STRING([--enable-gedit=[2/3/no]],[Enable installation of gedit plugin (default: 2)]),
              [enable_gedit=$enableval],
              [enable_gedit=2])

if test "x$enable_gedit" != "xno"; then
	GEDIT_MAJOR_VERSION="$enable_gedit"
	enable_gedit="yes"

	if test "x$GEDIT_MAJOR_VERSION" = "x2"; then
		GEDIT_PLUGIN_DIR="$libdir/gedit-2.0/plugins"
	else
		GEDIT_PLUGIN_DIR="$libdir/gedit/plugins"
	fi

	AC_SUBST(GEDIT_PLUGIN_DIR)
	AC_SUBST(GEDIT_MAJOR_VERSION)
fi

AM_CONDITIONAL(ENABLE_GEDIT, test "x$enable_gedit" = "xyes")
AM_CONDITIONAL(ENABLE_GEDIT3, test "x$GEDIT_MAJOR_VERSION" = "x3")

IO_LIBTOOL_FLAGS="-module -avoid-version"
IO_LIBS_DIR="$libdir/codyn-$CODYN_API_VERSION/io"

AC_SUBST(IO_LIBTOOL_FLAGS)
AC_SUBST(IO_LIBS_DIR)

dnl ===========================================================================
dnl Check for python
dnl ===========================================================================
PYGOBJECT3_REQUIRED=3.0.0
PYGOBJECT2_REQUIRED=2.0.0

AC_ARG_ENABLE([python],
              AS_HELP_STRING([--enable-python[=@<:@no/auto/yes@:>@]],[Build with python support]),
              [enable_python=$enableval],
              [enable_python="auto"])

if test "x$enable_python" = "xauto"; then
	PKG_CHECK_EXISTS([pygobject-3.0 >= $PYGOBJECT3_REQUIRED],
	                 [enable_python=yes],
	                 [PKG_CHECK_EXISTS([pygobject-2.0 >= $PYGOBJECT2_REQUIRED],
	                                   [enable_python=yes],
	                                   [enable_python=no])])
fi

if test "x$enable_python" = "xyes"; then
	PKG_CHECK_EXISTS([pygobject-3.0 >= $PYGOBJECT3_REQUIRED],
	                 [PYGOBJECT_REQUIRED=$PYGOBJECT3_REQUIRED
	                  PYGOBJECT_PKG=pygobject-3.0],
	                 [PYGOBJECT_REQUIRED=$PYGOBJECT2_REQUIRED
	                  PYGOBJECT_PKG=pygobject-2.0])

	PKG_CHECK_MODULES(PYTHON,
	                  [$PYGOBJECT_PKG >= $PYGOBJECT_REQUIRED])

	pyoverridesdir=`$PYTHON -c "from distutils.sysconfig import get_python_lib; import os; print(os.path.join(get_python_lib(True, False, '$prefix'), 'gi', 'overrides'))"`

	AC_SUBST(pyoverridesdir)
fi

AM_CONDITIONAL(ENABLE_PYTHON, test x"$enable_python" = "xyes")

AC_CONFIG_FILES([
Makefile
codyn.pc
codyn-webots.pc
codyn/Makefile
codyn/instructions/Makefile
codyn/operators/Makefile
codyn/integrators/Makefile
codyn/tree-algorithms/Makefile
codyn-webots/Makefile
data/Makefile
data/cdn.xml
data/library/Makefile
data/library/layout/Makefile
data/library/oscillators/Makefile
data/library/webots/Makefile
data/library/physics/Makefile
docs/Makefile
docs/reference/Makefile
docs/man/Makefile
io/Makefile
io/file/Makefile
io/network/Makefile
io/wii/Makefile
m4/Makefile
tests/Makefile
win32/Makefile
win32/make-zip
tools/Makefile
tools/cdn-monitor/Makefile
tools/cdn-parser/Makefile
tools/cdn-render/Makefile
tools/cdn-context/Makefile
tools/cdn-archive/Makefile
tools/cdn-compile/Makefile
tools/plugins/Makefile
tools/plugins/gedit/Makefile
tools/plugins/gedit/cdncontext/Makefile
tools/plugins/gedit/cdncontext.plugin
tools/plugins/gedit/cdncontext.gedit-plugin
])

AC_OUTPUT

echo "

Configuration:

	Prefix:                 ${prefix}
	Source code location:	${srcdir}
	Compiler:		${CC}
	Webots support enabled:	$enable_webots
	GObject Introspection:	$enable_introspection
	Documentation enabled:  $enable_doc
	gedit plugin:           $enable_gedit
	cdn-archive:            $have_tar
	network support:        $have_networking
	wii support:            $enable_wii
	python support:         $enable_python
"

