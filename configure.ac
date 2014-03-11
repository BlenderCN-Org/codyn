AC_INIT(codyn, 3.4.4)

AC_PREREQ(2.62)
AM_INIT_AUTOMAKE([1.11 tar-ustar dist-xz no-dist-gzip -Wno-portability])

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_MACRO_DIR([m4])

AC_ARG_ENABLE([debug],
              AS_HELP_STRING([--enable-debug=[yes/no]],[Enable debug build (default: no)]),
              [enable_debug=$enableval],
              [enable_debug=no])

if test "$enable_debug" = "yes"; then
CFLAGS="-O0 -g $CFLAGS"
fi

AC_PROG_CC

AC_ISC_POSIX
AC_STDC_HEADERS

AC_HEADER_STDC
AM_PROG_LEX
AC_PROG_YACC
AC_PATH_PROG([ZIP], [zip])

LT_PREREQ([2.2.6])
LT_INIT(disable-static)

AM_MAINTAINER_MODE([enable])
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

AC_PROG_INSTALL
AC_PROG_MAKE_SET

PKG_PROG_PKG_CONFIG

AC_PATH_PROG(GLIB_GENMARSHAL, glib-genmarshal)
AC_PATH_PROG(GLIB_MKENUMS, glib-mkenums)

dnl check for osx platform
AC_MSG_CHECKING([for Mac OS X platform])

case "$host" in
	*-*-darwin*)
		platform_osx=yes
	;;
	*)
		platform_osx=no
	;;
esac

AC_MSG_RESULT([$platform_osx])
AM_CONDITIONAL(PLATFORM_OSX, test "$platform_osx" = "yes")

if test "$platform_osx" = "yes"; then
	AC_DEFINE([PLATFORM_OSX],[1],[Defined if platform is Mac OSX])
fi

AM_PATH_PYTHON([2.5],,[enable_python=no])

dnl adl_RECURSIVE_EVAL(VALUE, RESULT)
dnl =================================
dnl Interpolate the VALUE in loop until it doesn't change,
dnl and set the result to $RESULT.
dnl WARNING: It's easy to get an infinite loop with some unsane input.
AC_DEFUN([adl_RECURSIVE_EVAL],
[_lcl_receval="$1"
$2=`(test "x$prefix" = xNONE && prefix="$ac_default_prefix"
     test "x$exec_prefix" = xNONE && exec_prefix="${prefix}"
     _lcl_receval_old=''
     while test "[$]_lcl_receval_old" != "[$]_lcl_receval"; do
       _lcl_receval_old="[$]_lcl_receval"
       eval _lcl_receval="\"[$]_lcl_receval\""
     done
     echo "[$]_lcl_receval")`])

# Introspection
GOBJECT_INTROSPECTION_CHECK([1.32])

if test "$found_introspection" = "yes"; then
	enable_introspection=yes
	AC_DEFINE([ENABLE_INTROSPECTION], [1], [Define to enable GObject Introspection])
else
	enable_introspection=no
fi

bison_minor_version=`$YACC --version | head -n 1 | sed -e 's/.*2\.\([[0-9]]*\)/\1/g'`

if test "$bison_minor_version" -ge "5"; then
	BISON_PURE_PARSER="%define api.pure"
else
	BISON_PURE_PARSER="%pure-parser"
fi

AC_SUBST(BISON_PURE_PARSER)

dnl ================================================================
dnl Start of pkg-config checks
dnl ================================================================

checkpkgs="glib-2.0 gobject-2.0 gio-2.0 gmodule-2.0"

if test "$platform_osx" != "yes"; then
	checkpkgs="$checkpkgs libxml-2.0"
fi

PKG_CHECK_MODULES(CODYN, [
	$checkpkgs
])

PKG_CHECK_MODULES(JSON_GLIB, [
	json-glib-1.0
], [have_json_glib=yes], [have_json_glib=no])

AM_CONDITIONAL(ENABLE_CONTEXT, test "x$have_json_glib" = "xyes")

if test "$platform_osx" = "yes"; then
	CODYN_CFLAGS="$CODYN_CFLAGS -I/usr/include/libxml2"
	CODYN_LIBS="$CODYN_LIBS -lxml2"
fi

PKG_CHECK_MODULES(GIO_UNIX, [
	gio-unix-2.0
], [AC_DEFINE_UNQUOTED([ENABLE_GIO_UNIX], [1], [Enable gio unix support])
    have_gio_unix=yes],
   [have_gio_unix=no])

PKG_CHECK_MODULES(GLIB26, [
	glib-2.0 >= 2.26
],[have_networking=yes], [have_networking=no])

AM_CONDITIONAL(ENABLE_NETWORKING, test "x$have_networking" = "xyes")

CODYN_LIBS="${CODYN_LIBS} ${GIO_UNIX_LIBS} ${JSON_GLIB_LIBS}"
CODYN_CFLAGS="${CODYN_CFLAGS} ${GIO_UNIX_CFLAGS} ${JSON_GLIB_CFLAGS}"

if test "$USE_MAINTAINER_MODE" = "yes"; then
	CODYN_CFLAGS="$CODYN_CFLAGS -Wall -Werror -Wmissing-declarations"

	if test "$enable_debug" != "yes"; then
		CODYN_CFLAGS="$CODYN_CFLAGS -Wuninitialized -O2"
	fi
fi

AC_SUBST(CODYN_LIBS)
AC_SUBST(CODYN_CFLAGS)

CODYN_API_VERSION=3.0
AC_SUBST(CODYN_API_VERSION)
AC_DEFINE_UNQUOTED([API_VERSION], ["$CODYN_API_VERSION"], [API version])

AC_CHECK_LIB([termcap],
             [tgetent],
             [AC_CHECK_HEADER([termcap.h],
                              [CDN_PARSER_LIBS="$CDN_PARSER_LIBS -ltermcap"
                               CDN_CONTEXT_LIBS="$CDN_CONTEXT_LIBS -ltermcap"
                               CDN_ARCHIVE_LIBS="$CDN_ARCHIVE_LIBS -ltermcap"
                               AC_DEFINE_UNQUOTED([ENABLE_TERMCAP], [1], [Termcap support])],
                              [])],
             [])

AC_ARG_ENABLE([framework],
              AS_HELP_STRING([--enable-framework=[yes/no]],[Enable build for OS X framework (default: no)]),
              [enable_framework=$enableval],
              [enable_framework=no])

AM_CONDITIONAL(ENABLE_OSX_FRAMEWORK, test "x$enable_framework" = "xyes")

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

adl_RECURSIVE_EVAL($bindir, EXPANDED_BINDIR)
adl_RECURSIVE_EVAL($libdir, EXPANDED_LIBDIR)
adl_RECURSIVE_EVAL($includedir, EXPANDED_INCLUDEDIR)

AC_SUBST(EXPANDED_BINDIR)

AC_ARG_ENABLE([lapack],
	AS_HELP_STRING([--disable-lapack],[Enable lapack support (default: enabled)]),
	[enable_lapack=$enableval],
	[enable_lapack=auto])

have_lapack=no

if test "x$enable_lapack" != "xno" && test "x$platform_osx" = "xyes"; then
	AC_MSG_CHECKING([for lapack])

	AC_MSG_RESULT([using Accelerate framework])

	LAPACK_LIBS="-framework Accelerate"
	LAPACK_CFLAGS=""

	have_lapack=Accelerate
fi

dnl
dnl Check for lapack
dnl
if test "x$enable_lapack" != "xno" && test "x$have_lapack" = "xno"; then
	AC_CHECK_LIB([lapack],
	             [dgelsd_],
	             [LAPACK_LIBS="-llapack"
	              have_lapack=yes],
	             [])
fi

dnl
dnl Check for lapack in separate atlas/ dirs (both lib and include)
dnl
if test "x$enable_lapack" != "xno" && test "x$have_lapack" = "xno"; then
	for dir in "$EXPANDED_LIBDIR" "/usr/lib64" "/usr/lib" "/usr/local/lib"; do
		OLDLDFLAGS="$LDFLAGS"
		LDFLAGS="$LDFLAGS -L$dir/atlas"

		AS_UNSET([ac_cv_lib_lapack_clapack_dgetri])

		AC_CHECK_LIB([lapack],
		             [dgelsd_],
		             [LAPACK_LIBS="-L$dir/atlas -llapack"
		              have_lapack=yes],
		             [])

		LDFLAGS="$OLDLDFLAGS"

		if test "x$have_lapack" = "xyes"; then
			break;
		fi
	done
fi

dnl
dnl Check for lapack_atlas, last resort
dnl
if test "x$enable_lapack" != "xno" && test "x$have_lapack" = "xno"; then
	AC_CHECK_LIB([lapack_atlas],
	             [dgelsd_],
	             [LAPACK_LIBS="-llapack_atlas"
	              have_lapack=yes],
	             [])
fi

dnl
dnl Check to see if we need to pull clapack.h from include/atlas
dnl
if test "x$enable_lapack" != "xno" && test "x$have_lapack" = "xyes"; then
	AC_CHECK_HEADER([clapack.h],
	                [],
	                [for dir in "$EXPANDED_INCLUDEDIR" "/usr/include" "/usr/local/include"; do
	                 OLDCFLAGS="$CFLAGS"
	                 OLDCPPFLAGS="$CPPFLAGS"
	                 CFLAGS="$CFLAGS -I$dir/atlas"
	                 CPPFLAGS="$CPPFLAGS -I$dir/atlas"
	                 AS_UNSET([ac_cv_header_clapack_h])
	                 AC_CHECK_HEADER([clapack.h],
	                                 [LAPACK_CFLAGS="-I$dir/atlas"
	                                  have_lapack=yes],
	                                 [have_lapack=no])
	                 CFLAGS="$OLDCFLAGS"
	                 CPPFLAGS="$OLDCPPFLAGS"

	                 if test "x$have_lapack" = "xyes"; then
	                     break;
	                 fi
	                 done])
fi

if test "x$enable_lapack" = "xyes" && test "x$have_lapack" = "xno"; then
	AC_MSG_ERROR(failed to locate lapack library)
elif test "x$enable_lapack" = "xauto" && test "x$have_lapack" = "xno"; then
	AC_MSG_WARN(could not find lapack library)
fi

if test "$have_lapack" != "no"; then
	AC_DEFINE_UNQUOTED([HAVE_LAPACK], [1], [Have lapack support])
fi

AC_ARG_ENABLE([blas],
	AS_HELP_STRING([--disable-blas],[Enable blas support (default: enabled)]),
	[enable_blas=$enableval],
	[enable_blas=auto])

have_blas=no

if test "x$enable_blas" != "xno" && test "x$have_blas" = "xno"; then
	if test "x$platform_osx" = "xyes"; then
		AC_MSG_CHECKING([for blas])
		AC_MSG_RESULT([using Accelerate framework])

		BLAS_LIBS="-framework Accelerate"
		BLAS_CFLAGS=""

		have_blas=Accelerate
	else
		AC_CHECK_LIB([blas],
		             [cblas_dgemm],
		             [BLAS_LIBS="-lblas"
		              have_blas=yes],
		             [])
	fi
fi

dnl
dnl Check for blas in separate blas
dnl
if test "x$enable_blas" != "xno" && test "x$have_blas" = "xno"; then
	OLDLDFLAGS="$LDFLAGS"
	LDFLAGS="$LDFLAGS -L$EXPANDED_LIBDIR/libblas"

	AS_UNSET([ac_cv_lib_blas_cblas_dgemm])

	AC_CHECK_LIB([blas],
	             [cblas_dgemm],
	             [BLAS_LIBS="-L$EXPANDED_LIBDIR/libblas -lblas"
	              have_blas=yes],
	             [])

	LDFLAGS="$OLDLDFLAGS"
fi

dnl
dnl Check for cblas in separate atlas/ dirs
dnl
if test "x$enable_blas" != "xno" && test "x$have_blas" = "xno"; then
	for dir in "$EXPANDED_LIBDIR" "/usr/lib64" "/usr/lib" "/usr/local/lib"; do
		OLDLDFLAGS="$LDFLAGS"
		LDFLAGS="$LDFLAGS -L$dir/atlas"

		AS_UNSET([ac_cv_lib_cblas_cblas_dgemm])

		AC_CHECK_LIB([cblas],
		             [cblas_dgemm],
		             [BLAS_LIBS="-L$dir/atlas -lcblas"
		              have_blas=yes],
		             [])

		LDFLAGS="$OLDLDFLAGS"

		if test "x$have_blas" = "xyes"; then
			break;
		fi
	done
fi

dnl
dnl Check to see if we need to pull cblas.h from include/atlas
dnl
if test "x$enable_blas" != "xno" && test "x$have_blas" = "xyes"; then
	AC_CHECK_HEADER([cblas.h],
	                [],
	                [for dir in "$EXPANDED_INCLUDEDIR" "/usr/include" "/usr/local/include"; do
	                 OLDCFLAGS="$CFLAGS"
	                 OLDCPPFLAGS="$CPPFLAGS"
	                 CFLAGS="$CFLAGS -I$dir/atlas"
	                 CPPFLAGS="$CPPFLAGS -I$dir/atlas"
	                 AS_UNSET([ac_cv_header_cblas_h])
	                 AC_CHECK_HEADER([cblas.h],
	                                 [BLAS_CFLAGS="-I$dir/atlas"
	                                  have_blas=yes],
	                                 [have_blas=no])
	                 CFLAGS="$OLDCFLAGS"
	                 CPPFLAGS="$OLDCPPFLAGS"

	                 if test "x$have_blas" = "xyes"; then
	                     break;
	                 fi
	                 done])
fi

if test "x$enable_blas" = "xyes" && test "x$have_blas" = "xno"; then
	AC_MSG_ERROR(failed to locate blas library)
fi

if test "x$enable_blas" = "xauto" && test "x$have_blas" = "xno"; then
	AC_MSG_WARN(failed to locate blas library)
fi

if test "$have_blas" != "no"; then
	AC_DEFINE_UNQUOTED([HAVE_BLAS], [1], [Have blas support])
fi

AC_SUBST(CDN_PARSER_LIBS)
AC_SUBST(CDN_CONTEXT_LIBS)
AC_SUBST(CDN_ARCHIVE_LIBS)

AC_SUBST(LAPACK_LIBS)
AC_SUBST(LAPACK_CFLAGS)

AC_SUBST(BLAS_LIBS)
AC_SUBST(BLAS_CFLAGS)

dnl Check for wii
AC_ARG_ENABLE([wii],
              AS_HELP_STRING([--enable-wii=auto/yes/no],[Enable wii support (default: auto)]),
              [enable_wii=$enableval],
              [enable_wii=auto])

if test "x$enable_wii" != "xno"; then
	PKG_CHECK_MODULES(CDN_IO_WII, [
		bluez,
		cwiid,
		glib-2.0 >= 2.26
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

GEDIT_PLUGIN_DIR="$libdir/gedit/plugins"
AC_SUBST(GEDIT_PLUGIN_DIR)

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
blender/Makefile
codyn/Makefile
codyn/cdn-parser.y
codyn/instructions/Makefile
codyn/operators/Makefile
codyn/integrators/Makefile
codyn/tree-algorithms/Makefile
data/Makefile
data/cdn.xml
data/library/Makefile
data/library/algorithms/Makefile
data/library/layout/Makefile
data/library/oscillators/Makefile
data/library/physics/Makefile
data/library/physics/algorithms/Makefile
data/library/physics/lagrange/Makefile
data/library/physics/direct/Makefile
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
tools/cdn-monitor/cdn-monitor-i
tools/cdn-parser/Makefile
tools/cdn-render/Makefile
tools/cdn-context/Makefile
tools/cdn-archive/Makefile
tools/cdn-compile/Makefile
tools/cdn-python/Makefile
tools/plugins/Makefile
tools/plugins/gedit/Makefile
tools/plugins/gedit/cdncontext/Makefile
])

AC_OUTPUT

echo "

Configuration:

	Prefix:                 ${prefix}
	Source code location:	${srcdir}
	Compiler:		${CC}
	GObject Introspection:	$enable_introspection
	cdn-archive:            $have_tar
	cdn-context:            $have_json_glib
	network support:        $have_networking
	wii support:            $enable_wii
	python support:         $enable_python
	blas:                   $have_blas
	lapack:                 $have_lapack
"

