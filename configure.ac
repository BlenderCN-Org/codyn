AC_INIT(cpg-network, 2.3)

AC_PREREQ(2.62)
AM_INIT_AUTOMAKE([-Wno-portability])

AC_CONFIG_HEADERS(config.h)
AC_CONFIG_MACRO_DIR([m4])

AC_ISC_POSIX
AC_STDC_HEADERS
AC_PROG_CC
AM_PROG_CC_STDC
AC_HEADER_STDC
AM_PROG_LIBTOOL
AM_PROG_LEX
AC_PROG_YACC

GTK_DOC_CHECK(1.9)

AM_MAINTAINER_MODE([enable])
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

AC_PROG_INSTALL
AC_PROG_MAKE_SET

PKG_PROG_PKG_CONFIG

AC_PATH_PROG(GLIB_GENMARSHAL, glib-genmarshal)
AC_PATH_PROG(GLIB_MKENUMS, glib-mkenums)

AC_ARG_ENABLE([webots],
	AS_HELP_STRING([--enable-webots],[Enable building webots bindings (default: disabled)]),
	[enable_webots=$enableval],
	[enable_webots=no])
	
AM_CONDITIONAL([ENABLE_WEBOTS],[test "x$enable_webots" = "xyes"])

# look for webots
if test "x$enable_webots" = "xyes"; then
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
		AC_MSG_ERROR(could not locate webots)
	fi
else
	WEBOTS_CFLAGS=
	WEBOTS_LIBS=
fi

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

PKG_CHECK_MODULES(CPG_NETWORK, [
	glib-2.0
	gobject-2.0
	libxml-2.0
	gio-2.0
	gio-unix-2.0
])

CPG_NETWORK_LIBS="${CPG_NETWORK_LIBS}"

if test "$USE_MAINTAINER_MODE" = "yes"; then
	CPG_NETWORK_CFLAGS="$CPG_NETWORK_CFLAGS -Wall -Werror -Wuninitialized -Wmissing-declarations -O2 -g"
fi

AC_SUBST(CPG_NETWORK_LIBS)
AC_SUBST(CPG_NETWORK_CFLAGS)

CPG_NETWORK_API_VERSION=2.0
AC_SUBST(CPG_NETWORK_API_VERSION)
AC_DEFINE_UNQUOTED([API_VERSION], ["$CPG_NETWORK_API_VERSION"], [API version])

AC_CHECK_LIB([termcap],
             [tgetent],
              [CPG_PARSER_LIBS="$CPG_PARSER_LIBS -ltermcap"
               CPG_CONTEXT_LIBS="$CPG_CONTEXT_LIBS -ltermcap"],
             [AC_MSG_ERROR(could not find termcap library)])

AC_CHECK_LIB([readline],
             [rl_redisplay],
             [CPG_PARSER_LIBS="$CPG_PARSER_LIBS -lreadline"],
             [AC_MSG_ERROR(Could not find readline library)])

AC_SUBST(CPG_PARSER_LIBS)
AC_SUBST(CPG_CONTEXT_LIBS)

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

AC_CONFIG_FILES([
Makefile
cpg-network.pc
cpg-network-webots.pc
cpg-network/Makefile
cpg-network/instructions/Makefile
cpg-network-webots/Makefile
data/Makefile
data/cpg.xml
data/library/Makefile
data/library/layout/Makefile
data/library/oscillators/Makefile
data/library/webots/Makefile
docs/Makefile
docs/reference/Makefile
docs/man/Makefile
m4/Makefile
tests/Makefile
win32/Makefile
win32/make-zip
tools/Makefile
tools/cpg-monitor/Makefile
tools/cpg-parser/Makefile
tools/cpg-render/Makefile
tools/cpg-context/Makefile
tools/plugins/Makefile
tools/plugins/gedit/Makefile
tools/plugins/gedit/cpgcontext/Makefile
tools/plugins/gedit/cpgcontext.plugin
tools/plugins/gedit/cpgcontext.gedit-plugin
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
"

