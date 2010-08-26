AC_INIT(cpg-network, 1.12)

AM_INIT_AUTOMAKE(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

AC_CONFIG_MACRO_DIR([m4])

AC_ISC_POSIX
AC_STDC_HEADERS
AC_PROG_CC
AM_PROG_CC_STDC
AC_HEADER_STDC
AM_PROG_LIBTOOL

GTK_DOC_CHECK(1.9)

AM_MAINTAINER_MODE
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

AC_PROG_INSTALL
AC_PROG_MAKE_SET

PKG_PROG_PKG_CONFIG

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

dnl ================================================================
dnl Start of pkg-config checks
dnl ================================================================

PKG_CHECK_MODULES(CPG_NETWORK, [
	glib-2.0
	gobject-2.0
	libxml-2.0
	gio-2.0
])

GEDIT_LIBS="${CPG_NETWORK_LIBS}"
AC_SUBST(CPG_NETWORK_LIBS)
AC_SUBST(CPG_NETWORK_CFLAGS)

CPG_NETWORK_API_VERSION=1.0
AC_SUBST(CPG_NETWORK_API_VERSION)

AC_OUTPUT(
Makefile
cpg-network.pc
cpg-network-webots.pc
cpg-network/Makefile
cpg-network-webots/Makefile
data/Makefile
docs/Makefile
docs/reference/Makefile
tests/Makefile
cpg-monitor/Makefile
)
