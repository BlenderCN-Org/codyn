AC_INIT(cpg-network, 1.0)

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

AC_PROG_INSTALL
AC_PROG_MAKE_SET

PKG_PROG_PKG_CONFIG

AC_ARG_ENABLE([rt],
	AS_HELP_STRING([--enable-rt],[Enable building real time library (default: disabled)]),
	[enable_rt=$enableval],
	[enable_rt=no])
	
AM_CONDITIONAL([ENABLE_RT],[test "x$enable_rt" = "xyes"])
RT_CFLAGS=

if test "x$enable_rt" = "xyes" ; then
	AC_MSG_CHECKING(for rtlinux header files)
	
	dirs="/usr/rtlinux /usr/src/rtlinux"
	
	for i in dirs; do
		if test -r "$i/include/rtl_printf.h"; then
			AC_MSG_RESULT($i)
  			RT_CFLAGS="-I$i/include"
  			break;
		fi
	done
	
	if test "x$RT_CFLAGS" = "x"; then
		AC_MSG_RESULT(could not locate rtlinux)
	else
		AC_MSG_RESULT(found)
		AC_DEFINE(ENABLE_RT_DEBUG)
	fi
fi

AC_SUBST(RT_CFLAGS)

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
			
			break;
		fi
	done

	if test "$WEBOTS_CFLAGS" = none; then
		AC_MSG_ERROR(could not locate webots)
	fi
else
	WEBOTS_CFLAGS=
fi

AC_SUBST(WEBOTS_CFLAGS)

dnl ================================================================
dnl Start of pkg-config checks
dnl ================================================================

PKG_CHECK_MODULES(CPG_NETWORK, [
	glib-2.0
	gobject-2.0
	libxml-2.0
])
GEDIT_LIBS="${CPG_NETWORK_LIBS}"
AC_SUBST(CPG_NETWORK_LIBS)
AC_SUBST(CPG_NETWORK_CFLAGS)

CPG_NETWORK_API_VERSION=1.0
AC_SUBST(CPG_NETWORK_API_VERSION)

AC_OUTPUT(
Makefile
cpg-network.pc
cpg-network-rt.pc
cpg-network-webots.pc
cpg-network/Makefile
cpg-network-webots/Makefile
docs/Makefile
docs/reference/Makefile
tests/Makefile
)
