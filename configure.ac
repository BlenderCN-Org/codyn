AC_INIT(cpg-network, 0.1)

AM_INIT_AUTOMAKE(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

AC_CONFIG_MACRO_DIR([m4])

AC_ISC_POSIX
AC_STDC_HEADERS
AC_PROG_CC
AM_PROG_CC_STDC
AC_HEADER_STDC
AM_PROG_LIBTOOL

AC_PROG_INSTALL
AC_PROG_MAKE_SET

dnl check for pthread support

AC_ARG_ENABLE([threads],
	AS_HELP_STRING([--disable-threads],[Disable thread support (default: enabled)]),
	[enable_threads=$enableval],
	[enable_threads=yes])

if test "x$enable_threads" = "xyes" ; then
	AC_CHECK_LIB(pthread, pthread_mutex_init, THREAD_LIBS="-lpthread", AC_MSG_ERROR(pthread library not found))
	THREAD_CFLAGS=
else
	THREAD_LIBS=
	THREAD_CFLAGS=
fi

AM_CONDITIONAL([ENABLE_THREADS],[test "x$enable_threads" = "xyes"])

AC_SUBST(THREAD_CFLAGS)
AC_SUBST(THREAD_LIBS)

CPG_NETWORK_API_VERSION=0.1
AC_SUBST(CPG_NETWORK_API_VERSION)

AC_OUTPUT(
Makefile
cpg-network.pc
src/Makefile
tests/Makefile
)
