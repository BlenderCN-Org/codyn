SUBDIRS = codyn data tests m4 win32 tools io blender examples

ACLOCAL_AMFLAGS = -I m4

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = \
	codyn-@CODYN_API_VERSION@.pc

codyn-@CODYN_API_VERSION@.pc: codyn.pc
	cp codyn.pc codyn-@CODYN_API_VERSION@.pc

EXTRA_DIST = \
	codyn.pc.in

DISTCHECK_CONFIGURE_FLAGS =

MAINTAINERCLEANFILES = 		\
	$(GITIGNORE_MAINTAINERCLEANFILES_TOPLEVEL) \
	$(GITIGNORE_MAINTAINERCLEANFILES_MAKEFILE_IN) \
	$(GITIGNORE_MAINTAINERCLEANFILES_M4_LIBTOOL) \
	INSTALL			\
	aclocal.m4		\
	config.guess		\
	config.h.in		\
	config.sub		\
	depcomp			\
	install-sh		\
	ltmain.sh		\
	missing			\
	ylwrap			\
	mkinstalldirs		\
	omf.make		\
	py-compile		\
	m4/intltool.m4		\
	m4/libtool.m4		\
	m4/ltoptions.m4		\
	m4/ltsugar.m4		\
	m4/ltversion.m4		\
	m4/lt~obsolete.m4

CLEANFILES = 	\
	$(pkgconfig_DATA)

CHANGELOG_START = v1.0

dist-hook:
	@if test -d "$(srcdir)/.git"; \
	then \
		echo Creating ChangeLog && \
		(GIT_DIR=$(top_srcdir)/.git \
		  ./missing --run git log $(CHANGELOG_START)^^.. --stat -M -C --name-status  --date=short --no-color) | \
		  fmt > ChangeLog.tmp \
		&& mv -f ChangeLog.tmp $(top_distdir)/ChangeLog \
		|| ( rm -f ChangeLog.tmp ; \
		  echo Failed to generate ChangeLog >&2 ); \
	else \
		echo A git clone is required to generate a ChangeLog >&2; \
	fi

-include $(top_srcdir)/git.mk
