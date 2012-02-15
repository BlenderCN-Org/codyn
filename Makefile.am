SUBDIRS = codyn data tests m4 win32 tools io

if ENABLE_DOC
SUBDIRS += docs
endif

ACLOCAL_AMFLAGS = -I m4

if ENABLE_WEBOTS
SUBDIRS += codyn-webots
endif

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = \
	codyn-@CODYN_API_VERSION@.pc

if ENABLE_WEBOTS
pkgconfig_DATA += codyn-webots-@CODYN_API_VERSION@.pc
endif

codyn-@CODYN_API_VERSION@.pc: codyn.pc
	cp codyn.pc codyn-@CODYN_API_VERSION@.pc

if ENABLE_WEBOTS
codyn-webots-@CODYN_API_VERSION@.pc: codyn-webots.pc
	cp codyn-webots.pc codyn-webots-@CODYN_API_VERSION@.pc
endif

EXTRA_DIST = \
	codyn.pc.in

if ENABLE_WEBOTS
EXTRA_DIST += codyn-webots.pc.in
endif

DISTCHECK_CONFIGURE_FLAGS = --enable-webots

if ENABLE_DOC
DISTCHECK_CONFIGURE_FLAGS += --enable-gtk-doc
endif

DISTCLEANFILES = \
	gnome-doc-utils.make

MAINTAINERCLEANFILES = 		\
	aclocal.m4		\
	config.guess		\
	config.h.in		\
	config.sub		\
	depcomp			\
	gtk-doc.make		\
	install-sh		\
	ltmain.sh		\
	missing			\
	ylwrap			\
	mkinstalldirs		\
	omf.make		\
	py-compile		\
	xmldocs.make		\
	m4/gnome-doc-utils.m4	\
	m4/gtk-doc.m4		\
	m4/intltool.m4		\
	m4/libtool.m4		\
	m4/ltoptions.m4		\
	m4/ltsugar.m4		\
	m4/ltversion.m4		\
	m4/lt~obsolete.m4	\
	`find "$(srcdir)" -type f -name Makefile.in -print`

CLEANFILES = 	\
	$(pkgconfig_DATA)

CHANGELOG_START = v1.0

dist-hook:
	@if test -d "$(srcdir)/.git"; \
	then \
		echo Creating ChangeLog && \
		(GIT_DIR=$(top_srcdir)/.git \
		  ./missing --run git log $(CHANGELOG_START)^^.. --stat -M -C --name-status  --date=short --no-color) | \
		  fmt --split-only > ChangeLog.tmp \
		&& mv -f ChangeLog.tmp $(top_distdir)/ChangeLog \
		|| ( rm -f ChangeLog.tmp ; \
		  echo Failed to generate ChangeLog >&2 ); \
	else \
		echo A git clone is required to generate a ChangeLog >&2; \
	fi

-include $(top_srcdir)/git.mk
