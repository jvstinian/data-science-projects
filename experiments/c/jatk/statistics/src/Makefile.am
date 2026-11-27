# ACLOCAL_AMFLAGS = -I m4 
# AUTOMAKE_OPTIONS = subdir-objects

pkgincludedir=$(includedir)/jatk/statistics

# We're not using the LAPACK CFLAGS at this point
AM_CFLAGS = -std=c99 -I$(JATK_TOP_SRCDIR)/vendor @LAPACK_CFLAGS@
# AM_CFLAGS = -std=c99 -I$(abs_top_srcdir)/vendor @LAPACK_CFLAGS@

pkginclude_HEADERS = statistics.h

bin_PROGRAMS = statistics
lib_LIBRARIES = libjatkstatistics.a
# include_HEADERS = statistics.h
# noinst_LTLIBRARIES = libhcdg.la
# libhcdg_la_SOURCES = hcdg.c
# libhcdg_la_CFLAGS = -std=c89

libjatkstatistics_a_SOURCES = statistics.c pareto.c
# libstatistics_a_CFLAGS = -std=c89
# libstatistics_LDFLAGS = @LAPACK_LDFLAGS@

statistics_SOURCES = statistics.c
# statistics_CFLAGS  = -std=c99
# The following isn't working, going to try LDADD and using builddir instead of srcdir
# statistics_LDFLAGS = $(top_srcdir)/vendor/mtwister/libmtwister.la
statistics_LDADD = $(JATK_TOP_BUILDDIR)/vendor/mtwister/libmtwister.la
# Place the helper library (libmtwister.la) in LDADD so Automake ensures the helper library compiles before the application links.
# Use LDFLAGS for linker options (such as -no-undefined or -rpath) and external system library search paths (-L/some/external/path).

# noinst_HEADERS = statistics.h
