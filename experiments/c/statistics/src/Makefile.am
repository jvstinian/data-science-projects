ACLOCAL_AMFLAGS = -I m4 
AUTOMAKE_OPTIONS = subdir-objects

AM_CFLAGS = -std=c99 -I$(top_srcdir)/vendor

bin_PROGRAMS = statistics
lib_LIBRARIES = libstatistics.a
include_HEADERS = statistics.h
# noinst_LTLIBRARIES = libhcdg.la
# libhcdg_la_SOURCES = hcdg.c
# libhcdg_la_CFLAGS = -std=c89

libstatistics_a_SOURCES = statistics.c
# libstatistics_a_CFLAGS = -std=c89

statistics_SOURCES = statistics.c
# statistics_CFLAGS  = -std=c99
statistics_LDFLAGS = $(top_srcdir)/vendor/mtwister/libmtwister.la

# noinst_HEADERS = statistics.h
