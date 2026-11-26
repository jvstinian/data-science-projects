ACLOCAL_AMFLAGS = -I m4 

pkgincludedir=$(includedir)/jatk/finance

AM_CFLAGS = -std=c99 -Wall -Wextra

pkginclude_HEADERS = equity.h
bin_PROGRAMS = equity-example
lib_LIBRARIES = libfinance.a

libfinance_a_SOURCES = equity.c
# libstatistics_a_CFLAGS = -std=c89
# libstatistics_LDFLAGS = @LAPACK_LDFLAGS@

equity_example_SOURCES = equity_example.c
# statistics_LDFLAGS = $(top_srcdir)/vendor/mtwister/libmtwister.la
equity_example_LDADD = libfinance.a

# noinst_HEADERS = statistics.h
