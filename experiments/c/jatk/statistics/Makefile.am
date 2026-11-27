ACLOCAL_AMFLAGS = -I m4 
SUBDIRS = src

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libjatkstatistics.pc

ctags:
	ctags --recurse=yes --exclude=.git --exclude=BUILD --exclude=.svn --exclude=vendor/* --exclude=node_modules/* --exclude=db/* --exclude=log/* --exclude=mysql-data/*

