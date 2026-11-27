AC_PREREQ([2.62])
AC_INIT([jatk-vendor-mtwister], [1.0.0], [jvstinian@gmail.com])
AC_CONFIG_SRCDIR([mtwister.c])
AM_INIT_AUTOMAKE([-Wall -Werror foreign tar-ustar])

# Checks for programs.
AC_PROG_CC
AC_LANG([C])
AC_PROG_INSTALL
AM_PROG_AR

LT_PREREQ([2.2])
LT_INIT

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

echo \
"-------------------------------------------------

 ${PACKAGE_NAME} Version ${PACKAGE_VERSION}

 Prefix: '${prefix}'.
 Compiler: '${CC} ${CFLAGS}'
 CFLAGS: '${CFLAGS}'
 LDFLAGS: '${LDFLAGS}'
 Libs: '${LIBS}'

 Now type 'make @<:@<target>@:>@'
   where the optional <target> is:
     all                - build all binaries
     install            - install everything

--------------------------------------------------"
